(ns woof.example.files
  "woof client/server communication endpoint."
  (:require
    [compojure.core :as compojure]
    [compojure.route :as route]
    [org.httpkit.server :as httpkit]
    [ring.util.response :as response]
    [cognitect.transit :as t]

    [clojure.core.async :as async :refer [go go-loop]]
    [clojure.test :refer :all]

    [woof.data :as d]
    [woof.wf :as wf]

    [woof.utils :as u :refer [inline--fn1]]
    [woof.test-data :as test-data]

    [woof.server.utils :refer [read-transit-str write-transit-str]]

    [me.raynes.fs :as fs]
    )
)


;;
;;

(defn receive-steps-handler
  "creates a step handler config [:your-handler-name <in-channel>] that will wait for the steps from <in-channel>.
  Parametrizeble by optional :step-fn param "
  [& {:keys [step-fn]
      :or {step-fn identity}}]
  {
    :fn (fn [in>]
          (let [chan> (async/chan)]
            (go-loop []
                     (when-let [new-steps (async/<! in>)]

                       (if-not (map? new-steps)
                         (u/throw! (str "invalid expand map passed to :in " (d/pretty new-steps))))

                       ; (locking *out* (println "server: IN" (d/pretty new-steps)))

                       (async/put! chan> (step-fn new-steps))
                       ;; todo: use :expand-key instead of having intermediary steps
                       ;; #_(async/put! chan> (with-meta {sid v} {:expand-key sid}))
                       (recur)
                       )
                     )
            chan>))
    :infinite true
    :expands? true
    }
  )




;; stateful step handlers


;; step handler can be described as 'reducing function'



;; params for initialization

;; out-chan




;; (step-handler start-fn stop-fn)



;; some initialization after



;; fixme: refine the api for 'send-value' handlers
;;

;; fixme: wrong abstraction remove from here

(defn send-value-handler
  "parametrizable function that generate send value handler"
  [out-chan<
   & {:keys [v-fn out-fn return-fn]
      :or {v-fn identity
           out-fn identity
           return-fn (fn[v vs] vs)
           }} ]


  ;; (sh)
  (go-loop []
           (if-let [v (async/<! out-chan<)] ;; redirect the wf output onto wire
             (do
               (inline--fn1 out-fn v)
               (recur))
             )
           )


  {:fn (fn [v]
         ;; transform v into other value or steps
         (let [vs (v-fn v)]
           (go ;; send the new value to out-chan<
             (async/>! out-chan< vs))
           ;; return a result
           (return-fn v vs)))
   :collect? true
   })





(defn prepare-content-map
  "returns the context config for the ws workflow"
  [socket-chan in out-chan<]  ;; context-map

  (merge
    {  ;; todo: refine the generic actions needed for RPC

      ;; identity function
      :v  {:fn (fn[a] (identity a))}

      ;; debug
      :log  {:fn (fn[a]
                   (locking *out* (println "DBG:" a))
                   (identity a))}

      ;; reference
      :&v {:fn (fn[a]
                 {(wf/rand-sid) [:v a]})
           :expands? true}

      ;; collect
      :v* {:fn identity :collect? true}

      ;; zipper
      :zip {:fn (fn [vs]
                  (partition (count vs) (apply interleave vs)))
            :collect? true}


      ;; older steps
      :debug (wf/step-handler (fn[x]
                                (println "DBG: " (d/pretty x))
                                x))

      }

    {

      ;; expand step that adds steps to the current ws
      :client<  (receive-steps-handler
                  :step-fn (fn [steps]
                             (println "SERVER: expand steps (via :client<)\n" (d/pretty steps))
                             steps))



      ;; fixme: find way of having several actions wiht :out-fn

      ;; step that sends the value back to client
      :client> (send-value-handler out-chan<
                                      :out-fn (fn [z]
                                                (println "SERVER: response (via :client>)\n" (d/pretty z))
                                                (httpkit/send! socket-chan (write-transit-str z))
                                                ))

      ;; 'cd'
      :cd {:fn (fn [v]
                 (let [file (fs/normalized (apply fs/file v))]
                   (println "SERVER: `cd " (.getAbsolutePath file) "`")
                   (.getAbsolutePath file))

                 )}

      ;; 'ls'
      :dir {:fn (fn[f]
                  (println "SERVER: `dir " f "`")
                  (let [base (fs/normalized (fs/file f))]
                    (map (fn [f]
                           (str
                             (if (fs/directory? f) "/" "")
                             (fs/base-name f))

                           ) (fs/list-dir base))))

            }

      :cwd-response {:fn (fn[x] [:cwd x])}
      :dir-response {:fn (fn[x] [:dir x])}

}))


(defn prepare-steps
  "returns initial steps for the ws workflow"
  [in-chan> out]

  {
    ;; ws
    ::receive-loop [:client< in-chan>]

    ;; send initial cwd to the client
    ::initial-cwd [:v (.getAbsolutePath (fs/file "/Users/ndrw/m/woof"))]

    ::cwd-response [:cwd-response ::initial-cwd]
    ::client-init [:client> ::cwd-response]



    }
  )





