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
    [woof.wfc :as wfc]
    [woof.xform :as x]


    [woof.utils :as u :refer [inline--fn1]]
    [woof.test-data :as test-data]

    [woof.server.utils :refer [read-transit-str write-transit-str]]

    [me.raynes.fs :as fs]
    )
)



(defn prepare-content-map
  "returns the context config for the ws workflow"
  [& {:keys [in-chan> out-chan< send-transit!]}]  ;; context-map

  (println "prepare-content-map" in-chan> out-chan< send-transit!)

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

      :client<  {
                      :fn (x/global-shandler
                            (x/infinite-expand-rf (fn
                                                    ([] in-chan>)
                                                    ([steps]
                                                     (println "SERVER: expand steps (via :client<)\n" (d/pretty steps))
                                                     (if (nil? steps)
                                                       {}
                                                       steps
                                                       )
                                                     )
                                                    ([in-chan out-chan]))))
                      :infinite true
                      :expands? true
                      }


      ;; expand step that adds steps to the current ws
;;      :client<  (receive-steps-handler
;;                  :step-fn (fn [steps]
;;                             (println "SERVER: expand steps (via :client<)\n" (d/pretty steps))
;;                             steps))



      ;; fixme: find way of having several actions wiht :out-fn

      ;; step that sends the value back to client
  ;    :client> (send-value-handler out-chan<
  ;                                    :out-fn (fn [z]
  ;                                              (println "SERVER: response (via :client>)\n" (d/pretty z))
  ;                                              (send-transit! z)
                                                ; (httpkit/send! socket-chan (write-transit-str z))
  ;                                              ))

          :client> {:fn (x/global-shandler
                    (x/channel-collect-rf
                      (fn
                        ([]    out-chan<)
                        ([v]
                           (println "SERVER: response (via :client>)\n" (d/pretty v))
                           (send-transit! v)
                         v)
                        ([a b]))))
              :collect? true
              }


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
  [& {:keys [in-chan> out]}]

  {
    ;; ws
    ::receive-loop [:client< nil]

    ;; send initial cwd to the client
    ::initial-cwd [:v (.getAbsolutePath (fs/file "/Users/ndrw/m/woof"))]

    ::cwd-response [:cwd-response ::initial-cwd]
    ::client-init [:client> ::cwd-response]



    }
  )



(defn wwf [in-chan> out-chan<
            params]

  ;; todo: add state

  (wfc/params-wf (merge params ;; pass the in/out channel in case they'll be needed in context or steps constructors
                        {
                          :in-chan> in-chan>
                          :out-chan< out-chan<
                          }
                        )
                 prepare-content-map
                 prepare-steps))



;; api for webserivce
(defn wf! []
  ;; pass here the def path

  (let [in-chan> (async/chan)
        out-chan< (async/chan)

        receive-fn (fn [payload]
                     (let [msg (read-transit-str payload)]
                       ;; (locking *out*  (println "SERVER RECEIVE:\n" (pr-str msg) "\n"))
                       (go
                         ;; call cmd-> steps inside wf
                         (async/put! in-chan> msg))) ;; (cmd->steps ) ;;
                     )
        close-fn (fn [status] ; :server-close/:client-close/:normal/:going-away/:protocol-error/:unsupported/:unknown

                   ; (locking *out* (println "STOP WS WF:" status))

                   (async/close! in-chan>)
                   (async/close! out-chan<)
                   )
        ]

    {
      :wf (partial wwf in-chan> out-chan<)

      :params {
                :receive-fn receive-fn
                :close-fn close-fn
                }
    }
    )
  )



