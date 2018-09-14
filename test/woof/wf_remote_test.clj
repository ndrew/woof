(ns woof.wf-remote-test
  (:require
    [clojure.core.async :as async :refer [go go-loop]]
    [clojure.test :refer :all]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.wf-data :as wdata]

    [woof.utils :as u]
    [woof.test-data :as test-data]
))



;; store executors, so the can be stopped

(def *server-wf (atom nil))
(def *client-wf (atom nil))

;; references for in/out channels

(def *server-in (atom nil))
(def *server-out (atom nil))




;; run the wf
(defn run-wf [prefix done-chan ctx-map steps]
  (let [xtor (wf/build-executor (wf/make-context ctx-map) steps)]
    (let [exec-chann (wf/execute! xtor)]

      (go-loop [] ; can handle state via loop bindings
               (let [r (async/<! exec-chann)
                     [status data] r]

                 (condp = status
                   :init (recur)
                   :error (do
                            (async/>! done-chan [:error data] )
                            (locking *out* (println prefix [:error data])))

                   :process (do
                              (recur))
                   :done (do
                           (async/>! done-chan [:done data])
                           ;(locking *out* (println prefix [:done data]))
                           )

                   (do ; other events like :wf-update
                     (recur)))))

      xtor
  )
)
)




;; shared context between the client/server
(def context-base
  {
    ;; identity functio
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

    ;; just for testing infinite workflows
    :tick {:fn (fn [t]
                 (let [chan (async/chan)]
                   (go-loop []
                            (async/<! (u/timeout t))
                            (async/>! chan (System/currentTimeMillis))
                            (recur))
                   chan))
           :infinite true
           }


    })



;; CLIENT: send some steps to server

(defn client-send! [server> v]  ;; todo add callback
  (let [v-sid (wf/rand-sid)
        v-ref (wf/rand-sid "&")
        v*    (wf/rand-sid "*")
        zip   (wf/rand-sid "zip-")

        out   (wf/rand-sid ">")

        server-steps {
                       ;; add a value (optional if edn)
                       v-sid [:v v]
                       v-ref [:&v v-sid] ;; get a reference to it
                       v*    [:v* [v-sid]] ;; get a value list
                       zip   [:zip [v-ref v*]] ;; interleave them

                       out   [:client> zip]
                       }
        ]
    ;sends steps to server
    (go
      (async/>! server> server-steps))

    {(wf/rand-sid "out-") [:&v server-steps]}
    ))


;; SERVER: wait for commands from client and add start executing them

(defn server-receive! [in>]
  (let [chan> (async/chan)]
    (go-loop []
             (let [v (async/<! in>)]

               (if-not (map? v)
                 (u/throw! (str "invalid expand map passed to :in " (d/pretty v))))

               (locking *out* (println "server: IN" (d/pretty v)))

               #_(async/put! chan> (with-meta
                                     {sid v}
                                     {:expand-key sid}))
               (async/put! chan> v)
               )
             (recur))
    chan>))



;; SERVER:

(defn server-send! [server< v] ;; todo: make out generic

  ;; currently sends only id list
  #_(let [kvs (partition 2 v)] ;; id v list
      (locking *out* (println "Server: OUT: " (d/pretty kvs)))
      (go
        (async/>! server< v))
      )

  (locking *out* (println "Server: OUT: " (d/pretty v)))
  v
  )



;;



;; CLIENT: receive from server

(defn client-receive! [in>] ;; add callback
  (let [chan> (async/chan)]
    (go-loop []
             (let [v (async/<! in>)]
               (locking *out* (println "client: IN" (d/pretty v)))
               (async/put! chan> {
                                   (wf/rand-sid) [:log v]}))
             (recur))
    chan>))







#_(do

  (reset! *server-in (async/chan))
  (reset! *server-out (async/chan))

;  (reset! *client-in (async/chan))
;  (reset! *client-out (async/chan))


  ;; clj
  (async/thread

    (locking *out* (println "SERVER: thread started"))

    (let [thread-blocker (async/chan)
          server-context-map
          {
            ;; step handler that waits for commands from in> channel
            :client< {
                  :fn server-receive!
                  :infinite true
                  :expands? true
                       }

            :client> {:fn (partial server-send! @*server-out)
                  :collect? true
                  }

            }
          server-steps {
                         ::loop [:client< @*server-in]}]

      (do
        (reset! *server-wf
                (run-wf "SERVER:"
                        thread-blocker
                        (merge context-base server-context-map)
                        server-steps)

                )
        (let [[op data] (async/<!! thread-blocker)]
          (locking *out* (println "SERVER: thread stopped\n"
                                  (d/pretty op) "\n"
                                  (d/pretty (wdata/inline-results data))))))))

  ;; cljs

  (async/thread

    (locking *out* (println "CLIENT: thread started"))
    (Thread/sleep 500)

    (let [thread-blocker (async/chan)
          client-context-map {
            :server< {
                       :fn client-receive!
                       :infinite true
                       :expands? true
                       }
            :server> {:fn (partial client-send! @*server-in)
                      :expands? true
                      }}

          client-steps {
            ::loop [:server< @*server-out]

            ::tick [:tick 3000]
            ::payload [:server> ::tick]
            }
          ]
      (do
        (reset! *client-wf
                (run-wf "CLIENT:"
                        thread-blocker
                        (merge context-base client-context-map)
                        client-steps))

        (let [[op data] (async/<!! thread-blocker)]
          (locking *out* (println "CLIENT: thread stopped\n"
                                  (d/pretty op) "\n"
                                  (d/pretty (wdata/inline-results data)))))))


    )


  )



; run to stop
(do
  (wf/end! @*server-wf)
  (wf/end! @*client-wf)
  )
