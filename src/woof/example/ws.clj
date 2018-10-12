(ns woof.example.ws
  "example of the workflow connected to a websocket"
  (:require
    [clojure.core.async :as async :refer [go go-loop]]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.wfc :as wfc]
    [woof.utils :as u :refer [inline--fn1]]

    [woof.xform :as x]

    [woof.server.utils :refer [read-transit-str write-transit-str]]
    )
)






;; context map



(defn context-fn [& {:keys [send-transit! in-chan> out-chan<]}]
  ;; todo: migrate :client< to be a xform global-shandler


  {

    :v  {:fn (fn[a] (identity a))}                        ;; identity function
    :log  {:fn (fn[a]                                     ;; debug
                 (locking *out* (println "DBG:" a))
                 (identity a))}
    :&v {:fn (fn[a]                                       ;; reference
               {(wf/rand-sid) [:v a]})
         :expands? true}
    :v* {:fn identity :collect? true}                     ;; collect

    :zip {:fn (fn [vs]                                    ;; zipper
                (partition (count vs) (apply interleave vs)))
          :collect? true}

;    :debug (wf/step-handler (fn[x]                        ;; older steps
;                              (println "DBG: " (d/pretty x))
;                              x))

    :server-time (wf/step-handler (fn[x]
                                    [:server-time (u/now)]))

    ;; debug step handler that returns debug info to client
    :debug {:fn (fn[x]
                  (send-transit! [:debug x])
                  x)}

    ;; loop that waits for steps from the in> channel
    :client<  {
                :fn (x/global-shandler
                      (x/infinite-expand-rf (fn
                                              ([] in-chan>) ;; return channel

                                              ([steps]


                                               (locking *out* (println "WF RECEIVE: " (d/pretty steps)))
                                               #_(cond
                                                 (nil? steps) nil
                                                 (map? steps) steps
                                                 ;:else (u/throw! (str "invalid expand map passed to :in " (d/pretty steps)))
                                                 )
                                               steps
                                               )

                                            ([in-chan out-chan]

                                             (locking *out* (println "closing :client< handler" in-chan out-chan))
                                             ;; when this is called

                                             )
                                            )))
                :infinite true
                :expands? true
                }


    :client> {:fn (x/global-shandler
                    (x/channel-collect-rf
                      (fn
                        ([]
                         out-chan<)
                        ([v]
                         (locking *out* [:to-client v])

                         (send-transit! [:to-client v])
                         v)
                        ([a b]
                         ;(locking *out* (println "end!" a b))
                         )
                        )
                      ))
              :collect? true
              }



    }
  )




(defn steps-fn [& {:keys []}]
  {
    ;; ::hello [:debug "woof!"]

    ::receive-loop [:client< {::hello [:debug "woof!"]}]

    ;; ::zzz [:client> "AAA"]
    })



;; workflow constructor

(defn wwf [in-chan> out-chan<
            params]

    (locking *out* (println "\n\n\n" "START WS WF:\n" (d/pretty params) "\n\n\n"))


  (wfc/params-wf (merge params ;; pass the in/out channel in case they'll be needed in context or steps constructors
                        {
                          :in-chan> in-chan>
                          :out-chan< out-chan<
                          }
                        )
                 context-fn
                 steps-fn))

;; how to call these?



(defn wf-opts []
    (let [in-chan> (async/chan)
        out-chan< (async/chan)

        receive-fn (fn [payload]
                     (let [msg (read-transit-str payload)]
                       (locking *out*  (println "SERVER RECEIVE:\n" (pr-str msg) "\n"))
                       (go
                         (async/put! in-chan> msg)))
                     )
        close-fn (fn [status]
                   ; :server-close   : Channel closed by sever
                   ; :client-close   : HTTP channel closed by client
                   ; :normal         : WebSocket closed by client (CLOSE_NORMAL)
                   ; :going-away     : WebSocket closed by client (CLOSE_GOING_AWAY)
                   ; :protocol-error : WebSocket closed by client (CLOSE_PROTOCOL_ERROR)
                   ; :unsupported    : WebSocket closed by client (CLOSE_UNSUPPORTED)
                   ; :unknown        : WebSocket closed by client (unknown reason)

                   (locking *out* (println "STOP WS WF:"))

                   (async/close! in-chan>)
                   (async/close! out-chan<)
                   )
        ]

    ;(endpoint-fn (partial wwf in-chan> out-chan<) receive-fn close-fn)
    {
      :wf (partial wwf in-chan> out-chan<)
      :receive-fn receive-fn
      :close-fn close-fn
      }
    )
  )




(defn prepare-wf []
  (let [in-chan> (async/chan)
        out-chan< (async/chan)

        receive-fn (fn [payload]
                     (let [msg (read-transit-str payload)]
                       (locking *out*  (println "SERVER RECEIVE:\n" (pr-str msg) "\n"))
                       (go
                         (async/put! in-chan> msg)))
                     )
        close-fn (fn [status]
                   ; :server-close   : Channel closed by sever
                   ; :client-close   : HTTP channel closed by client
                   ; :normal         : WebSocket closed by client (CLOSE_NORMAL)
                   ; :going-away     : WebSocket closed by client (CLOSE_GOING_AWAY)
                   ; :protocol-error : WebSocket closed by client (CLOSE_PROTOCOL_ERROR)
                   ; :unsupported    : WebSocket closed by client (CLOSE_UNSUPPORTED)
                   ; :unknown        : WebSocket closed by client (unknown reason)

                   (locking *out* (println "STOP WS WF:"))

                   (async/close! in-chan>)
                   (async/close! out-chan<)
                   )
        ]

    ;(endpoint-fn (partial wwf in-chan> out-chan<) receive-fn close-fn)
    {
      :wf (partial wwf in-chan> out-chan<)
      :receive-fn receive-fn
      :close-fn close-fn
    }
    )
  )





(defn wf-tester
  "assigns workflow to httpkit websocket route"
  [in-chan
   out-chan
   wwf-fn
   socket-receive-fn
   socket-close-fn & [opts]]


  (let [socket-send (fn [v]
                      ;; (httpkit/send! socket-chan v)
                      ;(println "SOCKET SEND: " v)
                      (go
                        (async/put! out-chan v))
                      )
        socket-send-transit (fn [v]
                              ;; (httpkit/send! socket-chan (write-transit-str v))
                              (go
                                (async/put! out-chan (write-transit-str v)))
                              )
        ;; add the helper funcs
        params {
                 :send! socket-send
                 :send-transit! socket-send-transit
                 }


        init-fn (fn[wf-chan xtor]
                  (go-loop []
                           (if-let [payload (async/<! in-chan)]
                             (do
                               (socket-receive-fn payload)
                               (recur)
                               )
                             (do
                               (locking *out* (println "WF END !!!"))

                               (wf/end! xtor)
                               (socket-close-fn :normal)
                               (async/close! in-chan)
                               (async/close! out-chan)
                               )
                             )))


        ws-opts (if (nil? opts) {} opts)

        done-fn  (get ws-opts :done (fn [data]
                                      (locking *out* (println "DONE!\n" (d/pretty data)))
                                      ))

        error-fn (get ws-opts :error (fn [data]
                                       (locking *out* (println "ERROR!\n" (d/pretty data)))))

        ]

    ;; todo: use the wf result processing options and merge them
    (wfc/wf-async-process! (wwf-fn params)
                           {
                             :before-process init-fn
                             :op-handlers-map {
                                                :done done-fn
                                                :error error-fn
                                                }
                             })
    )
  )





;; tester

#_(let [in (async/chan)
      out (async/chan)]


  (let [end-chan (async/chan)

        ;; opts
        init-fn (fn [wf-chan xtor]

                  (go
                    (when-let [v (async/<! end-chan)]
                      (locking *out* (println "stopping wf"))
                      (wf/end! xtor)

                      )
                    )

                  ;; simulate receiving from socket
                  (go-loop []
                           (if-let [v (async/<! out)]
                             (do
                               (locking *out* (println "RECEIVED" v))
                               (recur))
                             (do
                               (locking *out* (println "STOPPED WF"))

                               (async/close! in)
                               (async/close! out)
                               )
                             )))

        done-fn  (fn [data]
                   (locking *out* (println "DONE!\n" (d/pretty data))))

        error-fn (fn [data]
                    (locking *out* (println "ERROR!\n" (d/pretty data))))

        opts {
               :before-process init-fn
               :op-handlers-map {
                                  :done done-fn
                                  :error error-fn
                                  }
               }

        ; wf constructor

        wwf (partial wwf in out)

        ; wf params

        ; simulate putting data on the wire
        socket-send (fn [v]
                      (go
                        (async/put! out v)))

        socket-send-transit (fn [v]
                              (go
                                (async/put! out v
                                            ; (write-transit-str v)
                                            )))

        params {
                 :send! socket-send
                 :send-transit! socket-send-transit
                 }

        ]

  (wfc/wf-async-process! (wwf params) opts)


  ;; frontend sends new steps into workflow
  (go
    (async/<! (u/timeout 200))
      (async/put! in {
                       ::yo [:debug "Hello!"]
                       ::client [:client> "Yo"]
                       }))


  ;; simulate closing ws
  (go
    (async/<! (u/timeout 1200))
    (async/put! end-chan :done)
    )

  )
)
