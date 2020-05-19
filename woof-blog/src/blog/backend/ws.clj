(ns blog.backend.ws
  (:require
    [clojure.core.async :as async :refer [go go-loop]]
    [woof.base :as base]

    [woof.server.transport :as tr]
    [org.httpkit.server :as httpkit]

    [taoensso.timbre :as timbre :refer [info]]
    ))



(defn gen-ws-request-fn [params]

  ;; ws communication
  ;; problem: encapsulate the workflow part

  ;; <?> should ws communication be handled as workflow?

  (let [msg-mult (::main-msg-mult params)
        *upd-state (::*WS-STATE params)

        ;; creates a channel for ws output
        WS-chan (fn [ws-id]

                  ;; <?> should this channel be passed into other api
                  (let [msg-out (base/make-chan (base/&chan-factory params) ws-id)]
                    (async/tap msg-mult msg-out)
                    msg-out))

        ;; closes channel
        WS-chan-close! (fn [ws-id msg-out status]
                         (async/untap msg-mult msg-out)
                         (async/close! msg-out))

        ;; forms a message to be sent via websocket
        WS-msg (fn [ws-id v]
                 ;; v is nil if we need to send initial response
                 (let [msg (if (nil? v)
                             [:state @*upd-state]
                             [:upd v])]
                   (tr/write-transit-str {:ws-id ws-id
                                          :msg msg})
                   )
                 )
        ;; handles incoming messages from websocket
        WS-receive (fn [ws-id msg-out msg]

                     ;; re-route ws messages to workflow loop
                     (let [msg (tr/read-transit-str msg)]

                       (info ::ws-msg msg)

                       #_(let [
                             evt-loop (::EVT-LOOP params) ;; todo: get evt-loop via woof.wfs.evt-loop
                             ]
                           (go
                             (async/put! evt-loop
                                         {(base/rand-sid "msg-") [:ws-msg {:ws-id ws-id
                                                                           :msg msg}]})
                             )
                         )

                       ))
        ]
    (fn [request]
      (if-not (:websocket? request)
        (do {:status 200 :body "websocket connecting..."})
        ;; handle the request
        (httpkit/with-channel request ch
                              (let [ws-id (base/rand-sid "ws-")
                                    msg-out (WS-chan ws-id)]

                                ;; re-route out msg from wf onto a websocket
                                (go-loop []
                                  (when-let [v (async/<! msg-out)]

                                    ;; return nil as msg if we need to force stop
                                    (when-let [msg (WS-msg ws-id v)]
                                      (httpkit/send! ch msg)
                                      (recur))))

                                (httpkit/on-receive ch (fn [msg]
                                                         (WS-receive ws-id msg-out msg)))

                                (httpkit/on-close ch (fn [status]
                                                       (WS-chan-close! ws-id msg-out status)))

                                ;; send initial message if any
                                (if-let [msg (WS-msg ws-id nil)]
                                  (httpkit/send! ch msg))
                                )

                              ))
      )
    )

  )



(defn ws-impl [CHAN-FACTORY]
  (let [;; ws communication
        WS-MSG-CHAN (base/make-chan CHAN-FACTORY (base/rand-sid "curr-ws-"))
        WS-MSG-MULT (async/mult WS-MSG-CHAN)
        *WS-LAST (atom 0)
        *WS-STATE (atom [])]

    {
     :init (fn [params] {;; return latest msg source
                            ::main-msg-mult WS-MSG-MULT
                            ::main-msg-chan WS-MSG-CHAN
                            ::*WS-STATE *WS-STATE})
     :ctx (fn [params]
               {
                :each-10-s {:fn (fn [max-v]
                                  (let [ch (base/make-chan CHAN-FACTORY (base/rand-sid "10s-"))]
                                    (async/thread
                                      (loop []
                                        ;; start
                                        (let [curr-n (swap! *WS-LAST inc)]
                                          (swap! *WS-STATE
                                                 conj
                                                 curr-n)
                                          (async/put! ch curr-n))

                                        (Thread/sleep 10000)
                                        (if (< @*WS-LAST max-v)
                                          (recur))
                                        ))
                                    ch)
                                  )
                            :infinite true}

                :fire-update {:fn (fn [new-v]
                                    (async/put! WS-MSG-CHAN new-v)
                                    new-v)
                              }
                }
               )
     }

    )
  )