(ns woof.server.ws
  (:require
    [clojure.core.async :as async :refer [go go-loop]]
    [woof.base :as base]

    [woof.server.transport :as tr]
    [org.httpkit.server :as httpkit]

    [taoensso.timbre :as timbre :refer [info]]
    [woof.utils :as u]))

;; web sockets communication

;; parameterizable http-kit handler by ws-opts map
;; {
;;  :id-fn     (fn [ws-opts request]) -> unique id for the session
;;  :chan-fn   (fn [ws-id request]) -> chan - channel that will broadcast msg to be send via ws
;;  :in-fn     (fn [ws-id broadcast-chan msg-envelope]) -> incoming messages handler
;;  :out-fn    (fn [ws-id msg]) -> outcoming message handler
;;  :close!-fn (fn [ws-id broadcast-chan status]) -> close channel handler
; }
(defn generic-ws-handler_ [ws-opts request]

  (let [
        ;; provides an a unique id for communication session
        id-fn (get ws-opts :id-fn (fn [ws-opts request] (base/rand-sid)))
        ;;
        broadcast-fn (get ws-opts :chan-fn (fn [ws-id request]
                                        (u/throw! ":chan-fn should be provided")))

        ;; forms a message to be sent via web socket, can return nil to stop communication
        out-fn (get ws-opts :out-fn (fn [ws-id msg]
                                       {
                                        :ws-id ws-id
                                        :msg   msg
                                        }))

        in-fn (get ws-opts :in-fn (fn [ws-id broadcast-chan msg]
                                    (u/throw! ":in-fn should be provided")))

        close-fn! (get ws-opts :close!-fn (fn [ws-id broadcast-chan status]
                                            (u/throw! ":out-fn should be provided")))
        ]
    (if-not (:websocket? request)
      (do {:status 200 :body "websocket connecting..."})
      ;; handle the request
      (httpkit/with-channel
        request <WS-CHAN>
        ;; here ch will be bound to request async channel
        (let [ws-session-id (id-fn ws-opts request)
              wf-broadcast-chan (broadcast-fn ws-session-id request)]

          ;; re-route out msg from wf onto a websocket
          (go-loop []
            (when-let [v (async/<! wf-broadcast-chan)]
              ;; return nil as msg if we need to force stop
              (when-let [msg (out-fn ws-session-id v)]
                (httpkit/send! <WS-CHAN> msg)
                (recur))))

          (httpkit/on-receive <WS-CHAN> (fn [msg] (in-fn ws-session-id wf-broadcast-chan msg)))
          (httpkit/on-close <WS-CHAN> (fn [status] (close-fn! ws-session-id wf-broadcast-chan status)))

          ;; won't it be enough to put msg to a wf-broadcast-chan

          ;; send initial message if any
          (if-let [msg (out-fn ws-session-id nil)]
            (httpkit/send! <WS-CHAN> msg))
          )
        ))
    )
  )



;; specific way of ws communication
;; backend broadcasts messages via chan mult
(defn broadcast-ws-request-fn [params ;; for chan-factory
                               broadcast-mult
                               receive-msg-fn
                               send-msg-fn]

  (partial
    generic-ws-handler_
    {
     :id-fn     (fn [ws-opts request] (base/rand-sid "ws-"))

     :chan-fn   (fn [ws-id request]
                  (let [msg-out (base/make-chan (base/&chan-factory params) ws-id)]
                    (async/tap broadcast-mult msg-out)
                    msg-out))

     :in-fn     (fn [ws-id broadcast-chan msg-envelope]
                  (let [msg (tr/read-transit-str msg-envelope)]
                    ;; re-route ws messages to workflow loop if needed
                    (receive-msg-fn ws-id broadcast-chan msg)))

     :out-fn    (fn [ws-id msg]
                  (if-let [msg-envelope (send-msg-fn ws-id msg)]
                    (tr/write-transit-str msg-envelope)))


     :close!-fn (fn [ws-id broadcast-chan status]
                  (async/untap broadcast-mult broadcast-chan)
                  (async/close! broadcast-chan))
     }
    )
  )
