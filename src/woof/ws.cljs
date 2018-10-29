(ns woof.ws
  (:require

    [cljs.core.async :as async]
    [cognitect.transit :as transit]

    [woof.data :as d]
    [woof.utils :as u]

    )

  (:import
    goog.net.XhrIo)

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]])
)

;; transit stuff

(defn read-transit [s]
  (transit/read (transit/reader :json) s))

(defn write-transit [s]
  (transit/write (transit/writer :json) s))


;; url handling stuff

(defn resolve-url [url]
  (let [location (.-location js/window)]
    (str (.-protocol location) "//" (.-host location) url)))


(defn ws-resolve-url [url]
  (let [location (.-location js/window)
        websocket-protocol (if (= (.-protocol location) "https:") "wss:" "ws:")]
    (str websocket-protocol "//" (.-host location) url)))


;;
;; woof webservice protocol

(defprotocol WoofServer
  (start [this])

  (get-socket [this])

  (send! [this msg])

  (close! [this])
)




(defn connect [url & {:keys [on-open on-close on-message ready-chan]}]
  (let [url    (ws-resolve-url url)
        socket (js/WebSocket. url)]


      (set! (.-onopen socket)
        (fn [event]

          (when ready-chan
            (go
              (async/put! ready-chan socket)))
          (when on-open
            (on-open)

          )))

    (when on-message
      (set! (.-onmessage socket)
        (fn [event]
          (let [msg (read-transit (.-data event))]
            (on-message msg)))))


    (when on-close
      (set! (.-onclose socket)
        (fn [event]
          (on-close))))
    socket))



;; ws constructor
(defn ws-server
  "available options are on-open on-close on-message"
  [url & r]  ;; {:keys [on-open on-close on-message]}
  (let [*socket (volatile! nil)]

    (reify WoofServer
      (start [this]

             (let [ready-chan (async/chan)
                   socket (apply (partial connect url)
                                 (concat r [:ready-chan ready-chan]))] ;; check if the :ready-chan is present

               (vreset! *socket socket)
               ready-chan))

      (get-socket [this] @*socket)

      (send! [this msg]
             (.send @*socket (write-transit msg)))

      (close! [this]
              (if-let [socket @*socket]
                (.close socket))))))




;;
;; processing options for a ws based workflow


(defn ws-opts-impl [*STATE endpoint-url receive-fn close-fn]
  (let [*endpoint (atom nil)
        socket-send         (fn [v]
                                ;(println "sending" v @*endpoint)
                                (send! @*endpoint v)

                              )
        socket-send-transit (fn [v]
                              (send! @*endpoint v) ;; for now send value as is
                              )


        ;; add the helper funcs
        params {
                 :send! socket-send
                 :send-transit! socket-send-transit
                 }

        init-fn (fn[wf-chan xtor]

                  (let [endpoint (ws-server endpoint-url
                                                       :on-message (fn [msg]
                                                                     (receive-fn msg))

                                                       ;; how to init both on-close and socket-send
                                                       :on-close (fn [] ;; arguments
                                                                   (wf/end! xtor)
                                                                   (close-fn :status!))
                                                       )]
                    (reset! *endpoint endpoint)

                    (let [chan (start endpoint)]
                      ;; return the signaling channel, so the wf will wait for the ws to be initialized
                      chan)
                    )
                  )
        ]
    {
      :params params
      :opts {
              :before-process init-fn
              }
      })
  )



(defn ws-opts [endpoint *STATE params]
  (let [{close-fn :close-fn
                                 receive-fn :receive-fn
                                :or {
                                      close-fn (fn [status]
                                                 (u/throw! "no close-fn function provided")
                                                 )
                                      receive-fn (fn [msg]
                                                   (u/throw! "no receive-fn function provided")
                                                   )
                                      }
                                } params


        {ws-params :params
         opts :opts
         } (ws-opts-impl *STATE endpoint receive-fn close-fn)
        ]

    {
      :params (merge ws-params params)
      :opts opts
    }

    )
  ;; pass wf map

  )






;;
;; ajax requests


(defn- ajax!
  [url response-fn callback & [method]]

  (.send goog.net.XhrIo url
         (fn [reply]
           (let [res (response-fn (.getResponseText (.-target reply)))]
             (when callback
               (js/setTimeout #(callback res) 0))))
         (or method "GET")))


(defn ajax-handler [url]
  (let [ch (async/chan 1)
        ajax-ch   (async/chan 1)]
    (go
      (ajax! url identity
          (fn [data]
              (async/put! ajax-ch data)))

      (let [[server-data _] (async/alts! [ajax-ch (async/timeout 30000)])]
        (async/put! ch server-data)))
      ch))



(defn transit-handler [url]
  (let [ch (async/chan 1)
        ajax-ch   (async/chan 1)]
    (go
      (ajax! url read-transit
          (fn [data]
              (async/put! ajax-ch data)))

      (let [[server-data _] (async/alts! [ajax-ch (async/timeout 30000)])]
        (async/put! ch server-data)))
      ch))

