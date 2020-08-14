(ns woof.client.ws
  (:require
    [cljs.core.async :as async]
    [cognitect.transit :as transit]

    [woof.data :as d]
    [woof.utils :as u]
    [woof.base :as base]
    [goog.net.XhrIo :as xhrio]
    )

  (:import
    goog.net.XhrIo)

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; WEB-SOCKET COMMUNICATION

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
  (if (clojure.string/starts-with? url "ws")
    url
    (let [location (.-location js/window)
          websocket-protocol (if (= (.-protocol location) "https:") "wss:" "ws:")]
      (str websocket-protocol "//" (.-host location) url))
    )
  )

;; ws


(defn connect [url & {:keys [on-init
                             on-open
                             on-close
                             on-message
                             ] :or {on-init (fn [socket] socket)}}]

  (let [url    (ws-resolve-url url)
        socket (js/WebSocket. url)]

    (set! (.-onopen socket)
          (fn [event]
            (when on-open
              (on-open))))

    (when on-message
      (set! (.-onmessage socket)
            (fn [event]
              (on-message (.-data event)))))

    (when on-close
      (set! (.-onclose socket)
            (fn [event]
              (on-close))))

    (on-init socket)
    ))



(defn chan-connect
  "connect ws to url, return channel with socket after it's open"
  [url & {:keys [chan on-init on-open] :as cfg}]
  (let [ch (if chan chan (async/chan))
        *socket (volatile! nil)

        args (flatten (into [url] (merge cfg
                                         {:on-init (fn [socket]
                                                    (vreset! *socket socket)
                                                    (if on-init
                                                        (on-init socket))
                                                    ch)
                                         :on-open (fn []
                                                    (if on-open
                                                        (on-open))
                                                    (async/put! ch @*socket))}
                                         )))
        ]

    (apply connect args)
    ))



(defn send! [socket msg]
  (.send socket msg))

(defn send-transit! [socket msg]
  (.send socket (write-transit msg)))


(defn GET [url handler]
  (xhrio/send url (fn [event]
                    (let [response (.-target event)]
                      (handler (.getResponseText response)))
                    ))
  )



;; todo: handling multiple ws-ctx in the same wf

;; :ws/chan-fn          (fn [] ...)     => channel
;; :ws/gen-msg-handler  (fn [] ... )    => (fn [msg-envelope] ...)
;; :ws/msg-handler      (fn [msg] ... ) => ...
(defn ws-ctx-fn [params]
  (let [ws-chan (get params
                     :ws/chan-fn
                     (fn []
                       (.warn js/console "no :ws/chan-fn provided. creating channel with rand-sid")
                       (base/make-chan (base/&chan-factory params) (base/rand-sid "ws-"))))


        ;; ;; todo: :ws/gen-msg-handler can be null
        gen-msg-handler (get params :ws/gen-msg-handler)

        msg-handler (get params :ws/msg-handler
                         (fn [msg]
                           ;; todo: handle case when no :ws/msg-handler is passed
                           ))

        ]
    {
     :ws-socket    {
                      :fn (fn [url]
                            (let [ch (ws-chan)
                                  ws-msg-handler (if gen-msg-handler (gen-msg-handler)
                                                                      msg-handler)

                                  ]
                                 ;; returns socket
                                 (chan-connect url
                                               :chan ch

                                               ;; :on-init :on-exit
                                               :on-message (fn [payload]
                                                                (let [msg (read-transit payload)]
                                                                     (ws-msg-handler msg)))
                                                  )
                                 )
                            )
                      }

     ;;
     :ws-close! {:fn (fn [socket]
                       (.close socket)

                       (u/now)
                       )
                 :collect? true}

     :ws-send! {:fn (fn [[socket msg]]
                      (if (or (= :nil msg) (nil? msg))
                        (.log js/console "not sending an empty msg")
                        (send-transit! socket msg)
                        )
                      (u/now)
                      )
                :collect? true}

     }
    )
  )


;; parametrizible ws init function
(defn _ws-init-fn [process-ws-msg params]
  (let [chan-factory (base/&chan-factory params)]
    {
     :ws/chan-fn (fn []
                   (let [ws-chan (base/make-chan chan-factory
                                                 (base/rand-sid "ws-"))]
                     ws-chan))

     ;; disallow to use gen handler
     ;:ws/gen-msg-handler (fn []
     ;                      (u/throw! "scraping wf needs to specify it's own :ws/gen-msg-handler")
     ;                      )
     :ws/gen-msg-handler (fn []
                           (fn [msg-envelope]
                             (.log js/console (d/pretty! msg-envelope))

                             (try
                               (process-ws-msg params msg-envelope)
                               (catch js/Error e
                                 (.error js/console ":ws/gen-msg-handler error:" e)
                                 )
                               )
                             )
                           )

     ;; what is a good way of sending message to socket
     ;; via separate channel
     ;; or via socket directly

     ;                  :ws/msg-handler (fn [msg]
     ;                                    (.log js/console "[WS]" msg))

     }
    )
  )
