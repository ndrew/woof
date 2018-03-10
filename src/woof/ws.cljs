(ns woof.ws
  (:require
    [cognitect.transit :as transit]
    )
)

(defn read-transit [s]
  (transit/read (transit/reader :json) s))

(defn write-transit [s]
  (transit/write (transit/writer :json) s))



(defn resolve-url [url]
  (let [location (.-location js/window)
        websocket-protocol (if (= (.-protocol location) "https:") "wss:" "ws:")]
    (str websocket-protocol "//" (.-host location) url)))


(defn connect [url & {:keys [on-open on-close on-message]}]
  (let [url    (resolve-url url)
        socket (js/WebSocket. url)]

    (when on-open
      (set! (.-onopen socket)
        (fn [event]
          (on-open))))

    (when on-message
      (set! (.-onmessage socket)
        (fn [event]

          (println "GOT" (.-data event))

          (let [[data msg] (read-transit (.-data event))]

;;            (println "DATA" data)

            (if (= :ping data)
              (.send socket (write-transit :pong))
              (on-message msg))))))


    (when on-close
      (set! (.-onclose socket)
        (fn [event]
          (on-close))))
    socket))
