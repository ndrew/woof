(ns woof.client.ws
  (:require
    [cljs.core.async :as async]
    [cognitect.transit :as transit]

    [woof.data :as d]
    [woof.utils :as u])

  (:import
    goog.net.XhrIo)

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


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

(defn connect [url & {:keys [on-open on-close on-message ready-chan]}]
  (let [url    (ws-resolve-url url)
        socket (js/WebSocket. url)]

    (.log js/console url)

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


(defn send! [socket msg]
  (.send socket (write-transit msg)))
