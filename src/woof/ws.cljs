(ns woof.ws
  (:require

    [cljs.core.async :as async]
    [cognitect.transit :as transit]

    [woof.data :as d]

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


;; ws



(defprotocol WoofServer
  (start [this])

  (get-socket [this])

  (send! [this msg])

  (close! [this])
  )






(defn connect [url & {:keys [on-open on-close on-message]}]
  (let [url    (ws-resolve-url url)
        socket (js/WebSocket. url)]

    (when on-open
      (set! (.-onopen socket)
        (fn [event]
          (on-open))))

    (when on-message
      (set! (.-onmessage socket)
        (fn [event]

          (let [[status msg] (read-transit (.-data event))]
;            (println "DATA" data)
            ;(println status msg)

            (on-message status msg)

            ))))


    (when on-close
      (set! (.-onclose socket)
        (fn [event]
          (on-close))))
    socket))



;; todo: add kv params for open close
(defn ws-server [url & r]  ;; {:keys [on-open on-close on-message]}
  (let [*socket (volatile! nil)]

    (reify WoofServer
      (start [this]
             (vreset! *socket
                      (apply (partial connect url) r)))

      (get-socket [this] @*socket)

      (send! [this msg]
             (.send @*socket (write-transit msg)))

      (close! [this]
              (if-let [socket @*socket]
                (.close socket)))
      )
    )
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

