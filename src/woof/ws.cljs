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
          (let [[status msg] (read-transit (.-data event))]
            (on-message status msg)))))


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

