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
  )


(defn server-wf-handler[model r]
  (let [[status data] r
        done? (= :done status)]

    (println status)

    (when (= :init status)
      ;; send

      )

    (when (= :error status)
      ;; (swap! *result assoc-in [::wf-status] ::error)
      ;; (swap! *result assoc-in [::result] data)

      )


    (when (= :expand status)
      ;(let [[x-id nu-steps] data]
        ;; todo: check if this breaks done-percentage
      ;  (swap! *result update-in [::steps] merge nu-steps)
      ;  (swap! *result assoc-in [::result] data)
      ;  )

      )

    (when (= :process status)
      ;(swap! *result assoc-in [::result] data)
      )

    (when (= :wf-update status)
      ;(swap! *result assoc-in [::steps] (first data))
      ;(swap! *result assoc-in [::result] (second data))
      )

    (when done?
      ;(swap! *result assoc-in [::wf-status] ::done)
      ;(swap! *result assoc-in [::result] data)
      )

    ;(swap! *result update-in [::history] conj r)

    (not done?))
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




(defn ws-server [url model]
  (let [*socket (volatile! nil)]

    (reify WoofServer
      (start [this]
             (vreset! *socket
                      (connect url
                               :on-open    (fn []
                                             (println "ws open")
                                             ; (.log js/console socket)
                                             )
                               :on-close   (fn [] (println "ws close"))
                               :on-message (fn [msg] (println "got " msg))))
             )

      (get-socket [this] @*socket)
      )
    )
  )





(defn- ajax!
  [url response-fn callback & [method]]

  (.send goog.net.XhrIo url
         (fn [reply]
           (let [res (response-fn (.getResponseText (.-target reply)))]
             (when callback
               (js/setTimeout #(callback res) 0))))
         (or method "GET")))

;;
;; ajax requests

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

