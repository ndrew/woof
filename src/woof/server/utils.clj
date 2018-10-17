(ns woof.server.utils
  "utils for backend"
  (:require
    [org.httpkit.server :as httpkit]
    [cognitect.transit :as t]

    [clojure.core.async :as async :refer [go go-loop]]

    [woof.data :as d]

    [woof.wf :as wf]
    [woof.wfc :as wfc]

    [woof.utils :as u]
    )
)


(defn read-transit-str [s]
  (-> s
      (.getBytes "UTF-8")
      (java.io.ByteArrayInputStream.)
      (t/reader :json)
      (t/read)))



(defn write-transit-str [o]
  (let [os (java.io.ByteArrayOutputStream.)]
    (t/write (t/writer os :json) o)
    (String. (.toByteArray os) "UTF-8")))




;{
;  :before-process init-fn
;  :op-handlers-map {
;                     :done done-fn
;                     :error error-fn
;                     }
;  }

(defn httpkit-opts [socket-chan
                    socket-receive-fn
                    socket-close-fn]
  (let [socket-send         (fn [v] (httpkit/send! socket-chan v))
        socket-send-transit (fn [v] (httpkit/send! socket-chan (write-transit-str v)))
        ;; add the helper funcs
        params {
                 :send! socket-send
                 :send-transit! socket-send-transit
                 }

        init-fn (fn[wf-chan xtor]
                  (httpkit/on-receive socket-chan socket-receive-fn)
                  (httpkit/on-close   socket-chan
                                      (fn [status]
                                        (println "WF ended!")
                                        (wf/end! xtor)
                                        (socket-close-fn status)
                                        )
                                      )
                  :ok
                  )
        ]
    {
      :params params
      :opts {
              :before-process init-fn
              }
      })
  )

