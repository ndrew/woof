(ns woof.server
  (:require
    [compojure.core :as compojure]
    [compojure.route :as route]
    [org.httpkit.server :as httpkit]
    [ring.util.response :as response]
    [cognitect.transit :as t]

    [clojure.core.async :as async :refer [go go-loop]]
    [clojure.test :refer :all]

    [woof.data :as d]
    [woof.wf :as wf]
   ; [woof.wf-data :as wdata]

    [woof.utils :as u]
    [woof.test-data :as test-data]

    )
  (:gen-class))


(def *ws-wf (atom nil))

(def *ws-context
  (atom
    {

      :debug (wf/step-handler (fn[x]
                                (println "DBG: " (d/pretty x))
                                x))

      :server-time (wf/step-handler (fn[x]
                                [:server-time (u/now)]))


      ;; step handler that waits for commands from in> channel
      :in {
          :fn (fn[in>]


                (let [chan> (async/chan)]
                  (go-loop []
                           (let [v (async/<! in>)
                                 sid (wf/rand-sid)]

                             (println "processing " v sid)

                             (async/put! chan> (with-meta
                                                 {sid v}
                                                        {:expand-key sid}))
                             )
                           (recur))
                  chan>))

          :infinite true
          :expands? true
          }



      }


))






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




(compojure/defroutes app
  (compojure/GET "/api/websocket" [:as req]


    (httpkit/with-channel req chan

      (swap! *ws-context merge
             ;; todo: separate collect and not collect hadlers
             {:out {
               :fn (fn[xs]
                 ;xs
                 (locking *out* (println "OUT:" (last xs)))

                 (httpkit/send! chan (write-transit-str (last xs)))

                 xs
                 )

             :collect? true
             :infinite true
                     }

              })


      (println "CONTEXT:" @*ws-context)

      (let [in-chan> (async/chan)
            steps {
              ::IN [:in in-chan>]
              ::OUT [:out ::IN]}


          executor (wf/build-executor (wf/make-context @*ws-context) steps)
          wf-chan> (wf/execute! executor)]

        (httpkit/on-close chan
                          (fn [status]
                            (println "Disconnected")
                            (wf/end! executor)
                            ))

        (httpkit/on-receive chan
                            (fn [payload]
                                (println "Recieved:" (read-transit-str payload))

                              (let [msg (read-transit-str payload)
                                     [ping message] msg]

                                (go
                                  (async/put! in-chan> msg))
                                )))


        (go-loop []
           (let [[status data] (async/<! wf-chan>)]
             ; (locking *out* (println status (d/pretty data)))

             (if (not
                     (or (= :done status)
                         (= :error status)))
               (recur))
             ))


        )





      ))

  (compojure/GET "/" [] (response/resource-response "public/index.html"))

  (compojure/GET "/ajax" [] (write-transit-str "Hello from AJAX"))

  (route/resources "/" {:root "public"}))



(defonce server (atom nil))

(defn stop-server []
  (when-not (nil? @server)
    ;; graceful shutdown: wait 100ms for existing requests to be finished
    ;; :timeout is optional, when no timeout, stop immediately
    (@server :timeout 100)
    (reset! server nil)))


(defn -main [& args]
  (println "Starting server at port 8080")
  (reset! server (httpkit/run-server #'app {:port 8080})))


; (stop-server)
;(-main)
