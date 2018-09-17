(ns woof.server
  "woof client/server communication endpoint."
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


;;
;;


;; current executor
(def *ws-wf (atom nil))

;; executor channel
(def *ws-chan (atom nil))

;; receiving wf in channel
(def *ws-in-chan (atom nil))
;; receiving wf out channel
(def *ws-out-chan (atom nil))

;; current socket channel
(def *ws-socket-chan (atom nil))


(def *ws-context
  (atom
    {

      ;; identity function
      :v  {:fn (fn[a] (identity a))}

      ;; debug
      :log  {:fn (fn[a]
                   (locking *out* (println "DBG:" a))
                   (identity a))}

      ;; reference
      :&v {:fn (fn[a]
                 {(wf/rand-sid) [:v a]})
           :expands? true}

      ;; collect
      :v* {:fn identity :collect? true}

      ;; zipper
      :zip {:fn (fn [vs]
                  (partition (count vs) (apply interleave vs)))
            :collect? true}


      ;; older steps
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




;;
;; receiving workflow

(defn init-receiving-wf!
  "inits a receiving workflow"
  [chan]
  (let [in-chan> (async/chan)
        out-chan< (async/chan)]

    (swap! *ws-context merge
           { :client< (wf/receive-steps-handler
                        :step-fn (fn [steps]
                                   (println "SERVER RECEIVE: " (d/pretty steps))
                                   steps
                                   ))

             :client> (wf/send-value-handler out-chan<
                                             :v-fn (fn[v]
                                                     (println "SERVER SEND: " (d/pretty v))
                                                     v)
                                             :out-fn (fn [z]
                                                       (println "SEND TO SOCKET:" z)
                                                       (httpkit/send! @*ws-socket-chan (write-transit-str z))
                                                       ))
             })

    (println "SERVER: initializing receiving wf")

    (let [steps {
                  ::loop [:client< in-chan>]
                  }

          executor (wf/build-executor (wf/make-context @*ws-context) steps)
          ]

      ;; store executor and in/out chans
      (reset! *ws-in-chan in-chan>)
      (reset! *ws-out-chan out-chan<)

      (reset! *ws-wf executor)
      (reset! *ws-socket-chan chan)
      )
    )
  )



(defn process-message!
  "processes message and adds it to a receiving wf"
  [in-chan> payload]
  (let [msg (read-transit-str payload)]
    (go
      (async/put! in-chan> msg))))


(defn close-receiving-wf! [xtor status]
  (println "WS: Disconnected" status)
  (wf/end! xtor)
  ;; todo: collect ws result?
)


;;
;; server

(compojure/defroutes app
  ;; serve the application
  (compojure/GET    "/" [] (response/resource-response "public/index.html"))
  (route/resources   "/" {:root "public"})
  ;; websocket
  (compojure/GET "/api/websocket" [:as req]
    (httpkit/with-channel req chan

      (init-receiving-wf! chan) ;; fixme: use local state instead of atoms

      (httpkit/on-receive chan (partial process-message! @*ws-in-chan))
      (httpkit/on-close chan   (partial close-receiving-wf! @*ws-wf))

      ;; run the workflow
      (wf/process-results! (wf/->ResultProcessor @*ws-wf {}))))

  ;; testing ajax calls
  (compojure/GET "/ajax" [] (write-transit-str "Hello from AJAX"))
)


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
