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


    [woof.server.utils :refer [read-transit-str write-transit-str]]
    [woof.example.files :as files-wf]

    )
  (:gen-class))


;;
;; websocket workflow

;; (compojure/GET "/your/websocket/url" [:as req]
;;  (httpkit/with-channel
;;    req chan
;;    ;; chan — socket channel
;;
;;    (ws-wf! chan  ;; will run the workflow
;;      (fn[socket in out] {}) ;; -> context map,
;;      (fn[in out] {})        ;; -> steps

      ;; messages from socket channel are put to in channel is linked to a ws

      ;; message from workflow should be put to out channel,
      ;;   which later should be written to socket via
      ;;
      ;;  (httpkit/send! socket-chan (write-transit-str value))

;;  )))
;;



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

      }
))






;; todo: add optional named arguments to ws f

(defn ws-wf!
  "compojure handler for httpkit/with-channel"
  [socket-chan context-fn steps-fn]

  (let [in-chan> (async/chan)
        out-chan< (async/chan)

        context-map (context-fn socket-chan in-chan> out-chan<)
        steps        (steps-fn in-chan> out-chan<)]

    (let [xtor (wf/build-executor (wf/make-context context-map)
                                   steps)]

      (httpkit/on-receive socket-chan
        (fn [payload]
          (let [msg (read-transit-str payload)]
            (go
              (async/put! in-chan> msg)))))

      (httpkit/on-close socket-chan
        (fn [status]
          (wf/end! xtor)
          ;; maybe, get the wf results some how

          (async/close! in-chan>)
          (async/close! out-chan<)
          ))

      ;; run the workflow
      (wf/process-results! (wf/->ResultProcessor xtor {})) ;; todo: processing opts
      )
    )
  )


(defn ws-prepare-content-map
  "returns the context config for the ws workflow"
  [socket-chan in out-chan<]  ;; context-map

  (merge @*ws-context         ;; use context 'shared' between client and server
         {

           ;; expand step that adds steps to the current ws
           :client<  (wf/receive-steps-handler
                       :step-fn (fn [steps]
                                  (println "SERVER RECEIVE: " (d/pretty steps))
                                  steps))


           ;; step that sends the value back to client
           :client> (wf/send-value-handler out-chan<
                                           :v-fn (fn[v]
                                                   (println "SERVER SEND: " (d/pretty v))
                                                   v)
                                           :out-fn (fn [z]
                                                     (println "SEND TO SOCKET:" z)
                                                     (httpkit/send! socket-chan (write-transit-str z))
                                            ))

           ;; todo :client>*

           }))

(defn ws-prepare-steps
  "returns initial steps for the ws workflow"
  [in-chan> out]

  {
    ;; optional initialization can be done here
    ::receive-loop [:client< in-chan>]
    }
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
                   (ws-wf! chan ws-prepare-content-map
                                 ws-prepare-steps)))

  (compojure/GET "/api/files" [:as req]
                 (httpkit/with-channel req chan
                   (ws-wf! chan files-wf/prepare-content-map
                                 files-wf/prepare-steps)))



  ;; testing ajax calls
  (compojure/GET "/ajax" [] (write-transit-str "Hello from AJAX")))


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
