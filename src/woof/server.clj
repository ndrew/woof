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
    [woof.wfc :as wfc]
   ; [woof.wf-data :as wdata]

    [woof.utils :as u :refer [inline--fn1]]
    [woof.test-data :as test-data]

    [woof.server.utils :refer [read-transit-str write-transit-str httpkit-opts httpkit-opts-impl]]
    [woof.example.files :as files-wf]

    [woof.example.ws :as ws]

    [woof.wf.edn-editor.backend :as fs]
    )
  (:gen-class))


;;
;; websocket workflow


;; workflow can be used for RPC via websocket









;; deprecated
(defn ws-wf!
  "compojure handler for httpkit/with-channel"
  [socket-chan context-fn steps-fn]

  ; (println "SERVER: start wf")

  (let [in-chan> (async/chan)
        out-chan< (async/chan)

        ;; & {:keys [ui-chan server-in server-out endpoint ]}
        context-map (context-fn socket-chan in-chan> out-chan<)
        ;; [& {:keys [ui-chan server-in server-out]}]

        steps        (steps-fn in-chan> out-chan<)]

    (let [xtor (wf/build-executor (wf/make-context context-map)
                                   steps)]

      (httpkit/on-receive socket-chan
        (fn [payload]
          (let [msg (read-transit-str payload)]
            ; (println "SERVER: got " (d/pretty msg))
            (go
              (async/put! in-chan> msg)))))

      (httpkit/on-close socket-chan
        (fn [status]
          ; (println "SERVER: end wf")
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





(defn websocket-ws
  ([wf! opts-fn]
   (websocket-ws wf! opts-fn {}))

  ([wf! opts-fn initial-params]
  (let [;; init wf constructor + wf defaults
        {
          wf-fn :wf
          wf-default-params :params
         } (wf!)

        ;;
         params (merge wf-default-params initial-params)

        ;; prepare params for specific runner
        {
          opts-params :params
          opts :opts
          } (opts-fn params)

         xtor (wfc/wf-xtor (wf-fn opts-params))
        ]

    ;; workflow default params + user provided params => wf params
    (wf/process-results! (wf/->ResultProcessor xtor opts))

    ))
  )


;;
;; server

(compojure/defroutes app
  ;; serve the application
  (compojure/GET    "/" [] (response/resource-response "public/index.html"))
  (route/resources   "/" {:root "public"})



  (compojure/GET "/api/config" [:as req]

                 (httpkit/with-channel req socket-chan
                   (websocket-ws fs/wf!
                                 (partial httpkit-opts socket-chan)
                                 {:initial-command [:file "/Users/ndrw/m/woof/test/data/config.edn"]})))



  (compojure/GET "/api/test" [:as req]
                 (httpkit/with-channel req socket-chan
                   (websocket-ws ws/prepare-wf (partial httpkit-opts socket-chan))
                   ))
  ;;
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




(comment
  [woof.blog.backend :as blog]


  #_(compojure/GET "/api/blog" [:as req]
                   (httpkit/with-channel req socket-chan

                     (server-wf!
                       (blog/prepare-params socket-chan)
                       blog/prepare-content-map
                       blog/prepare-steps
                       ;;
                       blog/msg-fn
                       blog/close-fn
                       )

                     )
                   )
  )
