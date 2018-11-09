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

    ;;;

    [woof.core.runner :as runner]
    )
  (:gen-class))

;;
;; server


;; shorthand for running the ws wf
(defn run-ws-wf [socket-chan
                  init-fn
                  wf-fn]
  (runner/run-wf
      init-fn
      wf-fn
      (partial httpkit-opts socket-chan)
      runner/default-run-fn))


(compojure/defroutes app
  ;; serve the application
  (compojure/GET    "/" [] (response/resource-response "public/index.html"))
  (route/resources   "/" {:root "public"})


  ;; file text editor backend
  (compojure/GET "/api/config" [:as req]
                 (httpkit/with-channel req socket-chan
                   (run-ws-wf socket-chan
                                (fn [] {:initial-command [:file "/Users/ndrw/m/woof/test/data/config.edn"]})
                                 fs/wf-fn)))


  ;; test workflow
  ;(compojure/GET "/api/test" [:as req]
  ;               (httpkit/with-channel req socket-chan
  ;                 (websocket-ws ws/prepare-wf (partial httpkit-opts socket-chan))
  ;                 ))

  ;; file selector wf
  ;(compojure/GET "/api/files" [:as req]
  ;               (httpkit/with-channel req chan
  ;                 (websocket-ws
  ;                               files-wf/wf!
  ;                               (partial httpkit-opts chan)
  ;                   )))

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
