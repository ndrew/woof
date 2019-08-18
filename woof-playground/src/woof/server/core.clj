(ns woof.server.core
  "woof client/server communication endpoint."
  (:require

    [compojure.core :as compojure]
    [compojure.route :as route]
    [compojure.handler :refer [site]]

    [org.httpkit.server :as httpkit]

    [ring.util.response :as response]
    [ring.middleware.reload :as reload]

    [woof.server.transport :as tr]

    [clojure.core.async :as async :refer [go go-loop]]

    [woof.data :as d]
    [woof.cc :as cc]

    ; [woof.wf :as wf]
    ; [woof.wf-data :as wdata]
    ; [woof.test-data :as test-data]

    ; [woof.server.utils :refer [read-transit-str write-transit-str]]
    ; [woof.example.files :as files-wf]

    [woof.server.log :refer [init-logging!]]

    [woof.utils :as u]

    ; logging
    [taoensso.timbre :as timbre]


    ;; prototypes

    [woof.prototype.preview :as preview]
    )
  (:gen-class))



;;



;; start command center
(defonce *dev (atom true))

(defn in-dev?
  "whether we are in dev mode, so there will be wrap-reload used"
  [] @*dev)

(defonce *cc-wf (atom nil))
; server handle
(defonce server (atom nil))


(defonce initialize-block
  (do
    (init-logging!)
    (timbre/info ::init-server)

    (reset! *cc-wf (cc/cc-wf!))

    ::initialized
    ))



(compojure/defroutes
  app
  ;; serve the application
  (compojure/GET "/" [] (response/resource-response "public/index.html"))
  ;; and it's resources
  (route/resources "/" {:root "public"})

  ;; cc websocket api
  (compojure/GET "/cc" [:as req] ((:response-handler @*cc-wf) req))

  ;; websocket
  #_(compojure/GET "/api/files" [:as req]
                   (httpkit/with-channel req chan
                                         (ws-wf! chan files-wf/prepare-content-map
                                                 files-wf/prepare-steps)))

  ;; testing ajax calls
  (compojure/GET "/test" [] (tr/write-transit-str "Hello from AJAX"))

  )



(defn stop-server []
  (when-not (nil? @server)
    ;; graceful shutdown: wait 100ms for existing requests to be finished
    ;; :timeout is optional, when no timeout, stop immediately
    (@server :timeout 100)
    (reset! server nil)))


(defn run-server [port]
  (let [handler (if (in-dev?)
                  (reload/wrap-reload (site #'app)) ;; only reload when dev
                  (site app))]

    (reset! server (httpkit/run-server handler {:port port}))
    (info ::started port)

    )
  )


;; main entry point
(defn -main [& args]
  (reset! *dev false)
  (run-server 8080))

