(ns woof.server.core
  "woof client/server communication endpoint."
  (:require

    [compojure.core :as compojure]
    [compojure.route :as route]
    [compojure.handler :refer [site]]

    [org.httpkit.server :as httpkit]

    [ring.util.response :as response]
    [ring.middleware.reload :as reload]

    [clojure.core.async :as async :refer [go go-loop]]

    ; logging
    [taoensso.timbre :as timbre]

    [woof.cc :as cc]

    [woof.server.log :refer [init-logging!]]
    [woof.server.transport :as tr]

    ;; prototypes
    [woof.prototype.preview :as preview]
    )
  (:gen-class))


;; OLD CORE TODO: migrate core

;;

;; start command center
(defonce *dev (atom true))

(defn in-dev?
  "whether we are in dev mode, so there will be wrap-reload used"
  [] @*dev)


; server handle
(defonce *SERVER (atom nil))

; woof command center workflow
(defonce *cc-wf (atom nil))





(defonce INITIALIZATION-BLOCK
         (do
            (init-logging!)
            (timbre/info ::init-server)

            ;; INIT WF
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
  ;; todo: wait for the wf
  (compojure/GET "/cc" [:as req] ((:response-handler @*cc-wf) req))
  (compojure/GET "/reload-cc" [:as req] (do
                                          (reset! *cc-wf (cc/cc-wf!)
                                          )

  ;; websocket
  #_(compojure/GET "/api/files" [:as req]
                   (httpkit/with-channel req chan
                                         (ws-wf! chan files-wf/prepare-content-map
                                                 files-wf/prepare-steps)))

  ;; testing ajax calls
  (compojure/GET "/test" [] (tr/write-transit-str "Hello from AJAX"))
  )



(defn stop-server []
  (when-not (nil? @*SERVER)
    ;; graceful shutdown: wait 100ms for existing requests to be finished
    ;; :timeout is optional, when no timeout, stop immediately
    (@*SERVER :timeout 100)
    (reset! *SERVER nil)))


(defn run-server [port]
  (let [handler (if (in-dev?)
                  (reload/wrap-reload (site #'app)) ;; only reload when dev
                  (site app))]

    (reset! *SERVER (httpkit/run-server handler {:port port}))
    (info ::started port)

    )
  )


;; server entry point, for standalone running
(defn -main [& args]

  (reset! *dev false)
  (run-server 8080)
  )