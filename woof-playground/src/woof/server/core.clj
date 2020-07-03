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
    [taoensso.timbre :as timbre
     :refer [;; log
             trace debug info warn error fatal report
             logf tracef debugf infof warnf errorf fatalf reportf
             spy get-env]]

    [woof.server.log :refer [init-logging!]]
    [woof.server.transport :as tr]
    [woof.server.scraper.core :as scraper-wf]
    )
  (:gen-class))


;; this ns is used as an endpoint.


(defonce *INITBLOCK (do
                      (init-logging!)
                      (timbre/info ::initialized)
                      :logging/initialized
                      ))
;;

(defn has-cli-arg? [arg]
  (filter #(= % arg) *command-line-args*))


(defonce *dev (atom true))            ;; whether to wrap reload ring-handler

(defonce AUTO-RUN-WF?
         (has-cli-arg? "--run-backend-wf"))


(defonce *SERVER (atom nil))  ; store server instance



(defn in-dev?
  "whether we are in dev mode, so there will be wrap-reload used"
  [] @*dev)



;; ----------------- reloadable wf here --------------------


;; how to handle reloadable workflow based  ws handler?

(defonce *server-wf (atom nil))

(defn- init-wf! []
  (info "[Backend]  Initializing WF")
  (reset! *server-wf (scraper-wf/scraper-wf!))
  (info "[Backend]  Starting WF")
  ((:start-wf! @*server-wf)))

(info (seq AUTO-RUN-WF?))


(when (seq AUTO-RUN-WF?)
  (if-let [old-instance @*server-wf]
    ;; re-start wf if it's already running
    (let [stop-wf-fn! (:stop-wf! old-instance)]
      (info "[Backend]  Stopping WF - started")
      (stop-wf-fn! (fn [stop-chan]
                     (go
                       (let [stop-signal (async/<! stop-chan)] ;; todo: timeout if stop-signal is not being sent?
                         (info "[Backend]  Stopping WF - done!")
                         (init-wf!)
                         )
                       )
                     (info "[Backend]  Stopping WF - callback")
                     )
                   ))
    ;; else
    (init-wf!))
    )


;; ---------------------------------------------------------


;; ring-handler for figwheel
(compojure/defroutes
  ring-handler
  ;; serve the application
  (compojure/GET "/" [] (response/resource-response "public/index.html"))
  ;; and it's resources
  (route/resources "/" {:root "public"})

  (compojure/GET "/scraper-ws" [:as request]
    ;; call currently running server workflow
    ((:handle-ws @*server-wf) request)
    )

  ;; testing ajax calls
  (compojure/GET "/test" []
    (tr/write-transit-str
      ((:handle-get @*server-wf))
      )
    )
  )




;;
;; standalone server

(defn stop-server []
  (when-not (nil? @*SERVER)
    ;; graceful shutdown: wait 100ms for existing requests to be finished
    ;; :timeout is optional, when no timeout, stop immediately
    (@*SERVER :timeout 100)
    (reset! *SERVER nil)))


(defn run-server [port]
  (let [handler (if (in-dev?)
                  (reload/wrap-reload (site #'ring-handler)) ;; only reload when dev
                  (site ring-handler))]

    (reset! *SERVER (httpkit/run-server handler {:port port}))
    (info ::started port)

    )
  )


;; server entry point, for standalone running
(defn -main [& args]
  (debug ::-main)

  (reset! *dev false)
  (run-server 8080)
  )