(ns woof.server.core
  "woof client/server communication endpoint."
  (:require

    [compojure.core :as compojure]
    [compojure.route :as route]
    [compojure.handler :refer [site]]

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

(defonce AUTO-RUN-WF?
         (has-cli-arg? "--run-backend-wf"))

;; ----------------- reloadable wf here --------------------


;; how to handle reloadable workflow based  ws handler?

(defonce *server-wf (atom nil))

;; todo: read port from command line args
(defonce ws-cfg { :port 8081 })

(info (seq AUTO-RUN-WF?) ws-cfg)



;; runs the workflow
(defn- WF! [cfg]
  (info "[Backend]  Initializing WF")
  (reset! *server-wf (scraper-wf/scraper-wf! cfg))
  (info "[Backend]  Starting WF")
  ((:start-wf! @*server-wf)))


(when (seq AUTO-RUN-WF?)
  (if-let [old-instance @*server-wf]
    ;; re-start wf if it's already running
    (let [stop-wf-fn! (:stop-wf! old-instance)]
      (info "[Backend]  Stopping WF - started")
      (stop-wf-fn! (fn [stop-chan]
                     (go
                       (let [stop-signal (async/<! stop-chan)] ;; todo: timeout if stop-signal is not being sent?
                         (info "[Backend]  Stopping WF - done!")
                         (WF! ws-cfg)
                         )
                       )
                     (info "[Backend]  Stopping WF - callback")
                     )
                   ))
    ;; else
    (WF! ws-cfg)
    ))

;; ---------------------------------------------------------


;; ring-handler doesn't work with websockets, but we leave this here to serve figwheel
(compojure/defroutes
  ring-handler
  (route/resources "/" {:root "public"}))




(defn run-server [port]
  ;; run wf
  ;; (WF! {:port port}) ;; todo: pass port into wf
  )


;; server entry point, for standalone running
(defn -main [& args]
  (debug ::-main)
  (run-server 8081))