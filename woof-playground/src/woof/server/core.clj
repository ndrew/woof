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

    [woof.server.scraper.core :as server-scraper]
    [woof.server.state :as state]

    [woof.base :as base])
  (:gen-class))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;


;; this ns is used as an endpoint.


(defonce *INITBLOCK (do
                      (init-logging!)
                      (timbre/info ::initialized)
                      :logging/initialized
                      ))
;;


;; ring-handler doesn't work with websockets, but we leave this here to serve figwheel
(compojure/defroutes
  ring-handler
  (route/resources "/" {:root "public"}))


(defn run-server [port]
  (base/auto-run-wf! state/*server-wf #(server-scraper/server-wf! (merge state/ws-cfg
                                                                         {:port port}))
                     :on-stop (fn [state]
                                (println "on reload")
                                )
                     )
  )





;; server entry point, for standalone running
(defn -main [& args]
  (info ::-main)

  ;; todo: how pass port here from configuration
  ;; (run-server 8081)

  )

