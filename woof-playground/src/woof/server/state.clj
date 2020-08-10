(ns woof.server.state
  "woof server state "
  (:require

    [clojure.core.async :as async :refer [go go-loop]]

    ; logging
    [taoensso.timbre :as timbre
     :refer [;; log
             trace debug info warn error fatal report
             logf tracef debugf infof warnf errorf fatalf reportf
             spy get-env]]

    [woof.base :as base]
    [woof.utils :as u])
  )


(defn has-cli-arg? [arg]
  (filter #(= % arg) *command-line-args*))

(defonce AUTO-RUN-WF?
         (seq (has-cli-arg? "--run-backend-wf")))

(defonce ws-cfg { :port 8081 }) ;; todo: read port from command line args


;; todo: routing of which backend wf to run

(defonce *server-wf (atom nil)) ;; move this to separate ns

(defonce *scraping-session (atom {}))

(defn get-scraping-session [id]
  (if-let [session (get @*scraping-session id)]
    session
    (swap! *scraping-session assoc id {
                                       :id id
                                       :created (u/now)
                                       :data []
                                       :summary {}
                                       })
    )
 )
