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

(defonce *server-wf (atom nil)) ;; move this to separate ns


(defonce *scraping-session (atom {}))


(defn new-scraping-session [id]
  (swap! *scraping-session assoc id {
                                     :id id
                                     :created (u/now)
                                     })
  )