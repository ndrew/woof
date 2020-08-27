(ns woof.server.state
  "woof server state "
  (:require

    [clojure.core.async :as async :refer [go go-loop]]

    [woof.base :as base]
    [woof.utils :as u])
  )

;; backend state, for now in-memory

(defn has-cli-arg? [arg]
  (filter #(= % arg) *command-line-args*))

(defonce AUTO-RUN-WF?
         (seq (has-cli-arg? "--run-backend-wf")))

;; todo: read port from command line args
(defonce ws-cfg { :port 8081 })

(defonce *server-wf (atom nil)) ;; move this to separate ns

;; todo: routing of which backend wf to run



;; ------------ global scraping session atom -------------

(defonce *scraping-session (atom {}))

(defn get-scraping-session [*scraping-session id]
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
