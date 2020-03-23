(ns woof.server.log
  "woof client/server communication endpoint."
  (:require
    ; logging
    [clojure.java.io :as io]
    [taoensso.timbre :as timbre]
    [taoensso.timbre.appenders.core :as appenders]
    ))

(def log-file-name "woof.log")

(defn init-logging! []
  ; Set up the name of the log output file and delete any contents from previous runs (the
  ; default is to continually append all runs to the file).

  (io/delete-file log-file-name :quiet)
  (timbre/refer-timbre) ; set up timbre aliases

  (timbre/merge-config!
    {
     ; Set the lowest-level to output as :debug
     :level        :debug  ; e/o #{:trace :debug :info :warn :error :fatal :report}

     ;; Control log filtering by namespaces/patterns. Useful for turning off logging in noisy libraries, etc.:
     :ns-whitelist [] #_["my-app.foo-ns"]
     :ns-blacklist [] #_["taoensso.*"]

     :middleware   [] ; (fns [data]) -> ?data, applied left->right

     ;; Clj only:
     ; :timestamp-opts default-timestamp-opts ; {:pattern _ :locale _ :timezone _}
     ; :output-fn default-output-fn ; (fn [data]) -> string

     :appenders {;; The standard println appender:
                 :println {:enabled? true}
                 :spit    (appenders/spit-appender {:fname log-file-name})}
     }
    )
  )