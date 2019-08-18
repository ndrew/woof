(ns woof.core.runner-test
  (:require
    [clojure.core.async :as async :refer [go go-loop]]

    [woof.data :as d]
    [woof.wf :as wf]

    [woof.core.runner :as runner]

    [woof.wfc :as wfc]
    [woof.utils :as u]
    [woof.xform :as x]

))

;; testing higher order wfs

(defn context-fn [& r] ;; & {:keys []}
  (println "->CTX: " r)
  {
    :log  {:fn (fn[a]
                 (locking *out* (println "DBG:" a))
                 (identity a))}

    :log*  {:fn (fn[a]
                 (locking *out* (println "DBG:" a))
                 (identity a))
            :collect? true
            }

})


(defn steps-fn [& r]  ;; & {:keys []}
  (println "->STEPS: " r)
  {
    ::hello [:log "Hello!"]

    })




(defn wf-fn [initial-params]
  (println "wf-fn: " initial-params)

  {
    :wf (fn [params]
            (wfc/params-wf params context-fn steps-fn)
          ) ;; (partial wwf in-chan> out-chan< *local) ;; wf constructor
    :params (merge initial-params {})
    }
)


(defn opts-fn [params]
  (println "opts-fn: " params)
  {
      :params params
      :opts {
              :before-process (fn[wf-chan xtor]
                                (println "Hello World")
                                :ok
                                )
              }
      }
  )


;; todo: wrap to a test

(runner/run-wf
      (fn []
        (println "INIT")
        {:hello :world}) ;; defaults
      wf-fn  ;; (fn [params] -> {:wf <wf>, :params {}})
      opts-fn
      runner/default-run-fn ;; todo: sync runner
  )
