(ns ^:figwheel-hooks
  woof.client.playground.wf.common
  (:require
    [cljs.core.async :as async]

    [woof.base :as wf]
    [woof.utils :as u]
  )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; common wf stuff, reused via other playground wf.

;; candidate for going to base


(defonce *CHAN-STORAGE (atom {}))

(defonce CHAN-FACTORY (wf/chan-factory *CHAN-STORAGE))


;; Provide a channel factory for a workflow.
(defonce chan-factory-init-fn (wf/build-init-chan-factory-fn CHAN-FACTORY))
(defonce chan-factory-opts-fn (wf/build-opts-chan-factory-fn CHAN-FACTORY))

(def make-chan (partial wf/make-chan CHAN-FACTORY))


(defn common-ctx-fn [params]
  {
   :id {:fn identity}

   :evt-loop {
              :fn       (fn [in-chan] in-chan)
              :infinite true
              :expands? true
              }
   }
  )

#_(defn ^:after-load my-after-reload-callback []
  ;(println "AFTER reload!!!")
  )
