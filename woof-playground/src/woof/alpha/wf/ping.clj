(ns woof.alpha.wf.ping
  "simplest workflow example"
  (:require
    [clojure.core.async :as async :refer [go go-loop]]

    [taoensso.timbre :as timbre :refer [info error]]

    [woof.base :as base]
    [woof.data :as d]
    [woof.utils :as u])
)


(defn print-results-opts-fn [params]
  {
   :after-process   (fn [exec-chann]
                      (info ::after-process)
                      exec-chann)

   :op-handlers-map {
                     :done  (fn [result]
                              (info ::done (d/pretty result)))
                     :error (fn [result]
                              (error ::error (d/pretty result)))
                     }

   })

(defn init-fn [params]
  ;; (info ::init-fn)
  {
   :t (u/now)
   }
  )

(defn ctx-fn [params]
  {
   :id {:fn identity}
   }
  )

(defn steps-fn [params]
  {
   ::ping [:id (:t params)]
   }
  )


(defn wf-run! [wf]
  (base/run-wf! wf identity))


(defn wf-run-sync! [wf]
  (base/sync-run-wf! wf identity))




(defn ping-wf-sync []
  (let [wf-impl (base/parametrized-wf!
             (base/combine-init-fns [init-fn])
             identity
             identity
             print-results-opts-fn
             ctx-fn
             steps-fn
             )
        ]
    (wf-run-sync! wf-impl)))



(defn prepare-response [wf-result]
  (pr-str (::ping wf-result)))