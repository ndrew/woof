(ns woof.server.playground.wf.ping
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



(defn ping-wf-sync []
  (let [wf-impl (base/wf!
             :init  { :t (u/now) }
             :ctx   { :id {:fn identity} }
             :steps (fn [params] {::ping [:id (:t params)] })
             :opts  print-results-opts-fn)]

    (base/sync-run-wf! wf-impl)))



(defn prepare-response [wf-result]
  (pr-str (::ping wf-result)))