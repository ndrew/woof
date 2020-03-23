(ns woof.test-base
  (:require
    [cljs.core.async :as async]
    [cljs.test :refer-macros [use-fixtures deftest is testing async]]

    [woof.base :as base])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; way to run a wf as an async test
(defn test-wf [init-fns
               ctx-fns
               steps-fns
               opt-fns]

  (async ready
    (let [wf-impl
          (base/parametrized-wf!
            (base/combine-init-fns init-fns)
            identity ; wf-params-fn
            identity ; opt-params-fn
            (base/combine-fns
              (conj opt-fns
                    (fn [params]
                      {
                       ;; timeout
                       ;; :after-process (fn [exec-chann]  exec-chann)

                       :op-handlers-map {
                                         :done  (fn [result]
                                                  ;(.log js/console ::after-process)
                                                  (ready))

                                         :error (fn [result]
                                                  (is (= "" (.-message result)))
                                                  (is (= "" (.-stack result)))

                                                  (.error js/console result)
                                                  ;(.log js/console ::after-process)
                                                  (ready))
                                         }
                       }))
              :merge-results base/merge-opts-maps)
            (base/combine-fns ctx-fns)
            (base/combine-fns steps-fns))
          ]
      (base/run-wf! wf-impl identity)
      )))


(defn run-simple-wf [ctx steps done-fn]

  (test-wf
    [] ;; init-fn
    [(fn [params] ctx)] ;; ctx-fn
    [(fn [params] steps)] ;; steps
    [(fn [params] ;; opts
       {:timeout 100
        :op-handlers-map {:done done-fn}})]
    )

  )
