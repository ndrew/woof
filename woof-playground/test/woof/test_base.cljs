(ns woof.test-base
  (:require
    [cljs.core.async :as async]
    [cljs.test :refer-macros [use-fixtures deftest is testing async]]

    [woof.base :as base])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


(defn- test-wf-opts_ [ready params]
  {
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
   })


;; way to run a wf as an async test
(defn test-wf [init-fns
               ctx-fns
               steps-fns
               opt-fns]

  (async ready
    (let [
          nu-opts (conj
                    (base/as-fn-list opt-fns)
                    (partial test-wf-opts_ ready))

          wf-impl
          (base/wf! :init init-fns
                    :steps steps-fns
                    :opts nu-opts
                    :ctx ctx-fns
                    )
          ]
      (base/run-wf! wf-impl identity)
      )))


(defn run-simple-wf [ctx steps done-fn]
  (test-wf
    [] ;; init-fn
    ctx
    steps
    [(fn [params] ;; opts
       {:timeout 100
        :op-handlers-map {:done done-fn}})]
    )

  )
