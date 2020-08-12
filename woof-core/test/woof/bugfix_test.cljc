(ns woof.bugfix-test
  (:require
    [clojure.test :refer [deftest testing is]]

    [woof.base :as base]
    ))



;; composing steps of maps was not working properly
(deftest as-fn-list-bug
  (let [v (base/as-fn-list [{
                     ::hello [:value "woof"]
                     }])]


    (is (vector? v))
    (let [[f] v]
      (is (= {
              ::hello [:value "woof"]
              } (f {})))
      )
    )

  )

(deftest ^:bug collecting-empty-map-via-collect-step

  (let [wf-impl (base/wf!
                  :ctx {
                        :value     {:fn (fn [v] v)}
                        :collect {:fn (fn [v] v)
                                  :collect? true
                                  }
                        }

                  :steps [{
                                   ::step-returning-empty-map [:value {}]

                                   ::collect [:collect [::step-returning-empty-map]]
                                   }])]

    (let [result @(base/sync-run-wf! wf-impl)]
      ;; resulting {} should not be converted to '()
      (is (= '({}) (::collect result)))
      )
    )

  )


#_(let [
      nu-opts (concat
                (base/as-fn-list opt-fns)
                [(partial test-wf-opts_ ready)])

      wf-impl
      (base/wf! :init init-fns
                :steps steps-fns
                :opts nu-opts
                :ctx ctx-fns
                )
      ]
  (base/run-wf! wf-impl identity)
  )