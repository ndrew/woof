(ns ^:figwheel-hooks woof.client.playground.wf.expand
  (:require
    [cljs.core.async :as async]
    [woof.test-data :as test-data]
    [woof.base :as base])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))

;;
;;
;; use this workflow as a sandbox for
;;
;;


(defn expand-wf-init! [*NODE]
  (let [ {
          context :context
          steps :steps
          }

        (binding [
                  test-data/*xpand-sample-rate* 0.5
                  test-data/*xpand-step-sample-rate* 0.8
                  test-data/*link-sample-rate* 0.9
                  ]
          (test-data/get-test-steps-and-context 50)
          )
        ]
    {
     :title       "Expand workflow"

     :explanation [:div.explanation
                   "test workflow for UI"]

     :init-fns    []

     :ctx-fns     [(fn [params] context)]

     :steps-fns   [(fn [params] steps)]

     :opt-fns     [(fn [params]
                     {

                      ;; send wf updates every 500 ms
                      :execute (partial base/_timed-execute-fn 500)
                      }
                     )]

     }

    )
  )



