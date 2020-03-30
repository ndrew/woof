(ns ^:figwheel-hooks woof.client.playground.wf.expand
  (:require
    [cljs.core.async :as async]
    [rum.core :as rum]

    [woof.client.playground.ui.wf :as wf-ui]
    [woof.client.stateful :as st-wf]
    [woof.data :as d]
    [woof.wf :as wf]
    [woof.utils :as utils]
    [woof.test-data :as test-data])
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
          } (test-data/get-test-steps-and-context 100)
        ]
    {
     :title       "Expand workflow"

     :explanation [:div.explanation
                   "test workflow for UI"]

     :init-fns   []

     :ctx-fns    [(fn [params] context)]

     :steps-fns  [(fn [params] steps)]

     :opt-fns    []

     }

    )
  )



