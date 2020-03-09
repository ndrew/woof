(ns ^:figwheel-hooks woof.alpha.wf
  (:require

    ;; common workflow stuff and ui
    [woof.playground.common :as cmn]
    [woof.playground.v1.ui :as ui]

    ;; alpha ui
    [woof.alpha.ui.internal :as internal]
    [woof.alpha.ui.wf :as wf-ui]

    ;; alpha workflow
    [woof.v2.wf.stateful :as st-wf]
    ;; example of frontend ui
    [woof.alpha.wf.test :as test-wf]
    [woof.playground.state :as state]
    [woof.data :as d])

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;;
;; playground wf runner function
;;

;; todo: decouple this
(defn init-alpha-wf! [wf-id wf-init-fn]
  (let [initial-state (state/empty-swf wf-id)]
    (st-wf/wf wf-id
              (fn [*SWF]
                (let [nu-wf-state (wf-init-fn *SWF)

                      updated-state (merge
                                      initial-state
                                      nu-wf-state
                                      {
                                       :title   (get nu-wf-state :title "Untitled WF")

                                       :actions (st-wf/default-actions-map
                                                  (partial st-wf/wf-init! *SWF)
                                                  (partial state/swf-run! *SWF)
                                                  (partial state/swf-stop! *SWF)
                                                  (get nu-wf-state :wf-actions {}))

                                       :ui-fn   (get nu-wf-state :ui-fn (partial wf-ui/<default-wf-ui>
                                                                                 wf-ui/<default-body>))
                                       })
                      ]

                     (swap! *SWF merge updated-state)
                     )
                ))
    )
  )
