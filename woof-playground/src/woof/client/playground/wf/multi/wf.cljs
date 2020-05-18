(ns ^:figwheel-hooks
  woof.client.playground.wf.multi.wf
  (:require
    [cljs.core.async :as async]

    [woof.client.playground.wf.common :as cmn]
    [woof.base :as wf]
    [woof.utils :as u])

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;;
;; multi wf demo

;; single playground state state



(defn multi-wf-initializer [*SWF]
  (let [EVT-LOOP  (cmn/make-chan (wf/rand-sid "evt-loop"))]
    {

     :title       "Workflow consisting of other workflows"

     :explanation [:div.explanation
                   [:p "Each aspect of the computation is a separate workflow"]]
     ;;
     :init-fns    [
                   cmn/chan-factory-init-fn
                   (fn [params] {::EVT-LOOP EVT-LOOP})    ;; expose evt loop for other workflows
                   ]

     ;;
     :ctx-fns     [cmn/common-ctx-fn]

     ;;
     :steps-fns   [(fn [params]
                     {
                      ::#EVT-LOOP# [:evt-loop (::EVT-LOOP params)]

                      ::hello      [:id :boo]

                      })
                   ]

     :opt-fns     [cmn/chan-factory-opts-fn]

     :wf-actions  {

                   :running [
                             ["emit evt" (fn []
                                           (async/put! EVT-LOOP
                                                       {(wf/rand-sid) [:id
                                                                       (str "---event-"
                                                                            (name (u/rand-sid)))]}
                                                       )
                                           )]
                             ]

                   :done    [["log WF state" (fn []
                                               (prn "Workflow is ready. WF state is:")
                                               (.log js/console @*SWF))
                              ]]}

     })
  )

(defn ^:before-load before-reload-callback []
  (println "BEFORE reload!!!")
  )
