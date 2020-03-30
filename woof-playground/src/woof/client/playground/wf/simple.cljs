(ns ^:figwheel-hooks woof.client.playground.wf.simple
  (:require
    [cljs.core.async :as async]
    [woof.utils :as u])
  (:import [goog.net.XhrIo ResponseType])

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;;
;; alpha workflow example

;; here we test the idea of WF initializer function:
;;   we need to return a WF map containing:
;;    * necessary stuff: init-fn/ctx-fn/steps-fn
;;    * optional stuff:
;;        ui fn, wf actions, etc
;;        also we pass WF atom where this map will live


(defn basic-wf-initializer [*SWF]
  {

   :title       "Finite WF with default UI"

   :explanation [:div.explanation

                 [:p "This is an introductory playground workflow. Playground provides the workflow with a possibility to have an UI."]

                 [:p "This workflow is finite, that is it will stop after all steps are processed."]

                 [:p "Hypothesis: Function composition (f(g(x))) can be done with woof workflows."
                  "Example of this can be map enrichment (like ring middle-ware)"]

                 [:pre {:style {:font-size "8.5pt" :padding-left "2rem"}}
"// start with some initial data passed to :id step handler
::value [:id {:initial :map}] \n
// do f(value)
::f [:f ::value] \n
// do z(f(value)) — z is async, so further steps will wait for the result
::z [:z-async ::f] \n
// do g(z(f(value)))
::g [:g ::z] \n"]]

   ;;
   :init-fns    []

   :ctx-fns     [(fn [params]
                   {

                    :id      {:fn identity}
                    :f       {:fn (fn [v]
                                    (prn ":F")
                                    (assoc v :f true))}

                    :g       {:fn (fn [v]
                                    (prn ":G")
                                    (assoc v :g true))}

                    :z-async {:fn (fn [v]
                                    (let [ch (async/chan)
                                          t 3000]

                                         (prn ":Z - starting")
                                         (go
                                           (async/<! (u/timeout t))
                                           (prn ":Z - done")
                                           (async/put! ch (assoc v :z true)))

                                         ch))}
                    }
                   )]

   :steps-fns   [(fn [params]
                   {

                    ::value [:id {:initial :map}]

                    ::f     [:f ::value]

                    ::z     [:z-async ::f]

                    ::g     [:g ::z]

                    })
                 ]
   :opt-fns     []

   :wf-actions  {
                 :done [["log WF state" (fn []
                                          (prn "Workflow is ready. WF state is:")
                                          (.log js/console @*SWF))
                         ]]}

   }
  )

