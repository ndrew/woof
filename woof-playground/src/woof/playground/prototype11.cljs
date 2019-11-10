(ns ^:figwheel-hooks woof.playground.prototype11
  (:require
    [rum.core :as rum]

    [woof.playground.common :as cmn]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



(defonce *UI-STATE (atom {
                          ::wf {}

                          :wf1 {
                                ::status :prepare
                                ::history []
                                }
                          }))


;; --- exports

(declare <ui>)

(def init! (partial cmn/default-init! *UI-STATE))
(def reload! (partial cmn/default-reload! *UI-STATE))


(rum/defc <ui> < rum/reactive [*STATE]
  [:div

   [:header "defining wf"]

   [:p "create new state map for wf-id"]
   [:p "generate init-fn/ctx-fn/steps-fn/opts-fn"]
   [:p "how to add subst step ids - via special s-handler, via modifying steps before running wf"]

   [:p "<?> should run be called after IN are configured??"]

   [:p "generate IN/UI/OUT"]

   [:p "<?>"]
   [:p "a) will the wf be started manually, and then ended"]


   [:header "IN"]
   [:ul
    [:li "can be returned via init-fn."]
    [:p "<?> chaining init-fns?/restarting/etc"]
    [:li "can be returned via s-handler."]

    [:p "{::value-needed-for-wf [:IN-UI :in-key]}"]
    [:p "in state: :in-cfg [ [:in-key :label 'Provide' :input-type '...' :etc] ...]"]
    ]
   [:p ]

   [:header "UI"]
   [:p ""]

   [:header "OUT"]
   [:p "::out in state set via done through opts-fn"]
   [:p "::out in state set via separate step handler"]

   [:p "duality between state change after wf ended/during wf execution"]
   [:p "if wf is infinite - use s-handlers, else opts-fn"]

   [:p "b) or this will be achieved via single infinite wf?"]

   [:header "IN"]
   [:p ""]

   [:header "UI"]
   [:p ""]

   [:header "OUT"]
   [:p ""]


   [:hr ]
   ]
  )

;; root ui component for prototype
(def <app> #(<ui> *UI-STATE))
