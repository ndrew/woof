(ns woof.ui.steps
  (:require
    [cljs.core.async :as async]
    [cljs.test :as test :refer-macros [deftest is testing run-tests async]]

    [goog.string :as gstring]
    [goog.string.format]

    [rum.core :as rum]

    [woof.app-data :as app-model]

    [woof.data :as d]
    [woof.graph :as g]
    [woof.wf :as wf]
    [woof.ws :as ws]

    [woof.ui :as ui]
    [woof.wf-ui :as wf-ui]
    [woof.utils :as u]

    )


  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]
    [woof.utils-macros :refer [put!?]]))


(rum/defcs <steps> < rum/reactive
  (rum/local nil  ::hid)

  [local *STEPS-MAP context-map]

  [:div.steps-map-ui

   [:h4 "steps:"]

   (into [:div.step]
         (map (fn [[k [hid v]]]
                [:div
                 (d/pretty k)
                 (ui/menu-item (d/pretty hid) (fn[]
                                                (.warn js/console v)
                                                ))
                 ]

                ) @*STEPS-MAP)
         )


   ]
  )

