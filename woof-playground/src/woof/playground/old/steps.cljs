(ns woof.playground.old.steps
  (:require
    [rum.core :as rum]

    [woof.data :as d]

    [woof.client.playground.ui :as ui]
    ))


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

