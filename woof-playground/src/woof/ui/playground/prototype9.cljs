(ns ^:figwheel-hooks woof.ui.playground.prototype9
  (:require
    [rum.core :as rum]


    ;; client core
    [woof.base :as base]
    [woof.wf :as wf]
    [woof.ui :as ui]
    [woof.u :as u]
    [woof.data :as d]
    [woof.utils :as utils]

    [woof.wfc :as wfc
     :refer [WoofWorkflow
             get-params
             get-context-map
             get-steps]
     ]


    ;; core async
    [cljs.core.async :as async]

    ;;
    [sablono.core :as sablono :refer-macros [html]]

    [woof.ui.playground.common :as cmn]

    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



(defonce *UI-STATE (atom {}))


;; --- exports

(declare <ui>)

(def init! (partial cmn/default-init! *UI-STATE))
(def reload! (partial cmn/default-reload! *UI-STATE))


(defn wf? [d]
  (keyword? d))



(rum/defc <wf-ui> < rum/reactive [wf]
  (if (wf? wf)
    [:div.wf
      [:.wf-body
       (pr-str wf)
       ]
     [:.result ">>"]
     ]
    (if (vector? wf)
      [:div.container
       (into [:div.container-body
              ;(pr-str wf)
              ]
             (map <wf-ui> wf))
       [:.result ">>>"]
       ]
      [:div "GIVNO"]
      )
    )
  )

(rum/defc <ui> < rum/reactive [*STATE]
  (let [
        ;wf-tree [:wf]
        ;wf-tree [:wf1 :wf2 :wf3]
        wf-tree1 [:wf1
                  [:wf2]
                 :wf3
                 [:wf4
                    [:wf5
                      [:wf6]]]
                 ]

        ; all nests
        ;wf-tree [:state :data :deploy]
        wf-tree [:state [:data1 :data2] :deploy
                 [:data3 [:data4 :data5
                          [:data6 [:data7]]
                          ]]
                 ]
        ]

    [:div.proto9
     [:pre
      (d/pretty wf-tree)]
     [:hr]

     (<wf-ui> wf-tree)

     ]
    )
  )

;; root ui component for prototype
(def <app> #(<ui> *UI-STATE))
