(ns ^:figwheel-hooks woof.playground.prototype13
  (:require
    [rum.core :as rum]

    ;; client core

    ;; core async
    [cljs.core.async :as async]

    [woof.playground.common :as cmn]
    [woof.playground.v1.playground :as pg]
    [woof.playground.v1.utils :refer [dstr kstr vstr]]
    [woof.playground.v1.ui :as ui]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



;; re-use state map from v1
(defonce *UI-STATE  pg/*STATE-MAP)


;; --- exports

(declare <ui>)

(def init! (partial cmn/default-init! *UI-STATE))
(def reload! (partial cmn/default-reload! *UI-STATE))



(def ASPECTS
  {
   :test-wf {}
   }
  )

;; ui


(rum/defc <wf-header> < rum/static [wf actions-map]
  (let [status (:status wf)]
    [:span.status
     (ui/menubar (str (pr-str (:id wf)) " (" status ")")
                 (get actions-map status [])
                 )

     ]
    )
  )

(rum/defc <wf> < rum/reactive [*wf]
  (let [wf @*wf
        aspects (get wf :aspects [])
        ]
    [:div
     [:div.flex
      (<wf-header> wf
                  {
                   :compile [
                             ["prepare" (fn []
                                          (prn "foo")
                                          (swap! *wf assoc :status :prepare)
                                          )]
                             ]
                   :prepare [
                             ["run" (fn [])]
                             ]
                   :running [
                             []
                             ]
                   }
                  )
      [:div {:style {:flex-grow 1}}]
      (ui/btn "✗" (fn []
                    (if (js/confirm "delete?")
                      (pg/remove-wf! (:id wf)))))
      ]
      ;(ui/<debug> aspects)
      (ui/<debug> @*wf)
    ]
    )
  )



(rum/defc <ui> < rum/reactive [*STATE]
  [:div

   (ui/menubar "prototype 13" [["add wf" pg/add-wf!]])

   (let [st @*STATE
         wf-defs (get-in st [:UI-wfs] [])]

     [:pre (map (fn [item]
                  (let [{id :id} item
                        *wf (rum/cursor-in *STATE [:WFs id])]
                    (<wf> *wf)))
                wf-defs)]
     )


   [:hr]
   (ui/<debug> @*STATE)
  ])








;; root ui component for prototype
(def <app> #(<ui> *UI-STATE))
