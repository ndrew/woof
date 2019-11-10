(ns ^:figwheel-hooks woof.playground.prototype5
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]

    [woof.ui :as ui]
    [woof.u :as u]
    [woof.data :as d]

    ;; ns for running wfs

    ; internal
    [woof.playground.common :as cmn]

    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; -------------------------- ui-state

(defonce
  *UI-STATE (atom
              {

               :outer-wf { ;; wf that prepares inner wf

                          ;; all
                          :status :not-configured

                          ;; not-configured

                          ;; :configuring

                          ;; :configured

                          :actions []
                          :ui-fn dummy-ui-fn
                          }

               :inner-wf {

                          :status :not-configured

                          :actions []
                          :ui-fn dummy-ui-fn
                          }

               }))


;; --- exports

(declare <ui>)

(def init! (partial cmn/default-init! *UI-STATE))
(def reload! (partial cmn/default-reload! *UI-STATE))


;; -------

;; workflow configuration prototype


(defn dummy-ui-fn [*wf]
  [:div "dafuq"])



;; wf => :not-configured
(defn reset-wf [*wf]
  (swap! *wf merge {:status :not-configured

                    :actions []
                    :ui-fn dummy-ui-fn
                    }))

(defn start-configuring [*wf]
  (swap! *wf assoc :status :configuring))


(rum/defc <status> < rum/static [status]
  [:span.status {:style {
                         :margin-left "1rem"
                         :margin-right "1rem"
                         }} (str "<" (name status) ">")])


(rum/defc <wf-list> < rum/reactive [*wf]
  [:div
   [:div "..."]
   [:div {:style {:margin "1rem"}}
    (ui/menubar "" [
                    ["ok" (fn [])]
                    ["cancel" (fn [])]
                    ])
    ]

   [:div
    [:header "resulting WF:"]

    [:div {:style {:margin "1rem"}}
    (ui/menubar "" [
                    ["ok" (fn [] (swap! *wf assoc :status :configured))]
                    ["cancel" (fn [] (reset-wf *wf))]
                    ])
     ]
    ]
   ]
  )

(rum/defc <run-ui> < rum/reactive [*wf]
  [:div "wf had been configured"]
  )

(rum/defc <generic-wf> < rum/reactive [*wf status-ui-map]
  (let [wf @*wf
        status (:status wf)]
     [:div {:style {:margin-top "1rem"}}
      (let [ui (get status-ui-map status)]
        (if-not (nil? ui)
          (ui *wf)
          [:h1 (str "unknown status " status)])
          )
     ]
    )
  )

(rum/defc <inner-wf> < rum/reactive [*wf]
  (let [wf @*wf
        status (:status wf)]
    [:div
     [:div {:style {:display "flex"}}
      (ui/menu-item "✖︎" (fn [] (reset-wf *wf)))
      (<status> status)
      [:header "Inner Workflow"]]

     [:div {:style {:margin-top "1rem"}}
      #_(condp = status
        :not-configured
        :configuring    <wf-list>
        :configured     <run-ui>
        [:h1 (str "unknown status " status)]
        )
      ]
     ]
    )

  )



(rum/defc <ui> < rum/reactive [*STATE]
  (let [*outer-wf (rum/cursor-in *STATE [:outer-wf])
        *inner-wf (rum/cursor-in *STATE [:inner-wf])
        ]
    [:div
     [:div {:style {:display "flex"}}
      (ui/menu-item "✖︎" (fn [] (reset-wf *outer-wf)))
      (<status> (:status @*outer-wf))
      [:header "Outer Workflow"]]

     (<generic-wf> *outer-wf
                   {
                  :not-configured (fn [*wf] (ui/menubar "" [["configure" (fn [] (start-configuring *wf))]]))
                  :configuring    <wf-list>
                  :configured     <run-ui>
                  }
                   )
     [:hr {:style {:margin "1rem"}}]

     [:div {:style {:display "flex"}}
      ; (ui/menu-item "✖︎" (fn [] (reset-wf *outer-wf)))
      (<status> (:status @*inner-wf))
      [:header "Inner Workflow"]]

     ;;
     ;; don't care about statuses - they may be different for each wf
     ;;

     (<generic-wf> *inner-wf
                   {

                    })

     ]

    )

  )

;; root ui component for prototype
(def <app> #(<ui> *UI-STATE))
