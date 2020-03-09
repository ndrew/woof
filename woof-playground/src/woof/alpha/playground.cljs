(ns ^:figwheel-hooks woof.alpha.playground
  (:require

    [rum.core :as rum]

    ;; common workflow stuff and ui
    [woof.playground.common :as cmn]
    [woof.playground.v1.ui :as ui]

    ;; alpha ui
    [woof.alpha.ui.internal :as internal]
    [woof.alpha.ui.wf :as wf-ui]

    [woof.alpha.wf :as awf]

    ;; alpha workflow
    [woof.v2.wf.stateful :as st-wf]
    ;; example of frontend ui
    [woof.alpha.wf.test :as test-wf]
    [woof.alpha.wf.page :as page-wf]
    [woof.playground.state :as state]
    [woof.base :as base]
    [cljs.core.async :as async])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



;; updatible storage

;; this will be a project tree
;; * each node starting with wf- is a separate workflow
;; * internal - internal woof stuff

(defn init-test-wfs []
  {
   ;; workflow w state (keywords that start with wf-...)
   :wf-simplest-wf     (awf/init-alpha-wf! "A" test-wf/simplest-wf-initializer)
   :wf-dummy           (awf/init-alpha-wf! "dummy" test-wf/dummy-wf-initializer)

   :wf-page            (awf/init-alpha-wf! "file preview" page-wf/initialize!)
   }
  )

(defonce *TREE (atom (merge
                       (init-test-wfs)
                       {
                      ;; alpha

                      ;; internal
                      :internal           {
                                           :ping       0
                                           :show-menu? false
                                           }
                      ;; alpha ui stuff
                      ::current           []

                      ::global-actions    [["global action" (fn [] (prn "I am a configurable global action"))]]

                      ::global-wf-actions {
                                           :wf-dummy [["wf specific action" (fn [] (prn "I am a global action for wf-dummy"))]
                                                      ]}
                      })))


(defonce *INTERNAL (rum/cursor-in *TREE [:internal]))


;; ui

(def init! (partial cmn/default-init! *TREE))
(def reload! (partial cmn/default-reload! *TREE))


(defn ==>workflow-selected [*wf-state]
  ;; inits workflow:
  ;; - each wf contains both state map + behaviour
  (st-wf/wf-init! *wf-state)
  )


(defn- global-menu-items [global-actions]
  (if (seq global-actions) (into [[]] global-actions) [])
  )

;; finds workflow state in state tree
(defn <project-selector> [*WF-TREE current-selector]

  (let [wf-keyword? (fn [k]
                        (and
                          (keyword? k)
                          (not (qualified-keyword? k))
                          (re-matches #"wf-.*" (name k))))

        select-node! (fn [k]
                       (let [nu-tree (swap! *WF-TREE assoc-in [::current] (conj current-selector k))
                             *wf-state (rum/cursor-in *WF-TREE (::current nu-tree))]
                            (==>workflow-selected *wf-state)))

        WF-TREE @*WF-TREE

        menu-items (map (fn [k] [(name k) (partial select-node! k)])
                        (filter wf-keyword? (keys WF-TREE)))

        global-actions (get-in WF-TREE [::global-actions] [])
        ]

    (ui/menubar (if-not (seq menu-items) "W O O F _ A L P H A — go configure some workflows ! ! !" "W O O F _ A L P H A")
                (concat
                  menu-items
                  (global-menu-items global-actions) ;; just add some actions to add new workflow
                  ))))


(defn back-to-project-selector [*TREE]
  ;; TODO: if wf is running then prompt about ending
  ;; (when (js/confirm "delete wf?") ...)
  (swap! *TREE assoc-in [::current] []))


(rum/defc <wf-ui> < rum/reactive [*TREE current-selector wf-actions]
  (let [*wf (rum/cursor-in *TREE current-selector)]
    [:div
     (ui/menubar (pr-str current-selector)
                 (concat
                   [["←" (partial back-to-project-selector *TREE)]]
                   (global-menu-items wf-actions)))
     [:hr]

     [:div {:style {:padding "1rem"
                    :background-color "rgba(255,255,0,.043333)"}}
      ;; todo: check if the wf has own ui
      (let [wf @*wf]
        ((:ui-fn wf) *wf))
      ]           ]
    )
  )


(rum/defc <project-ui> < rum/reactive [*TREE]

   (let [TREE @*TREE
         current-selector (get-in TREE [::current])
         root? (= [] current-selector)]

     [:div.woof
      (internal/<menu> *INTERNAL)

      (if root?
        (<project-selector> *TREE current-selector)
        (<wf-ui> *TREE current-selector
                       (get-in @*TREE [::global-wf-actions (last current-selector)])))
      ]
     )
  )


(def <app> #(<project-ui> *TREE))


(defn ^:after-load on-js-reload [d]

  (prn "FOR NOW: always RELOAD WFs:")
  ; (.log js/console (:reloaded-namespaces d))


  (let [tree @*TREE
        curr (::current tree)]

    (swap! *TREE merge (init-test-wfs) {::current []})

    ; (.log js/console tree)

    (if curr
      (let [ch (if (= :running (get-in tree (conj curr :status)))
                 (base/end! (get-in tree (concat curr [:runtime :xtor])))
                 (let [dummy-ch (async/chan)]
                   (async/put! dummy-ch "done")
                   dummy-ch)
             )]

        (go
          (async/<! ch)



          ;(prn "upd")
          ;; mimic selection of the wf
          (==>workflow-selected (rum/cursor-in *TREE curr))
          (swap! *TREE merge {::current curr})
          )
        )
        )
    )
  )