(ns ^:figwheel-hooks woof.client.playground.core
  (:require

    [cljs.core.async :as async]
    [rum.core :as rum]

    ;; common workflow stuff and ui
    [woof.playground.common :as cmn]
    [woof.playground.v1.ui :as ui]



    ;; alpha workflow
    [woof.client.stateful :as st-wf]
    ;; example of frontend ui
    [woof.client.playground.ui.internal :as internal]
    [woof.client.playground.ui.wf :as wf-ui]

    [woof.client.playground.wf.simple :as test-wf]
    [woof.client.playground.wf.page :as page-wf]
    [woof.client.playground.wf.listing :as listing-wf]
    [woof.client.playground.wf.post :as post-wf]
    [woof.client.playground.wf.preview :as preview-wf]

    [woof.playground.state :as state]

    [woof.client.ws :as ws]

    [woof.utils :as u])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;;
;; playground wf runner function
;;

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

;; updatible storage

;; this will be a project tree
;; * each node starting with wf- is a separate workflow
;; * internal - internal woof stuff

(defn init-test-wfs []
  {
   ;; workflow w state (keywords that start with wf-...)
   :wf-simplest-wf     (init-alpha-wf! "A" test-wf/simplest-wf-initializer)
   :wf-dummy           (init-alpha-wf! "dummy" test-wf/dummy-wf-initializer)

   :wf-page            (init-alpha-wf! "file preview" page-wf/initialize!)

   :wf-listings        (init-alpha-wf! "listings" listing-wf/initialize!)

   :wf-local-storage-post (init-alpha-wf! "local storage: post" post-wf/init-post-wf! )
   :wf-local-storage-preview (init-alpha-wf! "local storage: preview" preview-wf/init-preview-wf!)
   }
  )

(defn global-action []
  ;(prn "I am a configurable global action")


  (let [socket (ws/connect "ws:localhost:8081/ws"
                           :on-open (fn []
                                      (.log js/console "opened")
                                      )
                           :on-message (fn [payload]
                                         (.log js/console "PAYLOAD" payload)
                                         )
                           )]


    (let [msg {:hello :woof!
               :t     (u/now)}]
      (js/setTimeout (fn [] (ws/send! socket msg) ) 1000)
      )

    ;; send a message

    )

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

                      ::global-actions    [["global action" global-action]]

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

    (ui/menubar (if-not (seq menu-items) "W O O F — go configure some workflows ! ! !" "W O O F")
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

;; uncomment for wf reloading

#_(when (goog.object/get js/window "PLAYGROUND")

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
  )

