(ns ^:figwheel-hooks woof.client.playground.core
  (:require

    [cljs.core.async :as async]
    [rum.core :as rum]

    [goog.log :as glog]

    ;; common workflow stuff and ui
    [woof.playground.common :as cmn]
    [woof.client.playground.ui :as ui]


    ;; alpha workflow
    [woof.client.stateful :as st-wf]
    ;; example of frontend ui
    [woof.client.playground.ui.internal :as internal]
    [woof.client.playground.ui.wf :as wf-ui]

    [woof.client.playground.wf.simple :as simple-wf]
    [woof.client.playground.wf.simple-custom-ui :as simple-w-ui]

    [woof.client.playground.wf.sandbox :as sandbox-wf]
    [woof.client.playground.wf.expand :as expand-wf]

    [woof.client.playground.wf.page :as page-wf]
    [woof.client.playground.wf.listing :as listing-wf]
    [woof.client.playground.wf.post :as post-wf]
    [woof.client.playground.wf.preview :as preview-wf]
    [woof.client.playground.wf.in-out :as in-out-wf]

    [woof.client.stateful :as state]

    [woof.client.ws :as ws]

    [woof.utils :as u]
    [woof.base :as base])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;;
;; playground wf runner function
;;

(defn playground-wf! [update? wf-id wf-init-fn]
  (let [initial-state (state/empty-swf wf-id)]

    (st-wf/wf wf-id
              (fn [*SWF]
                (let [nu-wf-state (wf-init-fn *SWF)]

                     (if update?
                       (do
                         (swap! *SWF merge nu-wf-state)
                         )

                       (do
                         (swap! *SWF merge
                                (merge
                                  initial-state
                                  nu-wf-state
                                  {
                                   :title   (get nu-wf-state :title "Untitled WF")
                                   ;; todo: should run/stop fn be exposed

                                   :actions (st-wf/default-actions-map
                                              (partial st-wf/wf-init! *SWF)
                                              (partial state/swf-run! *SWF)
                                              (partial state/swf-stop! *SWF)
                                              (get nu-wf-state :wf-actions {}))

                                   :ui-fn   (get nu-wf-state :ui-fn (partial wf-ui/<wf-UI>
                                                                             wf-ui/<default-body>))
                                   }))

                         )
                       )


                     )
                ))
    )
  )

;; updatible storage

;; this will be a project tree
;; * each node starting with wf- is a separate workflow
;; * internal - internal woof stuff

(defn init-test-wfs [update?]

  ;; todo: is it needed to call init-alpha-wf! here?
  ;; or it should be called if the button is pressed
  {

   :wf-sandbox  (playground-wf! update?
                                :wf-sandbox sandbox-wf/sandbox-wf-init!)
   ;; workflow w state (keywords that start with wf-...)
   :wf-basic-wf (playground-wf! update? :wf-basic-wf simple-wf/basic-wf-initializer)
   :wf-with-ui  (playground-wf! update? :wf-with-ui simple-w-ui/wf-with-ui-initializer)

   :wf-expand   (playground-wf! update? :wf-expand expand-wf/expand-wf-init!)

   ;:wf-page                  (init-alpha-wf! update? :wf-page page-wf/initialize!)
   ;:wf-listings              (init-alpha-wf! update? :wf-listings listing-wf/initialize!)

   ;:wf-local-storage-post    (init-alpha-wf! update? :wf-local-storage-post post-wf/init-post-wf!)
   ;:wf-local-storage-preview (init-alpha-wf! update? :wf-local-storage-preview preview-wf/init-preview-wf!)

   ;:wf-IN-OUT                (init-alpha-wf! update? :wf-IN-OUT in-out-wf/initialize-in-out-wf)
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
                       (init-test-wfs false)
                       {
                      ;; internal
                      :internal           {
                                           :ping       0
                                           :show-menu? false

                                           ;; what should the defaults be, false
                                           :stop-wf-on-reload? false

                                           }
                      ;; alpha ui stuff
                      ::current           []

                      ::global-actions    [
                                           ;; ["global action" global-action]
                                           ]

                      ::global-wf-actions {
                                           ;:wf-dummy [["wf specific action" (fn [] (prn "I am a global action for wf-dummy"))]]
                                           }
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

                            (==>workflow-selected *wf-state)

                            ;; todo: maybe call init-alpha-wf! should be done here
                            ;; (swap! *WF-TREE assoc k (init-alpha-wf! (str k) (get nu-tree k)))
                            ;; (==>workflow-selected (rum/cursor-in *WF-TREE (::current nu-tree)))

                            ))

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


(defn stop-current-wf! [tree curr new-curr]
  (let [wf-running? (= :running (get-in tree (conj curr :status)))
        ch (if wf-running?
             (do
               (prn "stopping the " (first curr))
               (base/end! (get-in tree (concat curr [:runtime :xtor])))
               )
             (let [dummy-ch (async/chan)]
               (async/put! dummy-ch "done")
               dummy-ch)
             )]

    (go
      (async/<! ch)
      (let [updated-wfs (init-test-wfs false)]
        (swap! *TREE merge updated-wfs {::current []})

        (when-not (empty? new-curr)
          (prn "restarting " new-curr)
          (==>workflow-selected (rum/cursor-in *TREE new-curr))
          (swap! *TREE merge {::current new-curr})
          )

        )
      )
    ))

(defn update-current-wf! [tree curr]


  (let [[wf-id] curr
        updated-wfs (init-test-wfs true)
        updated-wf (get-in updated-wfs curr)

        other-wfs (dissoc updated-wfs wf-id)
        ]
    ;; update other workflows
    (swap! *TREE merge other-wfs)

    (let [*dummy-state (atom updated-wf)]

      (==>workflow-selected *dummy-state)

      (let [nu-wf-map @*dummy-state

            _upd-map (select-keys nu-wf-map [:ui-fn :actions :title :wf-actions :explanation

                                             :init-fns :steps-fns :opt-fns
                                             ])
            upd-map (into {
                           :error nil
                           } (filter (fn [[k v]] (not (nil? v)))
                                     _upd-map))
            ]
        ;; what of wf map can be updated for running workflow
        (prn "updating the selected workflow " wf-id )
        ;(.warn js/console upd-map)
        (swap! *TREE update-in curr merge upd-map)

        ;(.log js/console "PREV" (get-in tree curr))
        ;(.log js/console "NU" nu-wf-map)
        )
      )
    )
  )


(defn back-to-project-selector [*TREE current-selector]
  (stop-current-wf! @*TREE current-selector [])
  )


(rum/defc <wf-ui> < rum/reactive [*TREE current-selector wf-actions]
  (let [*wf (rum/cursor-in *TREE current-selector)]
    [:div.woof-playground

     (ui/menubar (pr-str current-selector)
                 (concat
                   [["←" (partial back-to-project-selector *TREE current-selector)]]
                   (global-menu-items wf-actions)))

     (let [wf @*wf]
       ((:ui-fn wf) *wf))
     ]
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



;; Global key listener

(defn global-keydown [chord]
  (when (and (:meta chord) (= (:code chord) 191))
    ;(prn "shift+? pressed")
    (swap! *INTERNAL update-in [:show-menu?] not)
    )
  )


;;
;; WORKFLOW RELOADING
;;
;; figwheel reloading doesn't propagate changes into already defined/running workflows.
;;
;; we can either stop the running wf, update the wf and restart it
;; or try updating the parts of workflow that can be updated, like ui
;;



(defonce *initialized (atom false))


(when (goog.object/get js/window "PLAYGROUND")


  ;; DISABLE FIGWHEEL LOGGING, for now

  (when-not @*initialized
    (prn "initialing playground")

    (let [logger (glog/getLogger "Figwheel")]
      (.setLevel logger goog.debug.Logger.Level.WARNING))

    (js/addEventListener "keydown"
                         (fn [e]
                           (let [chord (into {} (for [[key attr] {:shift "shiftKey" :ctrl "ctrlKey" :alt "altKey" :meta "metaKey"
                                                                  :code "keyCode"}]
                                                  [key (aget e attr)]))]
                                (global-keydown chord)
                                ))
                         false)

    (reset! *initialized true)
    )


  (defn ^:after-load on-js-reload [d]

    (let [tree @*TREE
          curr (::current tree)]

      (if-let [curr-wf-id (first curr)]
        (do
          (prn "WF " curr-wf-id "is working - updating")
          (if-not (:stop-wf-on-reload? @*INTERNAL)
            (update-current-wf! tree curr)
            ;; else
            (stop-current-wf! tree curr curr)))
        (do
          (prn "NO WF is working - updating all workflows")
          (swap! *TREE merge (init-test-wfs false) {::current []})
          )
        )
      )


    )
  )

