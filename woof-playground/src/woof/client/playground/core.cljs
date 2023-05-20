(ns ^:figwheel-hooks
  woof.client.playground.core
  (:require

    [cljs.core.async :as async]
    [rum.core :as rum]

    [goog.log :as glog]

    [woof.base :as base]
    [woof.utils :as u]

    ;; common workflow stuff and ui
    [woof.client.stateful :as state]
    [woof.client.playground.ui :as ui]
    [woof.client.ws :as ws]

    [woof.playground.common :as cmn]


    ;; streets processing
    [woof.client.playground.streets.pg :as streets-wf]
    [woof.client.playground.streets.kga :as kga-wf]

    ;; apt playground
    [woof.client.playground.apt.wf :as apt-wf]

    ;;
    [woof.client.playground.scraper.cc :as scraper-wf]
    [woof.client.playground.scraper.tw :as tw-wf]

    ;; examples
    ;;
    ;; example of frontend ui
    [woof.client.playground.ui.internal :as internal]
    [woof.client.playground.ui.wf :as wf-ui]


    [woof.client.playground.wf.simple :as simple-wf]
    [woof.client.playground.wf.simple-custom-ui :as simple-w-ui]

    [woof.client.playground.wf.sandbox :as sandbox-wf]
    [woof.client.playground.wf.expand :as expand-wf]

    [woof.client.playground.wf.listing :as listing-wf]

    [woof.client.playground.wf.page :as page-wf]
    [woof.client.playground.wf.post :as post-wf]
    [woof.client.playground.wf.preview :as preview-wf]
    [woof.client.playground.wf.in-out :as in-out-wf]

    [woof.client.playground.wf.multi.wf :as multi-wf]
    [woof.client.playground.wf.multi.templating-wf :as templating-wf]
    ; [woof.client.playground.wf.multi.meta-wf :as meta-wf]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;;
;; playground wf runner function
;;


(defn playground-wf-handler [{initial-state ::initial-state
                              update? ::update?
                              wf-init-fn ::init-fn
                              auto-run? ::auto-run
                              } *SWF]
  (let [nu-wf-state (wf-init-fn *SWF)

        run-wf-fn! (partial state/swf-run! *SWF)
        stop-wf-fn! (partial state/swf-stop! *SWF)
        reset-wf-fn! (partial state/wf-init! *SWF)
        wf-state (if update? nu-wf-state
                             (merge
                               initial-state
                               nu-wf-state
                               {
                                :title   (get nu-wf-state :title "Untitled WF")

                                :run!    run-wf-fn!
                                :stop!   stop-wf-fn!
                                :reset!  reset-wf-fn!

                                :actions (state/default-actions-map
                                           reset-wf-fn!
                                           run-wf-fn!
                                           stop-wf-fn!
                                           (get nu-wf-state :wf-actions {}))

                                :ui-fn   (get nu-wf-state :ui-fn (partial wf-ui/<wf-UI>
                                                                          wf-ui/<default-body>))
                                }
                               ))
        ]

    (swap! *SWF merge wf-state)

    (when auto-run?
      (.log js/console "auto-running wf")
      (run-wf-fn!)
      )
    )
  )

(defn playground-wf!
  [wf-cfg]

  (let [wf-id (::wf-id wf-cfg)
        initial-state (state/empty-swf wf-id)]
    (state/wf wf-id (partial playground-wf-handler
                             (merge
                               {::initial-state initial-state}
                               wf-cfg)))))


;; updatible storage

;; this will be a project tree
;; * each node starting with wf- is a separate workflow
;; * internal - internal woof stuff

(defn init-test-wfs [update?]
  (merge
    (array-map)
    {
     :wf-tw-wf (playground-wf! {::wf-id    :scraper-wf
                                ::init-fn  (fn [*swf] (tw-wf/wf! *swf))
                                ::update?  update?
                                ::auto-run true})
     }

    {
     ; scraper-wf
     :wf-scraper-wf (playground-wf! {::wf-id    :scraper-wf
                                     ::init-fn  (fn [*swf] (scraper-wf/wf! *swf))
                                     ::update?  update?
                                     ::auto-run true
                                     })

     }

    ;; apt workflows
    {
     :wf-streets-wf (playground-wf! {::wf-id    :streets-wf
                                     ::init-fn  (fn [*swf] (streets-wf/wf! *swf))
                                     ::update?  update?
                                     ::auto-run true})
     }

    {
     :wf-streets-kadastr (playground-wf! {::wf-id    :kadastr-wf
                                          ::init-fn  (fn [*swf] (kga-wf/wf! *swf))
                                          ::update?  update?
                                          ::auto-run true})
     }

    {
     :wf-apt (playground-wf! {::wf-id    :atp-wf
                              ::init-fn  (fn [*swf] (apt-wf/wf! *swf))
                              ::update?  update?
                              ::auto-run true})
     }

    ;; old workflows - todo: go through them
    #_{

     :wf-sandbox    (playground-wf! {::wf-id   :wf-sandbox
                                     ::init-fn sandbox-wf/sandbox-wf-init!
                                     ::update? update?
                                     ::auto-run true
                                     })

     :wf-basic-wf   (playground-wf! {::wf-id   :wf-basic-wf
                                     ::init-fn simple-wf/basic-wf-initializer
                                     ::update? update?})

     :wf-with-ui    (playground-wf! {::wf-id   :wf-with-ui
                                     ::init-fn simple-w-ui/wf-with-ui-initializer
                                     ::update? update?}
                                    )

     :wf-expand     (playground-wf! {::wf-id   :wf-expand
                                     ::init-fn expand-wf/expand-wf-init!
                                     ::update? update?})

     :wf-multi      (playground-wf! {::wf-id   :wf-multi
                                     ::init-fn multi-wf/wf-as-process-initializer
                                     ::update? update?
                                     ;;
                                     ;;::auto-run true
                                     })

     :wf-templating (playground-wf! {::wf-id    :wf-templating
                                     ::init-fn  templating-wf/hiccup-template-wf-initializer
                                     ::update?  update?
                                     ::auto-run true
                                     })

     :wf-meta       (playground-wf! {::wf-id    :wf-meta
                                     ::init-fn  (fn [*swf] (meta-wf/meta-wf-initializer *swf))
                                     ::update?  update?
                                     ::auto-run true
                                     })


     ;:wf-page (playground-wf! {::wf-id    :wf-page
     ;                 ::init-fn  (fn [*swf] (page-wf/initialize! *swf))
     ;                 ::update?  update?
     ;                 ::auto-run true
     ;                 })


     ;; car grouping ui
     :wf-listings              (playground-wf!
                                 {::wf-id    :wf-meta
                                  ::init-fn  (fn [*swf] (listing-wf/initialize! *swf))
                                  ::update?  update?
                                  ::auto-run true
                                  })


     ;:wf-local-storage-post    (init-alpha-wf! update? :wf-local-storage-post post-wf/init-post-wf!)
     ;:wf-local-storage-preview (init-alpha-wf! update? :wf-local-storage-preview preview-wf/init-preview-wf!)

     ;:wf-IN-OUT                (init-alpha-wf! update? :wf-IN-OUT in-out-wf/initialize-in-out-wf)
     }
    )

  )


(defn global-action []
  ;(prn "I am a configurable global action")

  (let [ch (async/chan)
        *socket (volatile! nil)]

    (ws/connect "ws:localhost:8081/ws"
                :on-init (fn [socket]
                           (vreset! *socket socket)
                           ch
                           )
                :on-open (fn []
                           (async/put! ch @*socket)
                           (.log js/console "opened"))

                :on-message (fn [payload]
                              (let [msg (ws/read-transit payload)]
                                   (.log js/console "PAYLOAD" msg))))

    (go
      (let [socket (async/<! ch)]
        (let [msg {:hello :woof!
                   :t     (u/now)}]

          (ws/send-transit! socket msg))))

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
  (state/wf-init! *wf-state)
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

            ;; maybe ask workflow which keys to update for running wf?

            keys-for-update (get nu-wf-map :playground/keys-to-update-on-reload [:ui-fn :actions :title :wf-actions :explanation
                                                                                 :init-fns :steps-fns :opt-fns])

            _upd-map (select-keys nu-wf-map keys-for-update)
            upd-map (into {
                           :error nil
                           } (filter (fn [[k v]] (not (nil? v)))
                                     _upd-map))
            ]
        ;; what of wf map can be updated for running workflow
        (.log js/console "updating " keys-for-update " in the current workflow " wf-id  )
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
  (let [*wf (rum/cursor-in *TREE current-selector)
        wf @*wf]
    [:div.woof-playground

     (ui/menubar (pr-str current-selector)
                 (concat
                   [

                    ["←" (partial back-to-project-selector *TREE current-selector)]
                    [(str "stop WF on reload " (ui/shorten-bool (:stop-wf-on-reload? @*INTERNAL))) (fn []
                              (swap! *INTERNAL update :stop-wf-on-reload? not)

                              )]
                    ]
                   (global-menu-items wf-actions)))

     ((:ui-fn wf) *wf)
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
                                                                  :code  "keyCode"}]
                                                  [key (aget e attr)]))]
                                (global-keydown chord)
                                ))
                         false)

    (reset! *initialized true)
    )
  )


(defn do-js-reload
  "update the running workflow if needed"
  []
  (let [tree @*TREE
        curr (::current tree)]

    (if-let [curr-wf-id (first curr)]
      (do
        (.log js/console (str "WF " curr-wf-id "is working - updating"))
        (if-not (:stop-wf-on-reload? @*INTERNAL)
          (update-current-wf! tree curr)
          ;; else
          (stop-current-wf! tree curr curr)))
      (do
        (prn "NO WF is working - updating all workflows")
        (swap! *TREE merge (init-test-wfs false) {::current []})
        )
      )
    ))


(defn ^:after-load on-js-reload [d]
  (when (goog.object/get js/window "PLAYGROUND")
    (do-js-reload)))


