(ns ^:figwheel-hooks woof.ui.playground.prototype3
  (:require
    [rum.core :as rum]

    [woof.ui :as ui]

    [woof.u :as u]
    [woof.data :as d]

    ;; ns for running wfs

    ; internal
    ;[woof.wf :as wf]
    ;[woof.core.processors :as p]

    [woof.utils :as utils]
    [woof.core.runner :as runner]

    ;; higher level workflows
    [woof.wfc :as wfc
     :refer [WoofWorkflow
             get-params
             get-context-map
             get-steps]
     ]


    ;; core async
    [cljs.core.async :as async]

    [woof.base :as base]

    ;; client core
    [woof.client.ws :as ws]
    [woof.wf :as wf]
    [woof.ui.playground.common :as cmn]

    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; -------------------------- ui-state

;; although you can handle ui state as you want,
;; here is a playground for running wfs

(defonce
  *UI-STATE
  (atom
    {

     :status :list

     ;; list of workflows
     :wfs []

     :ui-wf {
             :status :init ;;:compile ;; :running

             :ui {
                  ;; ui wf here
                  :wf nil
                  :xtor nil
                  }

             :actions {
                       :compile [["run"]]
                       :running [["end!"]]
                       }

             :xtor nil
             :wf nil


             }

     }))


;; --- exports

(declare <ui>)

(def init! (partial cmn/default-init! *UI-STATE))
(def reload! (partial cmn/default-reload! *UI-STATE))


;; prototype


;; main ui


(defn run-wf [init-fn wf-fn opts-fn]
  (let [runner-fn runner/default-run-fn

        channel (runner/run-wf
                  init-fn
                  wf-fn
                  opts-fn

                  runner-fn)
        ]
    ; (println "z" channel)

    )
  )


;; actionable ui
; provides separate ui for each status + data
(rum/defc action-ui < rum/reactive [header ui-map actions-map status DATA]
  (let [ui (get ui-map status
                (fn[status data]
                  [:div "no ui provided for " (str status)
                   [:pre (pr-str data)]
                   ]
                  )
                )
        actions (get actions-map status [])
        always (get actions-map :always [])
        ]
    [:div
     (ui/menubar header (concat always actions))
     (ui status DATA)
     ]
    )
  )


(rum/defc <wf-ui> < rum/static [WF-MAP]
  (let [data WF-MAP
        ui-data (:ui data)
        status (:status ui-data)

        actions-map (:actions ui-data)
        ]
    [:div.wf

     (action-ui
       (str (:header data) " " (:id data))

       {:init (fn [status data]
                         ;; step before ui wf has been initialized
                         [:div "Initialization UI — maybe will be dropped in future."])

                 :compile (fn [status data]
                            ; ui for compiling the ui
                            [:div
                             "compilation"
                             #_(if-not wf
                               [:div "ui for compilation"]
                               )
                             ]
                            )
                 :running (fn [status data])

                 :done (fn [status data]
                         [:div
                          [:header "done"]
                          [:pre (d/pretty (:result ui-data))]
                          ]
                         )
                 }
                actions-map
                status
                data
                )

     ]
    )

  )

;; exposed ui


;;
(defn add-new-wf-1! [*STATE]
  (let [data @*STATE
        id (u/gen-uuid)]

    ;; add wf with empty ui map
    (swap! *STATE update-in [:wfs]
             conj
             {
              :id     id
              :header "wf.."

              :new? true

              :ui     {
                       :status  :init
                       :actions {}
                       }
              })

    id
    )
  )




;; how to parametrize this
(defn add-new-wf-2! [*ui-state]

  ;-- internal
  ; :ui-loop chan
  ; :ui-loop step handler,
  ; :ui-loop ::ui step

  ;-- external
  ; :before-process - update xtor, update status, store ui-loop in state
  ; :done, err, process

  ; actions
  ; optional ctx, steps

  (let [;; ui wf implementation
        ui-loop-init-fn (fn [params]
                          {
                           :ui-loop (async/chan) ;; init event loop chan
                           })

        ;; context map for ui wf (multi-arity)
        CTX-fn  (fn [& {:keys []}]
                  {
                   ;; this will add new actions to a wf
                   :ui-loop {:fn (fn [in-chan]
                                   (u/wiretap-chan in-chan (partial println "UI LOOP:"))
                                   )

                             :infinite true
                             :expands? true
                             }

                   ;; todo: how to pass own context functions
                   :yo1 {
                         :fn (fn [a]
                               (prn a)
                               a)
                         }
                   }
                  )
        ;; provides steps map from wf params (multi-arity)
        STEPS-fn (fn [& {:keys [ui-loop]}]
                   {
                    ::ui  [:ui-loop ui-loop]
                    }
                   )

        wf-params-fn identity
        opt-params-fn identity

        init-fn (base/combine-init-fns [ui-loop-init-fn])

        OPTS-fn (fn [& {:keys [ui-loop]}]
                  {
                   :before-process  (fn [wf-chan xtor]
                                      ;;(swap! *wf assoc :status :running)
                                      (.log js/console "ui wf started")

                                      ; store ui-loop channel

                                      (swap! *ui-state assoc-in [:state :ui-loop] ui-loop)
                                      ; store constructor
                                      (swap! *ui-state merge {:xtor   xtor
                                                              :status :running
                                                              })

                                      :ok
                                      )
                   :op-handlers-map {
                                     :process (fn [result]
                                                (.log js/console "intermediary results" result)
                                                )

                                     :error   (fn [err]
                                                (.error js/console err)

                                                (swap! *ui-state assoc-in [:status] :error)
                                                )
                                     :done    (fn [result]

                                                #_(let [result-w-meta (with-meta
                                                                        (merge
                                                                          IN
                                                                          result)
                                                                        @META)]

                                                    (.log js/console "wf done" result-w-meta))

                                                (.log js/console "done!" result)

                                                (swap! *ui-state assoc-in [:status] :done)
                                                (swap! *ui-state assoc-in [:result] result)
                                                ;; what to do with results?
                                                )
                                     }
                   }
                  )

        ctx-fn (base/arg-fn CTX-fn)
        steps-fn (base/arg-fn STEPS-fn)

        wf (base/parametrized-wf!
             init-fn
             wf-params-fn
             opt-params-fn
             (base/arg-fn OPTS-fn)
             ctx-fn
             steps-fn
             )

        wf-impl {
                 :wf     wf
                 :xtor   nil
                 :status :compile
                 :state  {}
                 }
        ]

    ;; maybe we need return just wf, but pass opts fn

    (swap! *ui-state merge wf-impl)
    )
  )


(defn default-ui-opts-fn [*ui-state]
  (let [OPTS-fn (fn [& {:keys [ui-loop]}]
                ; if not ui-loop
                  {
                   :before-process  (fn [wf-chan xtor]

                                      ; store ui-loop channel
                                      (swap! *ui-state assoc-in [:state :ui-loop] ui-loop)
                                      ; store constructor
                                      (swap! *ui-state merge {:xtor   xtor
                                                              :status :running
                                                              })

                                      (.log js/console "ui wf started"
                                            @*ui-state
                                            )

                                      :ok
                                      )
                   :op-handlers-map {
                                     :process (fn [result]
                                                (.log js/console "intermediary results" result)
                                                )

                                     :error   (fn [err]
                                                (.error js/console err)

                                                (swap! *ui-state assoc-in [:status] :error)
                                                )
                                     :done    (fn [result]
                                                (.log js/console "done!" result)

                                                (swap! *ui-state assoc-in [:status] :done)
                                                (swap! *ui-state assoc-in [:result] result)
                                                ;; what to do with results?
                                                )
                                     }
                   }
                  )]
    (base/arg-fn OPTS-fn))
  )

;; atom based wf constructor
(defn init-test-wf! [*ui-state & {:keys [actions
                                         init-fn
                                         state-fn
                                         opts-fn ctx-fn steps-fn
                                         run-fn
                                        ]
                                  :or   {actions  {}
                                         init-fn (fn [] {})

                                         state-fn (fn []
                                                    {}
                                                    )
                                         ctx-fn (fn [params]
                                                  {})

                                         steps-fn (fn [params]
                                                    {})

                                         opts-fn (fn [params]
                                                   {})
                                         run-fn (fn[]
                                                  )
                                         }}]
  (let [;;
        wf-params-fn identity
        opt-params-fn identity

        wf (base/parametrized-wf!
             init-fn
             wf-params-fn
             opt-params-fn
             opts-fn
             ctx-fn
             steps-fn
             )
        ]

    (swap! *ui-state merge
           {
            :wf     wf

            ;; for now, not running fn
            :xtor   nil
            :status :compile

            :state  (state-fn)

            :actions {
                      :always  [
                                ["delete wf" (:delete-wf actions)] []
                                ]
                      :init    []

                      :compile [["start ui wf" (:start-wf! actions)]]

                      :running [["send!" (:send-msg! actions)]
                                ["stop wf!" (:stop-wf! actions)]]
                      }
            })

    (run-fn)

    )
  )

;; workflow CRUD impl
(defn delete-wf! [*STATE id]
  (println "deleting wf with id" id)
  (swap! *STATE merge
         {:wfs (vec (filter (fn [a] (not= id (:id a)))
                            (:wfs @*STATE)))}))


;; ui-state workflow fns

(defn start-wf! [*ui-state]
  (let [{wf :wf} @*ui-state]
    ; maybe use different runner for UI
    (base/run-wf! wf identity)))


(defn end-wf! [*ui-state]
  (if-let [xtor (get-in @*ui-state [:xtor])]
    (base/end! xtor)
    (println "no :xtor found in" @*ui-state)))

;; ui

(rum/defc <add-wf> < rum/reactive [*STATE]
  (let [change-status! (fn[status]
                         (swap! *STATE assoc-in [:status] status))

        wfs (:wfs @*STATE)

        id (:id (last wfs))
        idx (dec (count wfs))

        *wf-state (rum/cursor-in *STATE [:wfs idx])
        *ui-state (rum/cursor-in *STATE [:wfs idx :ui])

        ;; note, that these are stateless


        ;; wf specific actions

        send-wf-msg! (fn [*ui-state]
                       ;; apply for state for wf specific params
                       ; (.warn js/console @*ui-state)

                       (go                                  ;; add new :server> step via :ui-chan
                         (async/>! (get-in @*ui-state [:state :ui-loop])
                                   {(base/rand-sid) [:print (str "click - " (.getTime (js/Date.)))]}
                                   )))

        test-wf! (fn []
                   ;; 'close' the actions during wf init
                   (let [actions {
                                  ;; map of available actions
                                  ;; these will be converted to actions per status by fn
                                  :delete-wf (fn []
                                               (when (js/confirm "delete wf?")
                                                 ;; close if running
                                                 (delete-wf! *STATE id)))
                                  :start-wf! (fn [] (start-wf! *ui-state))
                                  :stop-wf!  (fn [] (end-wf! *ui-state))

                                  ;; note, that send-wf-msg! knows about :print step
                                  :send-msg! (fn [] (send-wf-msg! *ui-state))
                                  }


                         ui-loop-init-fn (fn [params]
                                           (println "ui loop init" params)
                                           {
                                            :ui-loop (async/chan) ;; init event loop chan
                                            })

                         print-init-fn (fn [params]
                                         (println "print-init" params)
                                         {
                                          :print-fn (.-warn js/console)
                                          })



                         print-ctx-fn (fn [params]
                                        (println "print-ctx-fn" params)
                                        (let [print! (get params :print-fn prn)]
                                          {:print {
                                                   :fn (fn [a]
                                                         (print! a)
                                                         a)
                                                   }
                                           }))

                         ui-loop-ctx-fn  (fn [& {:keys [] :as params}]

                                           (println "ui-loop-ctx-fn" params)

                                           {
                                            ;; this will add new actions to a wf
                                            :ui-loop {:fn       (fn [in-chan]
                                                                  (u/wiretap-chan in-chan (partial println "UI LOOP:"))
                                                                  )

                                                      :infinite true
                                                      :expands? true
                                                      }
                                            }
                                           )

                         ctx-fn (base/combine-fns [print-ctx-fn (base/arg-fn ui-loop-ctx-fn)])

                         ui-loop-steps-fn (fn [& {:keys [ui-loop]}]
                                            {
                                             ::ui [:ui-loop ui-loop]
                                             }
                                            )
                         print-steps-fn (fn [params]
                                          {
                                           (base/rand-sid) [:print "Hello wf!"]
                                           }
                                          )

                         steps-fn (base/combine-fns [(base/arg-fn ui-loop-steps-fn) print-steps-fn])

                         ui-loop-opts (fn [params]

                                        {:before-process (fn [wf-chan xtor]
                                                           (js-debugger)
                                                           (swap! *ui-state assoc-in [:state :ui-loop]
                                                                  (:ui-loop params))
                                                           )})

                         xtor-opts (fn [params]
                                        {:before-process (fn [wf-chan xtor]
                                                           (js-debugger)

                                                           (swap! *ui-state merge {:xtor   xtor
                                                                                   :status :running
                                                                                   })
                                                           )})


                         OPTS-fn (fn [& {:keys [ui-loop]}]
                                   {
                                    :before-process  (fn [wf-chan xtor]
                                                       (js-debugger)
                                                       (.log js/console "ui wf started"
                                                             @*ui-state
                                                             )

                                                       :ok
                                                       )
                                    :op-handlers-map {
                                                      :process (fn [result]
                                                                 (.log js/console "intermediary results" result)
                                                                 )

                                                      :error   (fn [err]
                                                                 (.error js/console err)

                                                                 (swap! *ui-state assoc-in [:status] :error)
                                                                 )
                                                      :done    (fn [result]
                                                                 (.log js/console "done!" result)

                                                                 (swap! *ui-state assoc-in [:status] :done)
                                                                 (swap! *ui-state assoc-in [:result] result)
                                                                 ;; what to do with results?
                                                                 )
                                                      }
                                    }
                                   )
                         ;; ui-loop-opts xtor-opts
                         opts-fn (base/combine-fns [(base/arg-fn OPTS-fn) ui-loop-opts xtor-opts] :merge-results base/merge-opts-maps)
                         ]

                     ;; combining init funcs
                     ;; pass-through
                     (init-test-wf! *ui-state

                                    :init-fn (base/combine-init-fns [ui-loop-init-fn print-init-fn])
                                    :ctx-fn ctx-fn
                                    :steps-fn steps-fn
                                    :opts-fn opts-fn

                                    :actions actions
                                    :state-fn (fn []
                                                {:initial :state})

                                    :run-fn (fn []
                                              ;; no need to save constructor here, as we save it in :before-process
                                              (base/run-wf! (:wf @*ui-state) identity))
                                    )
                     )
                   )
        ]

    [:div
     [:header "New workflow"]

     [:pre (d/pretty @*wf-state)]

     ; [:pre (pr-str (last (:wfs @*STATE)))]

     (ui/menubar "" [
                     ["test wf init" test-wf!]
                     []

                     ["add not running" (fn []
                                          ;; pass actions here
                                          ;; pass :before-process
                                          ;; or what?
                                          (add-new-wf-2! *ui-state) ;; pass
                                          ; (change-status! :list)
                                          )]
                     []
                     ["add and run" (fn []
                                      (add-new-wf-2! *ui-state) ;; pass

                                      (base/run-wf! (:wf @*ui-state)
                                                    (fn [xtor]
                                                      ;; do we need to save constructor here?
                                                      #_(swap! *UI-WF assoc-in [:ui :xtor] xtor)
                                                      xtor)))]
                     []
                     ["ok" (fn [] (change-status! :list))]
                     ["cancel" (fn []
                                 ;; close if running
                                 (when (js/confirm "delete wf?")
                                   (swap! *STATE merge {:wfs (vec (filter (fn [a]
                                                                            (not= id (:id a)))
                                                                          (:wfs @*STATE)))})
                                   (change-status! :list)
                                   )
                                 )]
                     ])

     ]
    )
  )

(rum/defc <ui> < rum/reactive [*STATE]

  (let [data @*STATE

        ;; ui status of workflow playground
        status (:status data) ; :add|:list
        change-status! (fn[status]
                         (swap! *STATE assoc-in [:status] status))

        ]

    (condp = status
      :add (<add-wf> *STATE)

      :list [:div
             (ui/menubar "Woof" [
                                 ["ping cc" (fn []
                                              (prn "yo")

                                              (let [socket (ws/connect "/cc"
                                                                       :on-open (fn []
                                                                                  (.log js/console "opened")
                                                                                  )
                                                                       :on-message (fn [payload]
                                                                                     (.log js/console "PAYLOAD" payload)
                                                                                     )
                                                                       )]


                                                (js/setTimeout (fn[]
                                                                 (ws/send! socket {
                                                                                   ; (wf/rand-sid) [:test "azaza"]
                                                                                   (wf/rand-sid)
                                                                                     [:run-wf {:id :preview}]
                                                                                    ;[:response "WWWWOOOOOFFFF!!!"]
                                                                                   })
                                                                 ) 1000)
                                                ;; send a message

                                                )

                                              )]

                                 ["new workflow" (fn []
                                          ; append new wf to
                                          (add-new-wf-1! *STATE)
                                          (change-status! :add)
                                          )]])

             (let [wfs (:wfs @*STATE)]
               (if-not (seq wfs)
                 [:div.wf-list
                  [:p "go and add some workflows!"]]
                 (into [:div.wf-list]
                       (map-indexed (fn [i wf-data]
                                      ;; pass the atom or just data will be enough
                                      (<wf-ui> wf-data)) wfs)
                       )
                 )
               )
             ]
      )
    )
  )

(def <app> #(<ui> *UI-STATE))

