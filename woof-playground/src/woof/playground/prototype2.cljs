(ns ^:figwheel-hooks woof.playground.prototype2
  (:require
    [cljs.core.async :as async]
    [cljs.reader]
    [rum.core :as rum]

    [woof.base :as base]
    [woof.core.runner :as runner]
    [woof.u :as u]
    [woof.ui :as ui]
    [woof.playground.common :as cmn]
    [woof.utils :as utils]
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
     ;; workflow model atom
     :wf {

          :status :compile
          ;;:status :result

          :xtor nil

          :init-fn (fn[]
                     (utils/throw! "provide init fn")
                     )
          :init-fn' []

          }

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

(rum/defc <compile> < rum/reactive [wf-state]
  [:div
   [:button {:on-click (fn [e]

                         (swap! *UI-STATE
                                assoc-in [:wf :init-fn']
                                [
                                 (base/stateful-init-fn (atom {:foo :bar}))

                                 (base/meta-init-fn)

                                 (fn [params]
                                   (swap! (:META params) update :IN conj ::data)
                                   (merge
                                     params
                                     {:IN {::data (:foo @(:STATE params))}})
                                   )
                                 ])

                         )}
    "init fn: stateful + meta + IN-OUT"
    ]
   [:button {:on-click (fn [e]
                         (let [fns (:init-fn' wf-state)]
                           (swap! *UI-STATE assoc-in [:wf :init-fn] (base/combine-init-fns-old fns))
                           )
                         )}
    "compile!"
    ]
   ]
  )



;; actionable ui
; provides separate ui for each status + data
(rum/defc action-ui < rum/reactive [ui-map actions-map status DATA]
  (let [ui (get ui-map status
                (fn[status data]
                  [:div "no ui provided for " (str status)
                   [:pre (pr-str data)]
                   ]
                  )
                )
        actions (get actions-map status [])
        ]
    [:div
     (ui/menubar "Actions:" actions)
     (ui status DATA)
     ]
    )
  )

;; for now handle everything in single map
(rum/defc wf-ui < rum/reactive [*UI-WF]
  (let [data @*UI-WF
        ui-data (:ui data)
        status (:status data)

        ;; workflow definition
        wf (:wf data)

        ]
    [:div

     (action-ui {:init (fn [status data]
                         ;; step before ui wf has been initialized
                         [:div
                          "Initialization UI — maybe will be dropped in future."
                          ]
                         )
                 :compile (fn [status data]
                            ; ui for compiling the ui
                            [:div
                             (if-not wf
                               [:div "ui for compilation"]
                               )
                             ]
                            )}
                {:init [["start ui" (fn[]
                                      (let [{wf :wf
                                             xtor :xtor} ui-data

                                            init-fn (fn []
                                                      ;; init event loop chan
                                                      {
                                                       :ui-loop (async/chan)
                                                       }
                                                      )
                                            ;; transforms initial map to a wf params
                                            wf-params-fn identity
                                            ;; transforms wf params to opt params
                                            opt-params-fn identity

                                            ;; provides opts map via opt params (multi-arity)
                                            OPTS-fn (fn [& {:keys [ui-loop]}]
                                                      {
                                                       :before-process  (fn [wf-chan xtor]
                                                                          ;;(swap! *wf assoc :status :running)
                                                                          (.log js/console "ui wf started")

                                                                          ; store ui-loop channel
                                                                          (swap! *UI-WF assoc-in [:ui :ui-loop] ui-loop)

                                                                          :ok
                                                                          )
                                                       :op-handlers-map {
                                                                         :process (fn[result]
                                                                                    (.warn js/console result)
                                                                                    )

                                                                         :done    (fn [result]
                                                                                    #_(let [result-w-meta (with-meta
                                                                                                          (merge
                                                                                                            IN
                                                                                                            result)
                                                                                                          @META)]

                                                                                      (.log js/console "wf done" result-w-meta))
                                                                                    (.log js/console result)
                                                                                    )
                                                                         }
                                                       }
                                                      )
                                            ;; provides context map from wf params (multi-arity)
                                            CTX-fn  (fn [& {:keys [IN META]}]
                                                      {
                                                       :ui-loop {:fn (fn [in-chan]
                                                                       (u/wiretap-chan in-chan (partial println "UI LOOP:"))
                                                                       )

                                                                 :infinite true
                                                                 :expands? true
                                                                 }

                                                       :yo1 {
                                                             :fn (fn [a]
                                                                   (prn a)
                                                                   a)
                                                             }

                                                       :yo   {
                                                              :fn (fn [s]
                                                                    (let [chan (async/chan)]
                                                                      (go
                                                                        (async/<! (utils/timeout 100))
                                                                        (async/>! chan "1")

                                                                        (async/<! (utils/timeout 150))
                                                                        (async/>! chan "2"))
                                                                      chan))

                                                              :infinite true
                                                              ;:expands? true
                                                              }

                                                       }
                                                      )
                                            ;; provides steps map from wf params (multi-arity)
                                            STEPS-fn (fn [& {:keys [ui-loop]}]

                                                       {
                                                        ::ui  [:ui-loop ui-loop]
                                                        ::0 [:yo "hello!"]}
                                                       )

                                            ui-wf (base/parametrized-wf!
                                                    init-fn
                                                    wf-params-fn
                                                    opt-params-fn
                                                    (base/arg-fn OPTS-fn)
                                                    (base/arg-fn CTX-fn)
                                                    (base/arg-fn STEPS-fn)
                                                    )
                                            ]
                                        (swap! *UI-WF assoc-in [:ui :wf]
                                               ui-wf)

                                        (swap! *UI-WF assoc :status :compile)

                                        ; maybe use different runner for UI
                                        (base/run-wf! ui-wf (fn [xtor]
                                                              (swap! *UI-WF
                                                                  assoc-in [:ui :xtor] xtor)
                                                           xtor))

                                        )
                                      )]]
                 :compile [["compile" (fn[]
                                        (prn "yo")
                                        )]
                           ["send!" (fn[]
                                      (go ;; add new :server> step via :ui-chan
                                        (async/>! (get-in data [:ui :ui-loop])
                                                  {(base/rand-sid) [:yo1 (str "click - " (.getTime (js/Date.)))]}
                                                  )))]
                           ]
                 :running [["end!" (fn []
                                     (prn "stop the ui")

                                     (base/end! (get-in data [:ui :xtor]))
                                     )]]
                 }
                status
                data
                )
     ]
    )

  )

(rum/defc <ui> < rum/reactive [*STATE]

  (let [data (:wf @*STATE)]

    [:div
     (wf-ui (rum/cursor-in *STATE [:ui-wf]))

     [:div

      [:ul
       [:li "compile"
        ]
       (<compile> data)

       [:li "run"]


       [:button {:on-click
                 (fn [e]
                   (let [;; returns initial map
                         init-fn (:init-fn data)
                         wf-params-fn identity  ;; transforms initial map to a wf params
                         opt-params-fn identity ;; transforms wf params to opt params

                         ;; provides opts map via opt params (multi-arity)
                         OPTS-fn (fn [& {:keys [IN META]}]
                                   {
                                    :before-process  (fn [wf-chan xtor]
                                                       ;;(swap! *wf assoc :status :running)
                                                       (.log js/console "wf started")

                                                       (swap! *UI-STATE
                                                              assoc-in [:wf :status] :running)

                                                       :ok
                                                       )
                                    :op-handlers-map {
                                                      :process (fn[result]
                                                                 (.warn js/console result)
                                                                 )

                                                      :done    (fn [result]
                                                                 (let [result-w-meta (with-meta
                                                                                       (merge
                                                                                         IN
                                                                                         result)
                                                                                       @META)]

                                                                   (.log js/console "wf done" result-w-meta))
                                                                 )
                                                      }
                                    }
                                   )
                         ;; provides context map from wf params (multi-arity)
                         CTX-fn  (fn [& {:keys [IN META]}]
                                   {
                                    :yo1 {
                                          :fn (fn [a] a)
                                          }

                                    :yo   {
                                           :fn (fn [s]
                                                 (let [chan (async/chan)]
                                                   (go
                                                     (async/<! (utils/timeout 100))
                                                     (async/>! chan "1")

                                                     (async/<! (utils/timeout 150))
                                                     (async/>! chan "2"))
                                                   chan))

                                           :infinite true
                                           ;:expands? true
                                           }

                                    }
                                   )
                         ;; provides steps map from wf params (multi-arity)
                         STEPS-fn (fn [& {:keys [IN META]}]
                                    (let [steps {
                                                 ::0 [:yo (::data IN)]
                                                 }]
                                      (swap! META update :OUT conj ::0)
                                      steps)
                                    )

                         wf (base/parametrized-wf!
                              init-fn
                              wf-params-fn
                              opt-params-fn
                              (base/arg-fn OPTS-fn)
                              (base/arg-fn CTX-fn)
                              (base/arg-fn STEPS-fn)
                              )]

                     (base/run-wf! wf (fn [xtor]
                                        (swap! *UI-STATE
                                               assoc-in [:wf :xtor] xtor)
                                        xtor
                                        ))
                     )
                   )}
        "run!"]
       [:button {:on-click (fn[e]
                             (base/end! (:xtor data)))}
        "stop!"
        ]

       [:li "results"]
       ]


      ;"Hello, woof!"
      [:code (pr-str data)]

      ;;(<wf> (rum/cursor-in *STATE [:wf]))
      ]
     ]

    )


  )

(def <app> #(<ui> *UI-STATE))

