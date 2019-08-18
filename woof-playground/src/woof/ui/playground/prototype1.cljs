(ns ^:figwheel-hooks woof.ui.playground.prototype1
  (:require
    [cljs.reader]
    [rum.core :as rum]

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

          :RESULT (with-meta
                    {
                     ::in-step "woof"
                     ::out-step "hello 'woof'!"
                     }
                    {:IN #{::in-step}
                     :OUT #{::out-step}}
                    )
          }

     }))


;; --- exports

(declare <ui>)
(declare init!)

(declare reload!)



(defn init!  ;; ;; todo: re-implement as subscription
  "initializes ui state"
  ([mount-fn]
   (println "MOUNT")
   (add-watch *UI-STATE :woof-main
              (fn [key atom old-state new-state]
                (mount-fn)))

   (when-not (::initialized @*UI-STATE)
     (swap! *UI-STATE merge
            {::initialized true})))
  )


(defn reload! []
  (swap! *UI-STATE merge {
                          ::initialized false
                          }))



;; prototype


;; main ui



;;

;; take a workflow
;;  in-fn + context-fn + steps-fn + opts-fn

;; and adjust (utochnu, specify?) it for specific case

;; like for IN-OUT wfs


;;  <1>            <2>               <3>
;; init-fn => wf-fn.params => opts-fn.params
;;                 <4>
;;                 .wf => <wf> => runner


;; simplest version of wf definition:
;;   everything is hardcoded inside

(defn simple-wf! []
  {
   :init-fn (fn []
              ; <1>
              ; initial params
              {::init-fn :init-fn}
              )
   :wf-fn   (fn [params]
              ; <2>
              ;; init-fn => params

              {
               :wf     (fn [wf-params]
                         ; <4>
                         ;;
                         (reify WoofWorkflow
                           (get-params [this]
                             ;; returns initial wf params
                             {})
                           (get-context-map [this]
                             {
                              :yo {
                                   :fn (fn [a] "yo!")
                                   }
                              })
                           (get-steps [this] {::0 [:yo nil]}))
                         )

               :params (assoc params ::wf-fn :wf-fn)
               }
              )
   :opts-fn (fn [opts-params]
              ; <3>
              ;; wf-fn.params => opts-params
              {
               :params (assoc opts-params ::opts-fn :opts-fn)
               :opts   {
                        :before-process  (fn [wf-chan xtor]
                                           ;;(swap! *wf assoc :status :running)
                                           (.log js/console "wf started")

                                           :ok
                                           )
                        :op-handlers-map {
                                          :process (fn [interm-result]
                                                     ; (js-debugger)
                                                     ;; how to find which are working
                                                     )
                                          :done    (fn [result]
                                                     (.log js/console "wf done" result)
                                                     )
                                          }

                        }
               }
              )
   }
  )

;; wf with hardcoded IN-OUT handling

(defn meta-wf! []
  (let [; <1> initial params
        init-fn (fn []
                  (let [IN {::data "woof"}

                        *META (atom {:IN  #{::data}
                                    :OUT #{}})]

                    {
                     ;; pass the in data for wf
                     ::IN IN

                     ;; pass additional stuff, like meta
                     ::META *META
                     }
                    )
                  )
        ; <2> init-fn => params
        wf-fn (fn [params]
                (let [*META (::META params)]
                  {
                   :wf     (fn [wf-params]
                             ; <4>
                             ;;
                             (reify WoofWorkflow
                               ;; returns initial wf params
                               (get-params [this] {})
                               (get-context-map [this]
                                 ;; todo: if we add expand steps - we can add them to meta too
                                 {
                                  :yo {
                                       :fn (fn [a] a)
                                       }
                                  })

                               (get-steps [this]
                                 (let [IN (::IN wf-params)
                                       steps {
                                              ::0 [:yo IN]
                                              }]
                                   (swap! *META update :OUT conj ::0)

                                   steps)
                                 ))
                             )
                   :params (assoc params ::wf-fn :wf-fn)
                   }
                  )
                )
        ; <3> wf-fn.params => opts-params
        opts-fn (fn [opts-params]
                  (let [*META (::META opts-params)]
                    {
                     :params (assoc opts-params ::opts-fn :opts-fn)
                     :opts   {
                              :before-process  (fn [wf-chan xtor]
                                                 ;;(swap! *wf assoc :status :running)
                                                 (.log js/console "wf started")

                                                 :ok
                                                 )
                              :op-handlers-map {
                                                :process (fn [interm-result]
                                                           ;; how to find which are working
                                                           )
                                                :done    (fn [result]
                                                           (let [result-w-meta (with-meta
                                                                                 (merge
                                                                                   (::IN opts-params)
                                                                                   result)
                                                                                 @*META)]

                                                             (.log js/console "wf done" result-w-meta))
                                                           )
                                                }
                              }
                     })
                  )
        ]
    {
     :init-fn init-fn
     :wf-fn   wf-fn
     :opts-fn opts-fn
     }
    )
  )





;; slightly parametrized version of IN-OUT

(defn meta-wf-1! [IN-fn CTX-fn STEPS-fn RESULT-fn]
  (let [; <1> initial params
        init-fn (fn []
                  (let [*META (atom {:IN  #{}
                                     :OUT #{}})
                        IN (IN-fn *META)]
                    {
                     ;; pass the in data for wf
                     ::IN IN

                     ;; pass additional stuff, like meta
                     ::META *META
                     }
                    )
                  )
        ; <2> init-fn => params
        wf-fn (fn [params]
                (let [*META (::META params)]
                  {
                   :wf     (fn [wf-params]
                             ; <4>
                             ;;
                             (reify WoofWorkflow
                               ;; returns initial wf params
                               (get-params [this]
                                 (::IN wf-params))

                               (get-context-map [this]
                                 ;; todo: if we add expand steps - we can add them to meta too
                                 (CTX-fn *META (::IN wf-params)))
                               (get-steps [this]
                                 (STEPS-fn *META (::IN wf-params))))
                             )
                   ;; what if we'll have to return more stuff in params, like channels
                   :params (assoc params ::wf-fn :wf-fn)
                   }
                  )
                )
        ; <3> wf-fn.params => opts-params
        opts-fn (fn [opts-params]
                  (let [*META (::META opts-params)]
                    {
                     :params (assoc opts-params ::opts-fn :opts-fn)
                     :opts   {
                              :before-process  (fn [wf-chan xtor]
                                                 ;;(swap! *wf assoc :status :running)
                                                 (.log js/console "wf started")

                                                 :ok
                                                 )
                              :op-handlers-map {
                                                :process (fn [interm-result]
                                                           ;; how to find which are working
                                                           )
                                                :done    (fn [result]
                                                           (RESULT-fn *META (::IN opts-params) result))
                                                }
                              }
                     })
                  )
        ]
    {
     :init-fn init-fn
     :wf-fn   wf-fn
     :opts-fn opts-fn
     }
    )
  )


;; fully parametrized version of wf definition




(defn parametrized-wf! [init-fn        ;; returns initial maps
                        wf-params-fn   ;; transforms initial map to a wf params
                        opt-params-fn  ;; transforms wf params to opt params
                        opts-fn        ;; provides opts map via opt params
                        context-map-fn ;; provides context map from wf params
                        steps-fn       ;; provides steps map from wf params
                        ]
  (let [wf-fn (fn [params]
                (let [nu-params (wf-params-fn params)]
                  {
                   :params nu-params
                   :wf     (fn [wf-params]
                             (reify WoofWorkflow
                               (get-params [this] nu-params) ;; is this really needed
                               (get-context-map [this] (context-map-fn nu-params))
                               (get-steps [this] (steps-fn nu-params)))
                             )
                   }
                  )
                )
        opts-fn (fn [opts-params]
                  (let [nu-opt-params (opt-params-fn opts-params)]
                    {
                     :params nu-opt-params
                     :opts   (opts-fn nu-opt-params)
                     })
                  )
        ]
    {
     :init-fn init-fn
     :wf-fn   wf-fn
     :opts-fn opts-fn
     }
    )
  )

(defn parametrized-wf-1! [&]

  )

(defn run-wf! [wf]
  (let [channel (runner/run-wf
                  (:init-fn wf)
                  (:wf-fn wf)   ;; {:params {..}, :wf <wf-xtor>}
                  (:opts-fn wf) ;; {:params {..}, :opts {..}}
                  runner/default-run-fn)
        ]

    ; (println "z" channel)

    )
  )


(defn arg-fn [f]
  (fn [params]
    (let [args (apply concat params)]
      (apply f args)
      )))


;; the idea with parametrized wf - is that we can combine inner wf functions

;; for example:
;; init-fn

;; init-fn that exposes global state

(rum/defc <wf> < rum/reactive [*wf]
  [:div

   [:button {:on-click
             (fn [e]
               (let [init-1 (fn [params]
                              ;(js-debugger)
                              (merge
                                params
                                {:STATE (atom {:hello "woof!"})})
                              )
                     init-meta (fn [params]
                                 ;(js-debugger)
                                 (merge params
                                        {:META (atom {:IN  #{}
                                                      :OUT #{}})})
                                 )
                     init-data (fn [params]
                                 ;(js-debugger)
                                 (swap! (:META params) update :IN conj ::data)
                                 (merge
                                   params
                                   {:IN {::data (:hello @(:STATE params))}})
                                 )

                     combined-init (fn []
                                     (let [initial {}]
                                       (init-data
                                         (init-meta
                                           (init-1 initial)))
                                       )
                                     )

                     combined-init1 (apply comp
                                      [init-data
                                      init-meta
                                      init-1
                                      (fn[] {})])
                     ]
                 ;(.log js/console (combined-init))
                 (.log js/console (combined-init1))
                 )
               )}
    "combine test"
    ]

   [:button {:on-click
             (fn [e]
               (let [;; returns initial map
                     init-fn (fn []
                               (let [*META (atom {:IN  #{}
                                                  :OUT #{}})
                                     IN {::data "woof"}]

                                 (swap! *META update :IN conj ::data)

                                 {
                                  ;; pass the in data for wf
                                  :IN IN

                                  ;; pass additional stuff, like meta
                                  :META *META
                                  }
                                 )
                               )
                     wf-params-fn identity  ;; transforms initial map to a wf params
                     opt-params-fn identity ;; transforms wf params to opt params

                     ;; provides opts map via opt params (multi-arity)


                     OPTS-fn (fn [& {:keys [IN META]}]
                               {
                                :before-process  (fn [wf-chan xtor]
                                                   ;;(swap! *wf assoc :status :running)
                                                   (.log js/console "wf started")

                                                   :ok
                                                   )
                                :op-handlers-map {
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
                                :yo {
                                     :fn (fn [a] a)
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

                     wf (parametrized-wf!
                          init-fn
                          wf-params-fn
                          opt-params-fn
                          (arg-fn OPTS-fn)
                          (arg-fn CTX-fn)
                          (arg-fn STEPS-fn)
                          )]
                 (run-wf! wf)
                 )
                         )}
    "run!"]])


#_(rum/defc <wf> < rum/reactive [*wf]
  [:div
   [:button {:on-click (fn [e]
                         (let [

                               in-fn (fn  [*META]
                                 (swap! *META update :IN conj ::data)

                                 {::data "woof"})

                               ctx-fn (fn [*META IN]
                                 {
                                  :yo {
                                       :fn (fn [a] a)
                                       }
                                  }
                                 )

                               steps-fn (fn [*META IN]
                                 (let [steps {
                                              ::0 [:yo (::data IN)]
                                              }]
                                   (swap! *META update :OUT conj ::0)
                                   steps))


                               result-fn (fn [*META IN result]
                                 (let [result-w-meta (with-meta
                                                       (merge
                                                         IN
                                                         result)
                                                       @*META)]

                                   (.log js/console "wf done" result-w-meta))
                                 )

                               wf (meta-wf-1!
                                    in-fn
                                    ctx-fn
                                    steps-fn
                                    result-fn)
                               ;wf (meta-wf!)
                               ;wf (simple-wf!)
                               ]
                           (run-wf! wf)
                           )

                         )} "run!"]
   ]
  )


(defn in-out-wf! [in-fn
                  context-fn
                  steps-fn
                  opts-fn]

  (let [META (atom {:IN  #{}
                    :OUT #{}
                    })



        merge-meta! (fn [r]
                      (if-let [m (meta r)]
                        (swap! META (fn [a b]
                                      (assoc a
                                        :IN (clojure.set/union (:IN a) (get b :IN #{}))
                                        :OUT (clojure.set/union (:OUT a) (get b :OUT #{}))
                                        )
                                      m))
                        (utils/throw! (str "No metadata attached to " (pr-str r)))))


        ;; wraps IN keys from resulting IN
        wrapped-init-fn (fn []
                          (let [IN (in-fn)]
                            (merge-meta! IN)

                            ; or always use all in keys
                            ;(swap! META update :IN into (keys IN))
                            IN))

        wf-xtor (fn [initial-params params]


                  (.log js/console "initial" initial-params)

                  ;; <?> we can store initial params here

                  ;; WoofWorkflow
                  (wfc/params-wf
                    params context-fn
                    (fn [& r]
                      ;; IN => steps+metadata

                      (let [steps (apply steps-fn r)]
                        (merge-meta! steps)

                        steps
                        )
                      )

                    ))

        wf-fn (fn [initial-params]
                (let [params (merge initial-params {:ggg :aaa})]
                  {
                   ;; wf constructor
                   :wf     (partial wf-xtor initial-params)
                   :params params
                   }
                  )
                )



        wrapped-opts-fn (fn [params]

                          (.log js/console "opts" params)

                          (let [outer-opts (opts-fn params)

                                outer-done (get-in outer-opts [:opts :op-handlers-map :done] (fn [result]))
                                ]

                            (assoc-in outer-opts [:opts :op-handlers-map :done]
                                      (fn [result]
                                        (outer-done
                                          (with-meta
                                            result ;(merge IN result)
                                            @META))))
                            )
                          )
        ]

    (let [runner-fn runner/default-run-fn

          channel (runner/run-wf
                    wrapped-init-fn ;; defaults
                    wf-fn   ;; (fn [params] -> {:wf <wf>, :params {}})
                    wrapped-opts-fn

                    runner-fn)
          ]

      ; (println "z" channel)

      )
    )
  )


#_(rum/defc <wf> < rum/reactive [*wf]

  (let [wf @*wf
        status (:status wf)
        ]
    [:div
     [:pre (pr-str wf)]

     (if (= :compile status)
       [:div
        [:button {:on-click (fn [e]

                              (in-out-wf!
                                (fn []

                                  ;; store initial value in wf

                                  (with-meta
                                    {:data "woof"}
                                    {:IN #{:data}})
                                  )


                                ;; ctx fn
                                (fn [& r]
                                  ;; meta stuff should be wrapped inside
                                  {
                                   :yo {:fn (fn [a]
                                              "yo!"
                                              )}
                                   :yo-async {:fn (fn [a]
                                                    (let [c (async/chan)]
                                                      (go
                                                        (async/<! (utils/timeout 3000))
                                                        (async/put! c a))
                                                      c))
                                                                                                        }
                                   }
                                  )

                                ;; steps fn
                                (fn [& r]

                                  ;; todo: what if don't know which steps will be resulting
                                  ;; maybe via :op-handlers-map :done
                                  ;; but it doesn't change the data

                                  ;; todo: what if we return :IN here
                                  ;; how to merge it
                                  (with-meta
                                    {
                                     ::0 [:yo-async "!!!"]
                                     }
                                    {:OUT #{::0}})
                                  )

                                (fn [params]
                                  {:params params
                                   :opts   {
                                            :before-process  (fn [wf-chan xtor]
                                                               (swap! *wf assoc :status :running)
                                                               (.log js/console "external opts start")

                                                               :ok
                                                               )
                                            :op-handlers-map {
                                                              :process (fn [interm-result]
                                                                         ; (js-debugger)
                                                                         ;; how to find which are working
                                                                         )
                                                              :done (fn [result]

                                                                      (swap! *wf assoc
                                                                              :status :result
                                                                              :RESULT result)

                                                                      ;; assoc previously saved initial here

                                                                      (.log js/console "rrr" result)
                                                                      )
                                                              }

                                            }
                                   }
                                  )
                                )

                              )}
         "run!"
         ]
        ]
       )

     (if (= :running status)
       [:div "..."]
       )

     (if (= :result status)
       (let [result (:RESULT wf)]
         [:div
          [:button {:on-click (fn [e]
                                (swap! *wf assoc :status :compile
                                                 :RESULT nil)
                                )} "ok"]
          [:header "data"]
          [:pre (d/pretty result)]
          [:header "meta"]
          [:pre (d/pretty (meta result))]
          ]
         )


       )
     ]

    )
  )

(rum/defc <ui> < rum/reactive [*STATE]

  [:div
    [:pre (pr-str @*STATE)]
    [:hr]
    (<wf> (rum/cursor-in *STATE [:wf]))
   ]

  )

(def <app> #(<ui> *UI-STATE))

