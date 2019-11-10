(ns ^:figwheel-hooks woof.playground.prototype4
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]

    [woof.base :as base]
    [woof.playground.common :as cmn]
    [woof.playground.v1.ui :as ui]
    [woof.utils :as utils]
    [woof.wf :as wf]
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

     :test {
            ::status :ready
            ::header "Test Workflow UI"
            }

     ;; current wf state
     :wf {
          ::status :init ;;:compile ;; :running

          ::header "Workflow UI"

          ::actions []

          }

     :init-wf {
               ::status :init ;;:compile ;; :running

               ::header "Build your workflow"

               ::actions []

               ::available-init-fns [
                                     ]
               ::available-ctx-fns [

                                    ]

               ::available-steps-fns []

               ::wf {
                     ::status :init
                     ::header ""

                     ::actions []
                     }

               }

     }))


;; --- exports

(declare <ui>)

(def init! (partial cmn/default-init! *UI-STATE))
(def reload! (partial cmn/default-reload! *UI-STATE))


; {::header "" ::status .. ::actions}

(rum/defc <generic-ui> < rum/reactive [*STATE actions-map <ui>]
  (let [{status ::status
         header ::header
         } @*STATE]
    (conj
      [:div {:style {:outline "1px solid rgba(0,255,255,.133)"
                     :padding ".5rem"}}
       [:div {:style {:margin-bottom ".5rem"}}
        [:span {:style {:margin-right ".5rem"}} (str status) ]
        (ui/menubar header (get actions-map status []))
        ]
       ]
      (<ui>))
    ))


(rum/defc <sample-ui> < rum/reactive [*STATE]
  [:div
   "play with the buttons"]
  )



(defn keep-xtor-ref [*wf params]
  {
   :before-process  (fn [wf-chan xtor]
                      (swap! *wf assoc ::xtor xtor)
                      :ok)

   :op-handlers-map {

                     :process (fn [result]
                                (swap! *wf assoc ::result result)
                                (.log js/console result)
                                )
                     :error   (fn [result]
                                (swap! *wf assoc ::status :error)
                                (.error js/console result)
                                )
                     :done    (fn [result]
                                (swap! *wf assoc ::result result)
                                (swap! *wf assoc ::status :done)
                                (.log js/console "done" result))
                     }

   })


(defn init-ui-wf [*wf
           init-fns
           ctx-fns
           steps-fns
           ;; opts-fn
           & {:keys [auto-start?]
              :or {auto-start? true}}
           ]

  ;; for now
    ;; store initial params
    ;; store wf xtor
    ;; store intermediary results
    ;; store final results

  (let [wf-params-fn identity  ;; transforms initial map to a wf params
        ; latest fn that returns params
        opt-params-fn (fn [params]
                        ;;(.log js/console "params" params)

                        (swap! *wf assoc ::params params)
                        params
                        ) ;; transforms wf params to opt params


        opt-fns [(partial keep-xtor-ref *wf)]        ;; provides opts map via opt params

        wf (base/parametrized-wf!
             (base/combine-init-fns init-fns)
             wf-params-fn
             opt-params-fn

             (base/combine-fns opt-fns :merge-results base/merge-opts-maps)
             (base/combine-fns ctx-fns)
             (base/combine-fns steps-fns))]

    ; store wf
    (swap! *wf assoc ::wf wf)

    (when auto-start?
      ;; start wf
      (base/run-wf! wf identity)
      (swap! *wf assoc ::status :running)
      )
    )
  )

;; just display the results
(rum/defc <wf-ui> < rum/reactive [*wf]
  (let [wf @*wf
        result (::result wf)
        status (::status wf)
        ]

    (if (= :init status)
      [:div
       "wf is not running."
       ]
      [:div
       [:header "results"]
       (pr-str result)
       [:pre (pr-str @*wf)]
       ]
      )
    ))


(rum/defc <list-item> < rum/static [cfg item]
  (let [selected ((:selected cfg) item)]
    [:li
     {:class    (when selected "selected")
      :on-click (fn [_]
                  (let [id (:id cfg)
                        chan (:chan cfg)]
                    (go
                      (async/put! chan {(wf/rand-sid) [(if selected :li-deselect :li-select) [id item]]}))

                    )

                  )}
     (pr-str item)
     ;(pr-str ((:selected cfg) item))
     ]
)

  )

(rum/defc <list-box> < rum/static [list-cfg]
  (let [items (:v list-cfg) ]
    [:ui.listbox

     (map (partial <list-item> list-cfg) items)
   ]))



  (rum/defc <ui> < rum/reactive [*STATE]
  [:div
   [:p "workflow playground"]


   ;; minimal ui - no wf yet
   (let [*no-wf (rum/cursor *STATE :test)]
     [:div
      [:p "generic state-based ui - no wf yet"]
      (<generic-ui> *no-wf {:ready [["push me!" (fn[] (prn "and then just touch me"))]]}
                    (partial <sample-ui> *no-wf))
      ]
     )


   #_(let [*wf (rum/cursor *STATE :wf)

         run-wf! (fn []
           (let [init-fns [(fn [params]
                             {::t 3000}
                             )]
                 ctx-fns [(fn [params]
                            {
                             :hello {:fn (fn [a]
                                           (prn a)
                                           a)
                                     }
                             :long-hello {:fn (fn [a]
                                                (prn params)
                                                (let [ch (async/chan)]
                                                  (go
                                                    (async/<! (utils/timeout (::t params)))
                                                    (async/>! ch a)
                                                    )
                                                  ch
                                                  )
                                                )}
                             }
                            )] ;; provides context map from wf params
                 steps-fns [(fn [params]
                              {::hello [:hello "Hello!"]
                               ::long-hello [:long-hello "Yo!"]
                               }
                              )]       ;; provides steps map from wf params
                 ]
             (init-ui-wf *wf
                         init-fns
                         ctx-fns
                         steps-fns
                         :auto-start? true
                         )
             )

           )
         stop-wf! (fn []
                    (if-let [xtor (::xtor @*wf)]
                      (base/end! xtor)
                      (println "no :xtor found")
                      )
                    )

         re-run-wf! (fn[]
                      (swap! *wf merge {::result nil
                                        ::xtor nil})
                      (run-wf!)
                      )
         ]

     [:div
      [:p "state-based wf ui. with generic workflow ui"]
      (<generic-ui> *wf
                       {:init [["run" run-wf!]]
                        :running [["stop" stop-wf!]]
                        :done  [["re-run" re-run-wf!]]}
                       (partial <wf-ui> *wf))
      ]
     )

   (let [*wf (rum/cursor *STATE :init-wf)

         run-wf! (fn []
                   (let [init-fns [
                                   ;; why bottom up
                                   (fn [params]
                                     ;; todo: migrate to a separate opts-fn
                                     (swap! *wf assoc ::t (utils/now)
                                            ::ui-state {
                                                        ::all-init-fns {
                                                                    :id ::all-init-fns
                                                                    ;; for now, allow to send new actions directly
                                                                    :chan (::loop params)
                                                                    :selected #{}
                                                                    :v [
                                                                        "a"
                                                                        "b"
                                                                        ]
                                                                    }

                                                        ::init-fns {
                                                                    :id ::init-fns
                                                                    ;; for now, allow to send new actions directly
                                                                    :chan (::loop params)
                                                                    :selected #{}
                                                                    :v []
                                                                    }
                                                        }
                                            )

                                     ;; init functions -> key-value

                                     {}
                                     )

                                   (fn [params]

                                     {::t 3000}
                                     )
                                   (fn [params]
                                     {::loop (async/chan)
                                      }
                                     )

                                   ]
                         loop-ctx-fn (fn [params]
                                       {:loop {
                                               :fn       (fn [loop-chan]
                                                           ;; (u/wiretap-chan in-chan (partial println "UI LOOP:"))
                                                           loop-chan)

                                               :infinite true
                                               :expands? true
                                               }
                                        }
                                       )
                         ctx-fns [loop-ctx-fn
                                  (fn [params]
                                    {
                                     :hello {:fn (fn [a]
                                                   (prn a)
                                                   a)
                                             }
                                     :long-hello {:fn (fn [a]
                                                        (prn params)
                                                        (let [ch (async/chan)]
                                                          (go
                                                            (async/<! (utils/timeout (::t params)))
                                                            (async/>! ch a)
                                                            )
                                                          ch
                                                          )
                                                        )}
                                     }
                                    )

                                  (fn [params]
                                    {

                                     :li-select {
                                                 :fn (fn [[id v]]
                                                       (prn "selecting")
                                                       (swap! *wf update-in [::ui-state id :selected] conj v))

                                                 }

                                     :li-deselect {
                                                   :fn (fn [[id v]]
                                                         (prn "deselecting")
                                                         (swap! *wf update-in [::ui-state id :selected] disj v)
                                                         )
                                                   }

                                     ;; handle select as separate event
                                     :li-click {
                                                :fn (fn [[id v]]

                                                      (let [cfg (get-in @*wf [::ui-state id])
                                                            selected (:selected cfg)]

                                                        (if (selected v)
                                                          (do
                                                            (prn "deselecting")
                                                            (swap! *wf update-in [::ui-state id :selected] disj v)
                                                            )

                                                          (do
                                                            (prn "selecting")
                                                            (swap! *wf update-in [::ui-state id :selected] conj v)
                                                            )

                                                          )

                                                        )


                                                      ;;::ui-state
                                                      ;; (u/wiretap-chan in-chan (partial println "UI LOOP:"))
                                                      )
                                                }
                                     }
                                    )
                                  ] ;; provides context map from wf params
                         steps-fns [(fn [params]
                                      {
                                       ::loop [:loop (::loop params)]


                                       ::hello [:hello "Hello!"]
                                       ::long-hello [:long-hello "Yo!"]
                                       }
                                      )]       ;; provides steps map from wf params
                         ]
                     (init-ui-wf *wf
                                 init-fns
                                 ctx-fns
                                 steps-fns
                                 :auto-start? true
                                 )
                     )

                   )
         stop-wf! (fn []
                    (if-let [xtor (::xtor @*wf)]
                      (base/end! xtor)
                      (println "no :xtor found")
                      )
                    )

         re-run-wf! (fn[]
                      (swap! *wf merge {::result nil
                                        ::xtor nil})
                      (run-wf!)
                      )
         ]

     [:div
      [:p "generic ui for init"]
      (<generic-ui> *wf
                    {:init [["run" run-wf!]]
                     :running [["stop" stop-wf!]
                               ["add event!" (fn []
                                               (let [in-params (::params @*wf)
                                                     loop (::loop in-params)
                                                     ]

                                                 (go
                                                   (async/put! loop {(wf/rand-sid) [:hello (utils/now)]})
                                                   )
                                                 ;(prn (::params @*wf))
                                                 )

                                               )]
                               ]
                     :done  [["re-run" re-run-wf!]]}
                    ;(partial <wf-ui> *wf)
                    (fn []
                      (let [wf @*wf
                            state (::ui-state wf)

                             ]
                        [:div
                         [:header "init-fns"]
                         ;(pr-str (::available-init-fns wf))
                         (pr-str (::t wf))

                         [:div {:style {:display "flex"}}

                          (<list-box> (get state ::all-init-fns))

                          [:div
                           (ui/btn "->" (fn[]

                                          (let [selected (get-in state [::all-init-fns :selected])]
                                            ;(prn (get-in state [::all-init-fns :selected]) )
                                            (swap! *wf update-in [::ui-state ::init-fns :v] conj selected)
                                            )


                                          ))
                           (ui/btn "<-" (fn[]))
                           ]

                          (<list-box> (get state ::init-fns))

                          ]

                         ;"yooo!"
                         ]
                        )

                      )
                    )
      ]
     )
   ]
  )

;; root ui component for prototype
(def <app> #(<ui> *UI-STATE))
