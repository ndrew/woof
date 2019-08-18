(ns ^:figwheel-hooks woof.ui.playground.prototype6
  (:require
    [rum.core :as rum]

    [woof.ui :as ui]
    [woof.u :as u]
    [woof.data :as d]

    ;; ns for running wfs

    ; internal

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
    [woof.wf :as wf])

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; -------------------------- ui-state


(declare *UI-STATE)


;; --- exports

(declare <ui>)
(declare init!)
(declare reload!)


(defn init!  ;; ;; todo: re-implement as subscription
  "initializes ui state"
  ([mount-fn]
   ;(println "MOUNT")
   (add-watch *UI-STATE :woof-main
              (fn [key atom old-state new-state]
                (mount-fn)))

   (when-not (::initialized @*UI-STATE)
     (swap! *UI-STATE merge
            {::initialized true})))
  )


(defn reload! []
  (swap! *UI-STATE merge { ::initialized false }))


;; -------

;; ui workflow example
;; like UI component that can be extended with wf


;; the ui component will be a list box with a details view on the right
;;
;; list box data source
;;   ? infinite?
;;   ? expand?
;;

(defonce *UI-STATE (atom
                     {
                      ;; wf state atom per each key
                      :ui-wf {

                              }

                      :static-wf {

                                  }

                      }))



(defonce *TEST-DATA
         (atom { :posts [
                         {:header "post1"
                          :details "POST 1"}

                         {:header "post2"
                          :details "POST 2"
                          }] }))

;; generic wf stuff

(defn keep-xtor-ref [*wf params]
  {
   :before-process  (fn [wf-chan xtor]
                      (swap! *wf assoc ::xtor xtor)
                      :ok)

   :op-handlers-map {

                     :process (fn [result]
                                (swap! *wf assoc ::result result)

                                ;;
                                ;; (.log js/console result)

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

;; 1. create running ui wf with ui loop


(defn ui-wf [*wf
             init-fns
             ctx-fns
             steps-fns]
  (let [wf-params-fn identity  ;; transforms initial map to a wf params
        ; latest fn that returns params
        opt-params-fn (fn [params]
                        ;;(.log js/console "params" params)

                        ;(swap! *wf assoc ::params params)
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
    (swap! *wf merge wf)
    (swap! *wf assoc ::status :running)
    (base/run-wf! wf identity)
    )
  )

(defn init-ui-wf [*wf]
  (ui-wf *wf
         ; init
         [
          (fn [params]
            ;; save params
            (swap! *wf assoc ::params params)
            (swap! *wf assoc ::ui {
                                   ::list []
                                   ::details {}
                                   ::actions []
                                   })
            params
            )
          (fn [params]
            {::loop (async/chan)})

          ]
         ; ctx
         [(fn [params]
            {
             :loop {
                    :fn       (fn [loop-chan]
                                ;; (u/wiretap-chan in-chan (partial println "UI LOOP:"))
                                loop-chan)

                    :infinite true
                    :expands? true
                    }

             :posts-as-list {
                             :fn (fn [cfg]
                                   (get-in @*TEST-DATA [:posts]))
                             }

             :posts* {
                      :fn (fn [_]
                            (let [posts (get-in @*TEST-DATA [:posts])]
                              (reduce (fn [a p] (assoc a (wf/rand-sid "posts*-") [:post p]))
                                      {} posts)))
                      :expands? true
                      }

             :post {
                    :fn (fn [p]
                          p
                          )
                    }


             :ui-posts* {:fn (fn [p]
                               (map (fn [post] (:header post)) p))
                        :collect? true}

             :ui-posts-ids {:fn (fn [p] p)
                            }

             :hello {
                     :fn (fn [v]
                           (prn v)
                           )
                     }
             }
            )
          (fn [params]
            (let [*ui (rum/cursor-in *wf [::ui])]
              {
               :ui-list {
                         :fn (fn [v]
                               (swap! *ui assoc ::list v)
                               ;; details?
                               )
                         }
               :ui-details {
                            :fn (fn [v]
                                  (swap! *ui assoc ::details v)
                                  )
                            }
               }
              )

            )
          ]
         ; steps
         [(fn [params]
            {
             ::loop [:loop (::loop params)]

             ::posts [:posts-as-list {}]

             ::ui-posts [:ui-list ::posts]

             ;::all-posts [:posts* {}]
             ;::posts-sids [:ui-posts-ids ::all-posts]
             ;::ui-posts [:ui-posts* ::all-posts]



             })]
         )
  )


(rum/defc <wf-ui> < rum/reactive [*wf]
  (let [wf @*wf
        status (get wf ::status :not-running)
        loop-chan (get-in wf [::params ::loop])
        ui-state (get-in wf [::ui] {})
        ]
    [:div
      [:header
       (if (= :not-running status)
         (ui/btn "run" (fn []))
         )

       (if (= :running status)
         (ui/btn "stop" (fn []
                    (if-let [xtor (::xtor @*wf)]
                      (base/end! xtor)
                      (println "no :xtor found"))
                    ))
         )
       ]

     ;[:pre (d/pretty (get wf ::params))]
     (pr-str status)

     (if (= :running status)
     [:div
      #_(into [:select]
            (map (fn[a]
                   [:option (pr-str a)]
                   ) (::list ui-state))
            )

      ;; try
      (into [:ul]
            (map (fn[a]
                   [:li (pr-str a)])
                 (::list ui-state)))

      #_(ui/btn "test" (fn []
                       ;loop-chan
                       (go
                         (async/put! loop-chan {
                                                (wf/rand-sid) [:hello (utils/now)]})
                         )
                       ))

      (into [:div {:style {:margin-top "1rem"}} [:header "RESULTS"]]
        (map (fn [[k v]]
               [:div {:style {:display "flex"}}
                [:.k {:style {:width "40%"}} (pr-str k)]
                [:.v (pr-str v)]
                ;(pr-str [k v])
                ;[:hr]
                ]
               ) (::result wf))
        )
      ])
   ]
    )
  )

(rum/defc <wf-results> < rum/static [results]
  (into [:div {:style {:margin-top "1rem"}} [:header "RESULTS"]]
        (map (fn [[k v]]
               [:div {:style {:display "flex"}}
                [:.k {:style {:width "40%"}} (pr-str k)]
                [:.v (pr-str v)]]
               ) results))
  )


;;
;;
(rum/defc <generic-wf-ui> < rum/reactive [*wf cmp]
  (let [wf @*wf
        status (get wf ::status :not-running)
        ui-state (get-in wf [::ui] {})
        ]
    [:div {:style {:outline "1px solid crimson"
                   :padding "1rem"}}

      (ui/menubar (pr-str status) (if (= :running status)
                                   [["stop" (fn []
                                              (if-let [xtor (::xtor @*wf)]
                                                (base/end! xtor)
                                                (println "no :xtor found")))]]
                                   []))


      (if (= :running status)
       [:div
        (cmp wf)
        (<wf-results> (::result wf))
        ])
     ]
    )
  )


(rum/defc <static-posts-ui> < rum/static [wf]
  (let [ui-state (get-in wf [::ui] {})]
    (into [:ul]
          (map (fn [a]
                 [:li (pr-str a)])
               (::list ui-state)))))


(rum/defc <timer-ui> < rum/static [wf]
  (let [ui-state (get-in wf [::ui] {})]
    [:h2 (str "now is " (str (js/Date. (::now ui-state))))]))


(rum/defc <ui> < rum/reactive [*STATE]

  ;; ui loop stuff - common for all workflows
  (let [init-loop-fn (fn [params] {::loop (async/chan)})
        ctx-loop-fn (fn [params]
                      {
                       :loop {
                              :fn       (fn [loop-chan]
                                          ;; (u/wiretap-chan in-chan (partial println "UI LOOP:"))
                                          loop-chan)

                              :infinite true
                              :expands? true
                              }
                       })
        steps-loop-fn (fn [params] {::loop [:loop (::loop params)]})
        ]

    [:div


     ;; a timer example
     (let [*wf (rum/cursor-in *STATE [:static-wf])
           ; init
           init-ui-state-fn (fn [params]
                              ;; save params
                              (swap! *wf assoc ::params params)

                              (swap! *wf assoc ::ui {
                                                     ::now nil
                                                     })
                              params)
           ; ctx
           ui-component-ctx-fn (fn [params]
                                 (let [*ui (rum/cursor-in *wf [::ui])]
                                   {
                                    :ui-set-now! {
                                                  :fn (fn [v]
                                                        (swap! *ui assoc ::now v)
                                                        )
                                                  }
                                    }
                                   ))
           ;
           timer-ctx-fn (fn [params]
                          {
                           :now          {
                                          :fn (fn [_]
                                                (utils/now))
                                          }

                           :infinite-now {
                                          :fn       (fn [max]
                                                      ;; todo: how to close channel, or take it via params
                                                      (let [ch (async/chan)]
                                                        (go
                                                          (doseq [i (range max)]
                                                            (.log js/console (str "UPD: " i))
                                                            (async/>! ch (utils/now))
                                                            (async/<! (utils/timeout 1000))

                                                            )
                                                          )
                                                        ch
                                                        )
                                                      )
                                          :infinite true
                                          }
                           }
                          )
           ]
       [:div
        [:header "static UI "
         (ui/btn "init" (fn []
                          (ui-wf *wf
                                 ; init
                                 [init-ui-state-fn init-loop-fn]
                                 ; ctx
                                 [ctx-loop-fn
                                  timer-ctx-fn
                                  ui-component-ctx-fn
                                  ]
                                 ; steps
                                 [steps-loop-fn
                                  (fn [params]
                                    {

                                     ;; a single now call
                                     ;; ::now [:now {}]

                                     ; if the source is infinite - then ui will be also update
                                     ::now    [:infinite-now 100]

                                     ; works the same with both for infinite steps and normal ones
                                     ::ui-now [:ui-set-now! ::now]

                                     })]
                                 )))]
        [:p "UI displays some value by sending value step into ui state modification step"]
        (<generic-wf-ui> *wf
                         <timer-ui>)
        ]
       )


     #_(let [*test-wf (rum/cursor-in *STATE [:ui-wf])]
         [:div
          (ui/btn "init ui wf" (fn [] (init-ui-wf *test-wf)))
          (<wf-ui> *test-wf)
          ]
         )


     [:hr]
     #_(into
       [:div]
       (map (fn [[k v]]
              [:pre (d/pretty k) "\n" (d/pretty v)
               [:hr]
               ]
              ) @*STATE)
       )

     ]
    )
  )

;; root ui component for prototype
(def <app> #(<ui> *UI-STATE))
