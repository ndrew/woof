(ns woof.app
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]
    [cljs.test :as test :refer-macros [deftest is testing run-tests async]]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.utils :as u]

    [woof.test-data :as test-data]

    [goog.string :as gstring]
    [goog.string.format]
    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]
    [woof.utils-macros :refer [put!?]]))


;; ui

(rum/defc menu-item
    < { :key-fn (fn [label _]
                  (str label))}
  [label action-fn]
    [:a.menu-item {:href "#"
                   :on-click (fn [e]
                               (action-fn)
                               (.preventDefault e)
                               false)}
     label])


(rum/defc menubar
  < rum/reactive
  "build a menubar"
  [menu-header menu-items]
  (into [:span.menubar
           [:.header menu-header]]
        (map (fn [[label action]]
               (menu-item label action)
               )
             menu-items)))

;; todo: menu input (next to a menu button)



(enable-console-print!)

(defonce *APP-STATE
  (atom {
          :hello :woof
          :context {
                     :hello {:fn (fn [a]
                                   (println ":hello" (pr-str a))
                                   ;"Hello!"

                                   (let [c (async/chan)]

                                     (go
                                       (async/put! c (str "Hello! " (pr-str a)))
                                       )

                                     c
                                     )
                                   )}
                     :hello-wait {:fn (fn [a]
                                   ;(println ":hello" (pr-str a))
                                   ;"Hello!"

                                   (let [c (async/chan)]
                                      (go
                                        (async/<! (u/timeout 5000))

                                       (async/put! c (str "Hello! " (pr-str a)))
                                       )

                                     c
                                     )
                                   )}
                     :async-expand {:fn (fn [a]
                                          (let [c (async/chan)]
                                            (go
                                              (async/put! c
                                                          {::xpanded [:hello (str "xpand--" a)]}))
                                            c
                                            ))
                                    :expands? true}
                     :xpand {:fn (fn [a]
                                   {::xxx [:hello "YYY"]}
                                   )
                             :expands? true
                             }

                     :8 {:fn (fn [s]
                                 ; (str "Hello " s "!")
                                 (let [chan (async/chan)]

                                   (go
                                     (async/<! (u/timeout 1000))
                                     (async/>! chan "1")
                                     (println "1")

                                     (async/<! (u/timeout 3000))
                                     (async/>! chan "2")

                                     (println "2")
                                     )

                                   chan))
                           :infinite true
                           }

                     }
          :workflow {
                      :name "test workflow"
                      :steps (assoc (array-map)
                               ::0 [:hello-wait "123"]
                               ::0_1 [:hello ::0]

                               ::1 [:async-expand "yo!"]
                               ::2 [:xpand {}]

                               ::8 [:8 {}]
                               ::9 [:hello ::8]
                               )
                      }
          }))




(defn run-wf!

  ([executor exec-chan save-op-fn]
   (go-loop []
            (let [r (async/<! exec-chan)
                  [status data] r]
              (if (save-op-fn r)
                (recur))
              )))
  ([executor exec-chan save-op-fn t]
    (run-wf! executor exec-chan save-op-fn)
    (go
       (async/<! (u/timeout t))
      ;; todo: check if wf ended
       (wf/end! executor)
    ))
)






(defn gen-new-wf-f! [N]
  (fn[]
    (let [{
          test-context :context
          test-steps :steps
          } (test-data/get-test-steps-and-context N)]
    (swap! *APP-STATE assoc-in [:context] test-context)
    (swap! *APP-STATE assoc-in [:workflow :steps] test-steps))))


(rum/defc wf-status-ui
    < { :key-fn (fn [status]
                  (str status))}
  [status]

  [:span.tag
   {:class (condp = status
             ::not-started ""
             ::done        "done"
             ::running     "pending"
             ::stopped     "error"
             ::error       "error"
             )}
   (condp = status
             ::not-started "…"
             ::done        "done!"
             ::running     "running"
             ::stopped     "stopped!"
             ::error       "error!"
             )]
)





(rum/defc step-ui
  < rum/reactive
  { :key-fn (fn [k _ _]
              (str "step-" k))}
  [k step r]

  ;(println "step-ui" (u/now))
  (let [ch? (u/channel? r)]
    [:div.step
     [:span.hbox
      [:span.k (pr-str k)]
      [:span.action
       (d/pretty step)]
      ]
     [:div.result
      (if ch?
         "…"
         (pr-str r)
         )
      ]
     (wf-status-ui
       (cond (nil? r) ::not-started
             (= :nil r) ::error ;; actual nil
             ch? ::running
             :else ::done)
       )
     ])
  )


;; todo: add css animation on value update

(rum/defc wf-results-ui
  < rum/reactive
  { :key-fn (fn [*results]
                  (str (::header @*results)))}
  "resulting map"
  [*results]
  (let [{status  ::wf-status
         history ::history
         header  ::header
         result  ::result
         actual-steps ::steps
         start   ::start
         } @*results]
    ;[:div.hbox
     ;[:pre (d/pretty actual-steps)]
     ;[:pre
     ; (if (or (= status ::done) (= status ::error))
     ;   (d/pretty result)
     ;   "") ]
     ;]

    ;(println "wf-results-ui" (u/now))

    (into [:div.steps
           ]
          (map (fn [[k v]]
                 ;(menu-item label action)
                 (step-ui k v (u/nil-get result k))
                 )
               actual-steps))

    ))



(defn done-percentage [result actual-steps]
  (* 100 (/ (reduce (fn[acc [k v]]
                                   (+ acc
                                      (if (u/channel? v)
                                        0
                                        1)
                                      )
                                )
                              0 result
                              )
                      (count actual-steps))))


(rum/defcs wf-ui
  < rum/reactive
  (rum/local ::steps-ui  ::status)

  (rum/local nil         ::executor)
  (rum/local nil         ::exec-chan)

  (rum/local {::wf-status ::not-started
              ::steps      {}
              ::history    []
              ::result     {}
              ::header     ""
              ::start      0
              } ::result)

  [local *context *workflow]

  ; (println "wf-ui" (u/now))


  (let [status    @(::status local)
        executor  @(::executor local)
        exec-chan @(::exec-chan local)

        {steps :steps
         header :name
         } @*workflow

        steps-ready-fn (fn []
                         (reset! (::status local) ::workflow-ui))
        execute-wf (fn []
                     ;; todo: choose executor
                     (reset! (::executor local) (wf/executor *context steps))
                     (let [exec-chan-0 (wf/execute! @(::executor local))
                           exec-chan (async/pipe exec-chan-0 (async/chan 1 (wf/time-update-xf 750)))]
                       (reset! (::exec-chan local) exec-chan)

                       (swap! (::result local) merge
                              {
                                ::header header
                                ::wf-status ::running
                                ::steps steps
                                ::start (u/now)
                                })

                       (reset! (::status local) ::results-ui)

                       (run-wf! executor exec-chan
                                (fn [r]
                                  (let [[status data] r
                                        done? (= :done status)
                                        ]

                                    ;; todo: how to handle new steps added

                                    ;; (println "STATUS: " (pr-str status))

                                    (when (= :error status)
                                      (swap! (::result local) assoc-in [::wf-status] ::error)
                                      (swap! (::result local) assoc-in [::result] data))


                                    (when (= :expand status)
                                      (let [[x-id nu-steps] data]
                                        (swap! (::result local) update-in [::steps] merge nu-steps)
                                        (swap! (::result local) assoc-in [::result] data)
                                        ))

                                    (when (= :process status)
                                      (swap! (::result local) assoc-in [::result] data))

                                    (when (= :wf-update status)
                                      (swap! (::result local) assoc-in [::result] data)
                                      )

                                    (when done?
                                      (swap! (::result local) assoc-in [::wf-status] ::done)
                                      (swap! (::result local) assoc-in [::result] data))

                                    (swap! (::result local) update-in [::history] conj r)

                                    (not done?))))))

        generate-wf-fn (fn []
                         (swap! (::result local)
                                merge {
                                        ::wf-status ::not-started
                                        ::history []
                                        ::steps   []
                                        ::result  {}
                                        ::header  "test wf (40)"
                                        ::start (u/now)
                                        })
                         (reset! (::status local) ::steps-ui)
                         ((gen-new-wf-f! 40)))
        stop-fn (fn []
                  (wf/end! @(::executor local)) ;; todo:

                  ;; use different key, as stop is not immidiate
                  ;;(swap! (::result local) assoc-in [::wf-status] ::stopped)
                  )
        ]


    ;; short-hand version
    #_(do
        (when (nil? executor)
          (reset! (::executor local) (wf/executor *context steps))

          (let [exec-chan (wf/execute! @(::executor local))]
            (reset! (::exec-chan local) exec-chan)
            (run-wf! executor exec-chan (fn [r]
                                          (swap! (::result local) conj r)
                                          true
                                          ))))
        [:pre (d/pretty @(::result local))])

    [:div
     (when (#{::steps-ui} status)
       [:div
        (menubar header [["steps ready!" steps-ready-fn]
                         ["generate new" generate-wf-fn]])
        [:div.tip "Here workflow will be ui for defining and manipulating workflow."]
        [:div.hbox
         [:div.context-ui
          (menubar "context" [])
          [:div (d/pretty (keys @*context))]]
         [:div.steps-ui
          (menubar "steps" [])
          [:div (d/pretty steps)]]
         ]])

     (when (#{::workflow-ui} status)
       [:div
        (menubar header [["run (normal)" execute-wf]
                         ["run (timeout)" (fn[])]
                         ["run (with cache)" (fn[])]
                         ["debug" (fn[])]
                         ])
        [:div.tip "Configure workflow execution options."]])

     (when (#{::results-ui} status)
       (let [{status  ::wf-status
              history ::history
              header  ::header
              result  ::result
              actual-steps ::steps
              start   ::start
              } @(::result local)
             actions (if (= status ::done)
                       [["re-run" (fn [] (reset! (::status local) ::steps-ui))] ["generate new" generate-wf-fn]]
                       [["stop" stop-fn]])
             ]
         [:div
          (wf-status-ui status)
          [:span
           (str (gstring/format "%.2f" (done-percentage result actual-steps)) "% " (- (u/now) start) "ms.   ")]

          (menubar header actions)

          (wf-results-ui (::result local))
          ]))
     ]
    ))




(rum/defcs app-ui
  < rum/reactive [local *STATE]
  [:div#app
    ;[:header (str "Hello! " (:hello @*STATE))]

    (wf-ui (rum/cursor-in *APP-STATE [:context])
            (rum/cursor-in *APP-STATE [:workflow]))
   ])


(rum/mount (app-ui *APP-STATE)
           (. js/document (getElementById "app")))


(add-watch *APP-STATE :watcher
  (fn [key atom old-state new-state]
    (rum/mount (app-ui *APP-STATE)
               (. js/document (getElementById "app")))))



(defn on-js-reload []
  #_(.clear js/console))








(test/deftest hello
  (println "YO!!"))


;;(cljs.test/run-tests)


