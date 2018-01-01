(ns woof.app
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]
    [cljs.test :as test :refer-macros [deftest is testing run-tests async]]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.utils :as u]

    [woof.test-data :as test-data]
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
                                       (async/put! c "Hello!")
                                       )

                                     c
                                     )
                                )}
                     }
          :workflow {
                      :name "test workflow"
                      :steps (assoc (array-map)
                                ::0 [:hello {}]
                                ;; ::1 [:hello {}]
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
             )}
   (condp = status
             ::not-started "…"
             ::done        "done!"
             ::running     "running"
             ::stopped     "stopped!"
             )]
)


(rum/defcs results-ui
  < rum/reactive
  [local *results stop-fn]

  (let [{status  ::wf-status
         history ::history
         header  ::header

         } @*results
        actions (if (= status ::done)
                  []
                  [["stop" stop-fn]])
        ]
    [:div
     (wf-status-ui status)
     (menubar header actions)
     [:pre (d/pretty history)]
     ]
    )
  )



(rum/defcs wf-ui
  < rum/reactive
    (rum/local ::steps-ui  ::status)

    (rum/local nil         ::executor)
    (rum/local nil         ::exec-chan)

    (rum/local {::wf-status ::not-started
                ::history []
                ::result {}
                ::header ""
                } ::result)

  [local *context *workflow]

  (let [status    @(::status local)
        executor  @(::executor local)
        exec-chan @(::exec-chan local)

        {steps :steps
         header :name
         } @*workflow

        steps-ready-fn (fn []
                          (reset! (::status local) ::workflow-ui)

                         )
        execute-wf (fn []
                     ;; todo: choose executor
                     (reset! (::executor local) (wf/executor *context steps))
                     (let [exec-chan (wf/execute! @(::executor local))]
                       (reset! (::exec-chan local) exec-chan)

                       (swap! (::result local) merge
                              {
                                ::header header
                                ::wf-status ::running
                               })

                       (reset! (::status local) ::results-ui)

                       (run-wf! executor exec-chan
                                (fn [r]
                                  (let [[status data] r
                                        done? (= :done status)
                                        ]
                                    ;; ::wf-status ::running
                                    (when done?
                                      (swap! (::result local) assoc-in [::wf-status] ::done)
                                      )

                                    (swap! (::result local) update-in [::history] conj r)


                                    (not done?)
                                    )))))
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
        [:pre (d/pretty @(::result local))]
        )


      [:div

       (when (#{::steps-ui} status)
         [:div
          (menubar header [["steps ready!" steps-ready-fn] ["generate new" (gen-new-wf-f! 10)]])
          [:div.tip "Here workflow will be ui for defining and manipulating workflow."]
          [:div.hbox
           [:div.context-ui
            (menubar "context" [])

            [:div (d/pretty (keys @*context))]
            ]
           [:div.steps-ui
            (menubar "steps" [])

            [:div (d/pretty steps)]
            ]
           ]])

      (when (#{::workflow-ui} status)
        [:div
         (menubar header [["run (normal)" execute-wf]
                           ["run (timeout)" (fn[])]
                           ["run (with cache)" (fn[])]
                           ["debug" (fn[])]
                          ])
         [:div.tip "Configure workflow execution options."]

         ])

       (when (#{::results-ui} status)
         (results-ui (::result local)
                     (fn []
                       (wf/end! @(::executor local))

                       ;; use different key, as stop is not immidiate
                       ;;(swap! (::result local) assoc-in [::wf-status] ::stopped)
                       )
                     ))
      ]
    ))




(rum/defcs app-ui
  < [local *STATE]
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


