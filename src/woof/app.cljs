(ns woof.app
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]
    [cljs.test :as test :refer-macros [deftest is testing run-tests async]]

    [woof.data :as d]

    [woof.graph :as g]

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
               (if (and (nil? label) (nil? action))
                 [:.separator]
                 (menu-item label action)
                 )

               )
             menu-items)))

;; todo: menu input (next to a menu button)

; primitive type editor
(rum/defcs data-ui < rum/reactive
                      (rum/local false ::editing)
                      (rum/local nil ::value)
  [{*EDITING? ::editing
    *NEW-VALUE ::value} change-fn d]

  (let [is-data true] ; (satisfies? c/DATA d)

    (if @*EDITING?
      (let [change (fn[new-value] ; editor
                       (change-fn new-value)
                       (reset! *EDITING? false))]

        [:div.edn.data.edit [:input {:on-change (fn [s] (reset! *NEW-VALUE (.. s -target -value)))
                                     :on-blur (fn[e] (change (d/to-primitive @*NEW-VALUE)))
                                     :on-key-press (fn[e] (if (== 13 (.-charCode e)) (change (d/to-primitive @*NEW-VALUE))))
                                     :value @*NEW-VALUE}]])

        ;; data
      [:div.edn.data
       {on-double-click (fn[e] (swap! *EDITING? not)
                          (reset! *NEW-VALUE d))}
       (d/pretty d)])))



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
                      :name "TEST WORKFLOW"
                      :steps (assoc (array-map)
                               ;;::0 [:hello-wait "123"]
                               ;;::0_1 [:hello ::0]

                               ;;::1 [:async-expand "yo!"]
                               ;;::2 [:xpand {}]

                               ;;::8 [:8 {}]
                               ;;::9 [:hello ::8]
                               ::woof [:hello "test wf!"]
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
     [:div.result
      (if ch?
         "…"
         (pr-str r)
         )
      ]
     [:div.info
      [:span.k (pr-str k)]
      [:span.action
       (d/pretty step)]

     (wf-status-ui
       (cond (nil? r) ::not-started
             (= :nil r) ::error ;; actual nil
             ch? ::running
             :else ::done)
       )
      ]
     ])
  )


;; todo: add css animation on value update

(rum/defcs wf-results-ui
  < rum/reactive
  { :key-fn (fn [_ *results]
                  (str (::header @*results)))}

  (rum/local {} ::updated-params)

  "resulting map"
  [local *editors *results]
  (let [{status  ::wf-status
         history ::history
         header  ::header
         result  ::result
         actual-steps ::steps
         start   ::start
         } @*results
        {param-editors :pre
         previews :post} @*editors
        ]
    ;(println "wf-results-ui" (u/now))

    ;; todo: store changed params

    (into [:div.steps
           ]
          (map (fn [[k v]]
                 ;(menu-item label action)

                 (let [[action params] v]
                   [:.step-ui {:class (str
                               (if (get param-editors k) "edit" "")
                               (if (get previews k) "prev" "")
                               )
                               }
                    (when (get param-editors k)

                      (if-not (get @(::updated-params local) k)
                        ;(if-not (u/action-id? params)
                          (swap! (::updated-params local) assoc k params)
                        ;)
                      )

                    (if (get @(::updated-params local) k)
                     [:.editor
                      [:header (str (name k) action ": ")]
                      (data-ui (fn[new-v]
                                 (swap! (::updated-params local) assoc k new-v)
                                 (go (async/put! (get param-editors k) new-v)))
                               (get @(::updated-params local) k))]))

                     (if (get previews k)
                        [:.preview
                         (menubar "Preview" [["ok" (fn[]
                                                     (go
                                                       (async/>! (get previews k) :ok))
                                                     (swap! *editors update-in [:post] dissoc k)
                                                     )]])
                         [:marquee
                          (pr-str (u/nil-get result params))]
                         ])

                      (step-ui k v (u/nil-get result k))
                    ])))

               actual-steps)))



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

  (rum/local {:pre   {}
               :post {}}  ::editors)

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

        execute-wf (fn []
                     ;; todo: choose executor

                     (reset! (::executor local) (wf/executor *context
                                                             (:steps @*workflow)))

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

        steps-ready-fn (fn []
                         ;; uncomment these
                         ;; (reset! (::status local) ::workflow-ui)

                         (execute-wf)
                         )


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


        preview-test-fn
        (fn []
          (let [
                 preview-chan (async/chan)] ;; todo: use different channel for ui
            (swap! (::editors local) update-in [:post] assoc ::preview preview-chan)

            (swap! *context merge
                   {:preview {:fn (fn [s]
                                    (let [c (async/chan)]
                                      ;(println "PREVIEW" s)

                                      (go-loop []
                                        (let [v (async/<! preview-chan)] ; read from preview chan
                                          (async/>! c s)
                                          (recur))
                                        )
                                      c
                                      )
                                    ;(str "PREVIEW: " s)

                                    ) :infinite true}})

            (swap! *workflow update-in [:steps]
                                  merge {
                                          ::test-preview  [:hello "woof"]
                                          ::preview [:preview ::test-preview]
                                          })

            ))

        editor-test-fn (fn []
                         (let [editor-chan (async/chan)]

                           (swap! (::editors local) update-in [:pre] assoc ::editor editor-chan)

                           (swap! *context merge
                                  {:edit-params
                                   {:fn (fn [s]
                                          ; (str "Hello " s "!")
                                          (go
                                            (async/<! (u/timeout 1000))
                                            (async/>! editor-chan "1")
                                            )
                                          editor-chan)
                                    :infinite true
                                    }
                                   })

                           (swap! *workflow update-in [:steps]
                                  merge {
                                          ::editor [:edit-params ::woof]
                                          ::editor-result  [:hello ::editor]

                                          })
                           )
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
                         ["generate new" generate-wf-fn]
                         []
                         ["editor test" editor-test-fn]
                         ["preview test" preview-test-fn]
                         ])
        [:div.tip "Here workflow will be ui for defining and manipulating workflow."]
        [:div.hbox
         [:div.steps-ui
          (menubar "steps" [])
          [:pre (d/pretty steps)]]
         [:div.context-ui
          (menubar "context" [])
          [:pre (d/pretty (keys @*context))]]
         ]

        [:div.graph

         {:dangerouslySetInnerHTML
          {:__html (g/graph-to-svg steps (fn [gviz [k [action param]]]
                                           (if (u/action-id? param)
                                             (str gviz " "
                                                  (clojure.string/replace (name param) #"-" "_")
                                                  " -> "
                                                  (clojure.string/replace (name k) #"-" "_")
                                                  ";\n")
                                             gviz
                                             )
                                           ))}}
         ]

        ])

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

          (wf-results-ui (::editors local) (::result local))
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


