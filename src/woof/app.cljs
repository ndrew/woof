(ns woof.app
  (:require
    [cljs.core.async :as async]
    [cljs.test :as test :refer-macros [deftest is testing run-tests async]]

    [goog.string :as gstring]
    [goog.string.format]

    [rum.core :as rum]

    [woof.data :as d]
    [woof.graph :as g]
    [woof.wf :as wf]
    [woof.ui :as ui]
    [woof.utils :as u]
    [woof.test-data :as test-data])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]
    [woof.utils-macros :refer [put!?]]))




;(enable-console-print!)


(defonce *APP-STATE
  (atom {
          :hello :woof
          :context {
                     :identity {:fn (fn [a] a)}

                     :identity-async {:fn (fn [a]
                                            (let [c (async/chan)]
                                              (go
                                                (async/put! c a))
                                              c))}

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

                     :8 {:fn (fn [max-num]
                                 (.warn js/console "max-num" max-num)

                                 ; (str "Hello " s "!")
                                 (let [chan (async/chan)
                                       t (volatile! (u/now))]

                                   (go-loop [i 0]
                                     (async/<! (u/timeout 500))
                                     (.warn js/console "i" i (< i max-num) (- (u/now) @t) )
                                     (vreset! t (u/now))

                                     (async/>! chan (str i ": " (int (rand 100))))

                                    (if (< i max-num)
                                     (recur (inc i)))
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

                               ;; ::8 [:8 20]
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


(defonce status-classes-map {
                              ::not-started ""
                              ::done        "done"
                              ::running     "pending"
                              ::stopped     "error"
                              ::error       "error"
                              })

(defonce status-caption-map {
                              ::not-started "…"
                              ::done        "done!"
                              ::running     "running"
                              ::stopped     "stopped!"
                              ::error       "error!"
                              })

(rum/defc wf-status-ui  < rum/static
      ;{ :key-fn (fn [status] (str status))}
  [status]

  [:span.tag
   {:class (get status-classes-map status "")}
   (get status-caption-map status "")])




(rum/defc step-ui   <   rum/static
                        {:key-fn (fn [k _ _] k)}
  [k step r]

  (let [ch? (u/channel? r)
        status (if ch? ::running
                 (get {nil ::not-started, :nil ::error} r ::done))]
    [:div.step
     [:div.result
      (if ch? "…" (pr-str r))]
     [:div.info
      [:span.k (pr-str k)]
      [:span.action (d/pretty step)]

     (wf-status-ui status)]]))


;; todo: add css animation on value update






(defn done-percentage [result actual-steps]
  (if (map? result)
    (* 100 (/ (reduce (fn [acc [k v]]
                                   (+ acc
                                      (if (u/channel? v)
                                        0
                                        1)
                                      )
                                )
                              0 result
                              )
                      (count actual-steps)))
    (do
      (.warn js/console (d/pretty
                          [actual-steps result]
                          ))
      0
      )

    ))


(rum/defc wf-progress-ui < { :key-fn (fn [start _] start)} ;; todo: make it timer based
  "shows workflow progress status - time elapsed, done percentage"

  [start-t percent-done]
  [:span (str (gstring/format "%.2f" percent-done) "% " (- (u/now) start-t) "ms.   ")])





(rum/defcs wf-step-editor < rum/reactive {:key-fn (fn [k _ _ _] k)}
  (rum/local nil ::updated-v)
  (rum/local false ::manually-entered)

  [local header params editor-chan]

  ;; todo: store both params and manually entered value if any

  (if-let [local-params @(::updated-v local)]
    (if-not (or (= local-params params) @(::manually-entered local))
      (reset! (::updated-v local) params))
    (reset! (::updated-v local) params))


  (if-let [v @(::updated-v local)]
    [:.editor
     [:header header]
     (ui/data-editor (fn[new-v]
                       (reset! (::updated-v local) new-v)
                       (reset! (::manually-entered local) true)
                       (go
                         (async/put! editor-chan new-v)))
                     v)])
  )


(rum/defc wf-step-ui < rum/reactive {:key-fn (fn [k _ _ _] k)}


  [k v result editor-chan preview-fn]

  (let [[action params] v]
  [:.step-ui {:class (str
                       (if editor-chan "edit" "")
                       (if preview-fn "prev" ""))}

   (when (and editor-chan
              (if (u/action-id? params) (not (u/channel? (get result params))) true))
     (if (u/action-id? params)
       (if-let [nu-params (get result params)]
         (wf-step-editor (str (name k) action ": ") nu-params editor-chan))
       (wf-step-editor (str (name k) action ": ") params editor-chan)))

   (if preview-fn
     (preview-fn))

   (step-ui k v (u/nil-get result k))
]))




(rum/defc wf-results-ui
  < rum/reactive
  { :key-fn (fn [header _ _] header)}

  [header result actual-steps *editors]

  ;; FIXME:
  (let [{param-editors :pre
         previews :post} @*editors]
    ;(println "wf-results-ui" (u/now))

    ;; todo: store changed params
    (into [:div.steps]
          (map (fn [[k v]]
                 (let [editor-chan (get param-editors k)

                       preview-chan (get previews k)
                       preview-fn (if preview-chan
                         (fn []
                           [:.preview
                                (ui/menubar "Preview"
                                            [["ok" (fn[]
                                                     (go
                                                       (async/>! preview-chan :ok))
                                                     ;; TODO:
                                                     (swap! *editors update-in [:post] dissoc k)
                                                     )]])
                                [:div (pr-str (u/nil-get result (second v)))]])
                         nil)

                       ]

                   (wf-step-ui k v result editor-chan preview-fn)
                   )))
          actual-steps)))


(defonce UI-UPDATE-RATE 50)


(rum/defcs wf-ui   <   rum/reactive

  {:key-fn (fn [_ *workflow] (get @*workflow :name))}

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
                           exec-chan (async/pipe exec-chan-0 (async/chan 1 (wf/time-update-xf UI-UPDATE-RATE)))]
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
                                         ;; todo: check if this breaks done-percentage
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
                                        ::header  "test wf (20)"
                                        ::start (u/now)
                                        })
                         (reset! (::status local) ::steps-ui)
                         ((gen-new-wf-f! 20)))
        stop-fn (fn []
                  (wf/end! @(::executor local)) ;; todo:

                  ;; use different key, as stop is not immidiate
                  ;;(swap! (::result local) assoc-in [::wf-status] ::stopped)
                  )


        passthought-fn (fn [preview-chan s] ;; todo: maybe there is pipe fn for channel?

                          (let [c (async/chan)]
                            (go-loop []
                                     (let [v (async/<! preview-chan)] ; read from preview chan
                                       (async/put! c s)
                                       (recur)))
                            c))

        preview-test-fn (fn []
          (let [preview-chan (async/chan)] ;; todo: use different channel for ui
            (swap! (::editors local) update-in [:post] assoc ::preview preview-chan)

            (swap! *context merge
                   {:preview {:fn (partial passthought-fn preview-chan) :infinite true}})

            ;; todo: pass rum component into preview fn
            (swap! *workflow update-in [:steps]
                                  merge {
                                          ::test-preview  [:8 10]
                                          ::preview [:preview ::test-preview]
                                          })

            ))

        editor-test-fn (fn []
                         (let [editor-chan (async/chan)]

                           (swap! (::editors local) update-in [:pre] assoc ::editor editor-chan)

                           (swap! *context merge
                                  {:edit-params
                                   {:fn (fn [s]
                                          (let [c (async/chan)]
                                            (go-loop []
                                                     (let [v (async/<! editor-chan)] ; read from preview chan

                                                       ;;(.warn js/console (pr-str v))

                                                       (async/put! c v)
                                                       (recur)))
                                            c)
                                          )
                                    :infinite true}
                                   })

                           (swap! *workflow update-in [:steps]
                                  merge {
                                          ;::editor-source-1  [:8 3]
                                          ;::editor-source  [:8 ::editor-source-1]

                                          ::nested-1  [:identity-async ::nested-e]
                                          ::nested-e [:hello ::e-result-1]

                                          ::e-result  [:identity ::editor]
                                          ::editor [:edit-params "Hello!"]
                                          ::e-result-1  [:identity-async ::editor]


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
        (ui/menubar header [["steps ready!" steps-ready-fn]
                             ["generate new" generate-wf-fn]
                             []
                             ["editor test" editor-test-fn]
                             ["preview test" preview-test-fn]])
        [:div.tip "Here workflow will be ui for defining and manipulating workflow."]
        [:div.hbox
         [:div.steps-ui
          (ui/menubar "steps" [])

          [:pre (d/pretty steps)]]
         [:div.context-ui
          (ui/menubar "context" [])
          [:pre (d/pretty (keys @*context))]]
         ]

        #_(comment ;; todo: show via settings
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
                                           ))}}])


        ])

     (when (#{::workflow-ui} status)
       [:div
        (ui/menubar header [["run (normal)" execute-wf]
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
          (wf-progress-ui start (done-percentage result actual-steps))
          (ui/menubar header actions)

          (wf-results-ui header result actual-steps (::editors local) )]))
]))




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
  ;; todo: force close all channels

  (.clear js/console))








(test/deftest hello
  (println "YO!!"))


;;(cljs.test/run-tests)


