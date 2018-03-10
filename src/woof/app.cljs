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
    [woof.ws :as ws]

    [woof.ui :as ui]
    [woof.utils :as u]
    [woof.test-data :as test-data])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]
    [woof.utils-macros :refer [put!?]]))



(enable-console-print!)



(defonce UI-UPDATE-RATE 50) ; ms

(defonce TEST-WF-STEP-COUNT 25)



(defn default-context []
  {
    :identity {:fn (fn [a] a)}

    :identity-async {:fn (fn [a]
                           (let [c (async/chan)]
                             (go
                               (async/put! c a))
                             c))}
    :hello {:fn (fn [a]
                  (let [c (async/chan)]
                    (go
                      (async/put! c (str "Hello! " (pr-str a))))
                    c))}

    :hello-wait {:fn (fn [a]
                       (let [c (async/chan)]
                         (go
                           (async/<! (u/timeout 5000))

                           (async/put! c (str "Hello! " (pr-str a))))
                         c))}

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
                           (recur (inc i))))

                chan))
        :infinite true
        }
    }
  )



(defonce *APP-STATE
  (atom
    {
      ;; workflow context
      :context (default-context)

      ;; workflow definition
      :workflow {
                  :name "TEST WORKFLOW"
                  :steps (assoc (array-map) ::woof [:identity "test wf!"])
                  }


      ;; step handler to editor mapping
      :editors {:pre   {}
                :post {}}

      }))



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


(rum/defc <wf-status-ui>  < rum/static
  [status]

  [:span.tag
   {:class (get status-classes-map status "")}
   (get status-caption-map status "")])




(rum/defc <step-status>   <    rum/static
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

     (<wf-status-ui> status)]]))


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
      0))
)



(rum/defc <wf-progress-ui>   <   { :key-fn (fn [start _] start)} ;; todo: make it timer based
  "shows workflow progress status - time elapsed, done percentage"

  [start-t percent-done]
  [:span (str (gstring/format "%.2f" percent-done) "% " (- (u/now) start-t) "ms.   ")])





(rum/defcs <wf-step-editor>     <      rum/reactive {:key-fn (fn [k _ _ _] k)}
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



(rum/defc <wf-step-ui> < rum/reactive {:key-fn (fn [k _ _ _] k)}

  [k v result editor-fn preview-fn]

  (let [[action params] v]
  [:.step-ui {:class (str
                       (if editor-fn "edit" "")
                       (if preview-fn "prev" ""))}

   (if editor-fn
     (editor-fn))

   (if preview-fn
     (preview-fn))

   (<step-status> k v (u/nil-get result k))]
  ))




(rum/defc <wf-results-ui>
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
                 (let [[action params] v

                       editor-chan (get param-editors k)

                       show-param-editor? (and editor-chan
                                              (if (u/action-id? params) (not (u/channel? (get result params))) true))

                       editor-fn (if show-param-editor?
                                   (fn []
                                     (if (u/action-id? params)
                                       (if-let [nu-params (get result params)]
                                         (<wf-step-editor> (str (name k) action ": ") nu-params editor-chan))
                                       (<wf-step-editor> (str (name k) action ": ") params editor-chan))
                                     )
                                   nil)

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

                   (<wf-step-ui> k v result editor-fn preview-fn)
                   )))
          actual-steps)))




(defn workflow-handler [*result r]
  (let [[status data] r
        done? (= :done status)]

    (when (= :error status)
      (swap! *result assoc-in [::wf-status] ::error)
      (swap! *result assoc-in [::result] data))


    (when (= :expand status)
      (let [[x-id nu-steps] data]
        ;; todo: check if this breaks done-percentage
        (swap! *result update-in [::steps] merge nu-steps)
        (swap! *result assoc-in [::result] data)
        ))

    (when (= :process status)
      (swap! *result assoc-in [::result] data))

    (when (= :wf-update status)
      (swap! *result assoc-in [::steps] (first data))
      (swap! *result assoc-in [::result] (second data))
      )

    (when done?
      (swap! *result assoc-in [::wf-status] ::done)
      (swap! *result assoc-in [::result] data))

    (swap! *result update-in [::history] conj r)

    (not done?)))



(defn- passthought-fn [preview-chan s] ;; todo: maybe there is pipe fn for channel?
  (let [c (async/chan)]
    (go-loop []
             (let [v (async/<! preview-chan)] ; read from preview chan
               (async/put! c s)
               (recur)))
    c))



(defn- pipe-fn [editor-chan s]
  (let [c (async/chan)]
    (go-loop []
             (let [v (async/<! editor-chan)] ; read from preview chan
               (async/put! c v)
               (recur)))
    c))


(defn execute-for-ui [executor]
  (let [exec-chan-0 (wf/execute! executor)
        exec-chan (async/pipe exec-chan-0 (async/chan 1 (wf/time-update-xf UI-UPDATE-RATE)))]

    exec-chan))


(rum/defcs <wf-ui>   <   rum/reactive
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

  [local *context *workflow *editors]


  (let [status    @(::status local)
        executor  @(::executor local)
        exec-chan @(::exec-chan local)

        {steps :steps
         header :name
         } @*workflow

        execute-wf (fn []
                     (reset! (::executor local) (wf/executor *context (:steps @*workflow))) ;; todo: choose executor

                     (let [exec-chan (execute-for-ui @(::executor local))]
                       (reset! (::exec-chan local) exec-chan)
                       (swap! (::result local) merge {
                                                       ::header header
                                                       ::wf-status ::running
                                                       ::steps steps
                                                       ::start (u/now)
                                                       })
                       (reset! (::status local) ::results-ui)

                       (wf/async-execute! executor exec-chan (partial workflow-handler (::result local)))

                       ))

        steps-ready-fn (fn []
                         ;;
                         ;; uncomment this for intermediary screen
                         ;; (reset! (::status local) ::workflow-ui)

                         (execute-wf))

        generate-wf-fn (fn []
                         (swap! (::result local)
                                merge {
                                        ::wf-status ::not-started
                                        ::history []
                                        ::steps   []
                                        ::result  {}
                                        ::header  (str "test wf (" TEST-WF-STEP-COUNT ")")
                                        ::start (u/now)})
                         (reset! (::status local) ::steps-ui)
                         ((gen-new-wf-f! TEST-WF-STEP-COUNT)))
        stop-fn (fn []
                  (wf/end! @(::executor local))

                  ;; use different state, as stop is not immidiate
                  ;; (swap! (::result local) assoc-in [::wf-status] ::stopped)
                  )

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
                                          })))

        expand-test-fn (fn []
                         (let [{xpand-context :context
                                xpand-steps   :steps} (test-data/gen-expand-wf [:a :b :c])]

                           (swap! *context merge xpand-context)
                           (swap! *workflow update-in [:steps] merge xpand-steps)))

        editor-test-fn (fn []
                         (let [editor-chan (async/chan)]

                           (swap! *editors update-in [:pre] assoc ::editor editor-chan)

                           (swap! *context merge
                                  {:edit-params
                                   {:fn (partial pipe-fn editor-chan)
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

                                          ::tick   [:8 100]
                                          })))

        infinity-test-fn (fn []

                           (swap! *context merge
                                  {:xpand-8
                                   {:fn (fn [s]
                                          (let [c (async/chan)]
                                            (go []
                                                (let [v (async/<! (u/timeout 1500))]

                                                  (async/put! c
                                                              {::x1 [:8 20]
                                                               ::x2 [:hello ::x1]
                                                                })))
                                            c)
                                          )
                                    :expands? true}
                                   })

                           (swap! *workflow update-in [:steps]
                                  merge {
                                          ::xpnd [:xpand-8 {}]
                                          ::8 [:8 10]
                                          ::i [:identity ::8]
                                          ::h [:hello ::i]
                                          })
                           )

        ]


    [:div
     (when (#{::steps-ui} status)
       [:div
        (ui/menubar header [["steps ready!" steps-ready-fn]
                             ["generate new" generate-wf-fn]
                             []
                             ["editor test" editor-test-fn]
                             ["preview test" preview-test-fn]
                             []
                             ["expand test" expand-test-fn]
                             []
                             ["infinity " infinity-test-fn]
                            ])

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
     ]) ;; end STEPS-UI



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

             sort-steps (fn [steps]
                          ;(into (sorted-map-by <) steps)
                          steps
                          )]
         [:div
          (<wf-status-ui> status)
          (<wf-progress-ui> start (done-percentage result actual-steps))
          (ui/menubar header actions)
          (<wf-results-ui> header result (sort-steps actual-steps) *editors )]))
]))



(def cursor (partial rum/cursor-in *APP-STATE))


(rum/defcs <app-ui>
  < rum/reactive [local *STATE]
  [:div#app
    (<wf-ui>
      (cursor [:context])
      (cursor [:workflow])
      (cursor [::editors]))])



(rum/mount (<app-ui> *APP-STATE)
           (. js/document (getElementById "app")))


; todo:

#_(let [socket (ws/connect "/api/websocket"
                 :on-open    (fn []
                               (println "ws open")

                               (.log js/console socket)

                               )
                 :on-close   (fn [] (println "ws close"))
                 :on-message (fn [msg] (println "got " msg)))]




  )


#_(defn- listen-loop []
  (let [socket-ch (async/chan 1)
        data-ch   (async/chan 10)
        ajax-ch   (async/chan 1)
        socket (ws/connect "/api/websocket"
                 :on-open    #(async/put! socket-ch :open)
                 :on-close   #(doseq [ch [socket-ch ajax-ch data-ch]]
                                (async/close! ch))
                 :on-message #(async/put! data-ch %))]
    (go
      (when (async/<! socket-ch) ;; open socket
        (swap! app-state assoc :progress 0.3)
        (u/ajax "/api/db/" (fn [datoms] (async/put! ajax-ch datoms) (swap! app-state assoc :progress 0.6)))
        (let [[dump _] (async/alts! [ajax-ch (async/timeout 30000)])]
          (when dump  ;; wait ajax
            (profile "DB initialization"
              (reset! conn (d/init-db dump schema))
              (let [num-users (count (d/datoms @conn :aevt :user/name))
                    num-datoms (count (:eavt @conn))
                    num-achs (count (d/datoms @conn :aevt :ach/sha1))]
                (println "Pushed [datoms:" num-datoms "] [users:" num-users "] [achievements:" num-achs "]")
                (swap! app-state assoc
                  :users {:total num-users :visible (min 30 num-users)}
                  :first-load? false
                  :progress 1)))
            (loop []
              (when-let [tx-data (async/<! data-ch)]  ;; listen for socket
                (try
                  (d/transact! conn tx-data)
                  (let [num-users (count (d/datoms @conn :aevt :user/name))]
                    (swap! app-state assoc-in [:users :total] num-users))
                  (catch js/Error e
                    (.error js/console e)))
                (recur))))
          (.close socket)))
     (swap! app-state assoc :progress -1)
     (js/setTimeout listen-loop 1000))))



(add-watch *APP-STATE :watcher
  (fn [key atom old-state new-state]
    (rum/mount (<app-ui> *APP-STATE)
               (. js/document (getElementById "app")))))




(defn on-js-reload []
  ;; todo: force close all channels

  (.clear js/console))




