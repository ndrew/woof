(ns woof.app
  (:require
    [cljs.core.async :as async]
    [cljs.test :as test :refer-macros [deftest is testing run-tests async]]

    [goog.string :as gstring]
    [goog.string.format]

    [rum.core :as rum]

    [woof.app-data :as app-model]

    [woof.data :as d]
    [woof.graph :as g]
    [woof.wf :as wf]
    [woof.ws :as ws]

    [woof.ui :as ui]
    [woof.wf-ui :as wf-ui]
    [woof.utils :as u]

    [woof.test-data :as test-data]
    ; [woof.wf-tester-ui :as tester-ui]
    )


  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]
    [woof.utils-macros :refer [put!?]]))



(enable-console-print!)





(defonce *APP-STATE

  (atom
    {
      ;; workflow context
      :context (app-model/default-context)

      ;; workflow definition
      :workflow {
                  :name "TEST WORKFLOW"
                  :steps (assoc (array-map) ::woof [:identity "test wf!"])
                  }


      ;; step handler to editor mapping
      ;:editors {:pre   {}
      ;          :post {}}

      :xctor nil
      :xctor-chan nil

      }))


(def cursor (partial rum/cursor-in *APP-STATE))




(defn gen-new-wf-f! [N]
  (fn[]
    (let [{
          test-context :context
          test-steps :steps
          } (test-data/get-test-steps-and-context N)]
    (swap! *APP-STATE assoc-in [:context] test-context)
    (swap! *APP-STATE assoc-in [:workflow :steps] test-steps))))








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

     (wf-ui/<wf-status-ui> status)]]))


;; todo: add css animation on value update


(defn done-percentage [result actual-steps]

  ;; todo

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
      #_(.warn js/console (d/pretty
                          [actual-steps result]))
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
                                              (if (wf/sid? params) (not (u/channel? (get result params))) true))

                       editor-fn (if show-param-editor?
                                   (fn []
                                     (if (wf/sid? params)
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



;; todo: remove this to op-map

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




;; state changing fns
;; fixme:
(defprotocol IWFUi
  (get-status [this])

  (set-status [this status])


  (merge-result [this data])

  (get-result* [this])

  (add-post-editor [this k v])

  (add-pre-editor [this k v])
  )


(defn make-ui-state [local]
  ; (::result local)

  (reify IWFUi
    (get-status [this] @(::status local))
    (set-status [this status] (reset! (::status local) status))

    (merge-result [this data]
                  (swap! (::result local) merge data))

    (get-result* [this] (::result local))

    (add-post-editor [this k v]
      (swap! (::editors local) update-in [:post] assoc k v))

    (add-pre-editor [this k v]
      (swap! (::editors local) update-in [:pre] assoc k v))
    )
  )





;; menu items


;; generate test workflow

(defonce TEST-WF-STEP-COUNT 25)

(defn generate-wf-fn [UI-STATE]
  (fn []

    (merge-result UI-STATE {
                   ::wf-status ::not-started
                   ::history []
                   ::steps   []
                   ::result  {}
                   ::header  (str "test wf (" TEST-WF-STEP-COUNT ")")
                   ::start (u/now)})

    (set-status UI-STATE ::steps-ui)
    ((gen-new-wf-f! TEST-WF-STEP-COUNT))))


(defn- generate-wf-mi [UI-STATE]
  ["generate new" (generate-wf-fn UI-STATE)])


;; stop workflow

(defn- stop-wf-mi [xctor]
  ["stop"
   (fn []
     (wf/end! xctor)
     ;; use different state, as stop is not immidiate
     ;; (swap! (::result local) assoc-in [::wf-status] ::stopped)
     )]
  )


;; expand test

(defn- expand-test-mi [model]
  ["expand test"
   (fn []
     (let [{xpand-context :context
            xpand-steps   :steps} (test-data/gen-expand-wf [:a :b :c])]

       (app-model/merge-context model xpand-context)
       (app-model/merge-steps   model xpand-steps)
       )

     )
   ]
  )

;; run workflow

(defn- run-wf [model callback]
  (app-model/start! model callback)
  )

(defn- run-wf-mi [model callback]
  ["run 🏃"
   (fn []
     (run-wf model callback))])

(defn- re-run-mi [ui-model]
  ["re-run" (fn []
              (set-status ui-model ::steps-ui))])

;; preview test

(defn- preview-mi [model UI-STATE]
  ["preview test"
   (fn []
     (let [preview-chan (async/chan)] ;; todo: use different channel for ui

       (add-post-editor UI-STATE ::preview preview-chan)
       (app-model/merge-context model {:preview {:fn (partial passthought-fn preview-chan) :infinite true}})

       ;; todo: pass rum component into preview fn
       (app-model/merge-steps model {
                                      ::test-preview  [:8 10]
                                      ::preview [:preview ::test-preview]
                                      })))
   ]
  )

;;
(defn- ajax-step-mi [model UI-STATE]

  ["ajax wf"
   (fn[]
     ;; ws/make-ajax-handler

     (app-model/merge-context model
                              {:ajax {:fn ws/transit-handler}})

     (app-model/merge-steps model {
                                      ::test-ajax  [:ajax (ws/resolve-url "/ajax")]
                                      })

     )
   ]
  )

;; editor test

(defn- editor-mi [model UI-STATE]
  ["editor test"
   (fn []
     (let [editor-chan (async/chan)]

       (add-pre-editor UI-STATE ::editor editor-chan)

       (app-model/merge-context model
                                {
                                  :edit-params
                                  {:fn (partial pipe-fn editor-chan)
                                   :infinite true}
                                  })

       (app-model/merge-steps model
                              {
                                ;::editor-source-1  [:8 3]
                                ;::editor-source  [:8 ::editor-source-1]

                                ::nested-1  [:identity-async ::nested-e]
                                ::nested-e [:hello ::e-result-1]

                                ::e-result  [:identity ::editor]
                                ::editor [:edit-params "Hello!"]
                                ::e-result-1  [:identity-async ::editor]

                                ::tick   [:8 100]
                                })

       ))
   ]
  )

;; infinity test

(defn- infinity-mi [model]
  ["infinity test"
   (fn []

     (app-model/merge-context model
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

     (app-model/merge-steps model
                            {
                              ::xpnd [:xpand-8 {}]
                              ::8 [:8 10]
                              ::i [:identity ::8]
                              ::h [:hello ::i]
                              })
     )]
  )

(defonce GRID-SIZE 20)


(rum/defc <node> [node]
(comment
;       <g class='node ${node.is_mesh ? 'mesh' : ''}' id='node_${node.id}'>
;      <rect rx='2' ry='2' x=${rect.x} y=${rect.y-(GRID_SIZE/2)} width="${rect.w}" height="${rect.h}" class='${node.children.length == 0 ? "fill" : ""}'/>
;      <text x="${rect.x+(rect.w/2)}" y="${rect.y+rect.h+(GRID_SIZE/2)}">${node.label}</text>
;      ${draw_ports(node)}
;      ${draw_glyph(node)}
;    </g>

  )
  (let [{_x :x
         _y :y
         _h :h
         _w :w } (:rect node)

        hg (/ GRID-SIZE 2)
        {x :x
         y :y
         h :h
         w :w } {:x (* GRID-SIZE _x)
              :y (* GRID-SIZE _y)
              :h (* GRID-SIZE _h)
              :w (* GRID-SIZE _w)
              }

        ]

    [:g {:class "node"
         :id (str "node_" (:id node))}

     [:rect {:rx 2 :ry 2
             :x x
             :y (- y hg)
             :width w
             :height h

             :class "fill" ;;
             }

      [:text {:x (+ x
                    (/ w 2)) ; rect.x+(rect.w/2)
              :y (- y hg)
              } (:label node)]

      ;; draw_ports
      ;; draw_glyph
      ]

     ]

    )

  )

(rum/defcs <graph> < rum/reactive
  [local]

  (let [
        ;v 1234
        network {:node {:id "node"
                        :label "Node"
                        :x 2 :y 2
                        :rect {
                                :x 2
                                :y 4
                                :h 2
                                :w 2
                                }
                        }
                  }
        ]
  [:svg
   {:xmlns "http://www.w3.org/2000/svg" :baseProfile "full" :version="1.1"}
     ;[:circle {:cx 10 :cy 10 :r 5 :fill "#ccc"}]
     (<node> (:node network))

   ]
    )

  )


(rum/defcs <wf-ui>   <   rum/reactive
                          {:key-fn (fn [_ *workflow] (get @*workflow :name))}

                          (rum/local ::steps-ui  ::status)

                          (rum/local {:pre   {}
                                      :post {}} ::editors)

                          (rum/local {::wf-status ::not-started
                                      ::steps      {}
                                      ::history    []
                                      ::result     {}
                                      ::header     ""
                                      ::start      0
                                      } ::result)

  [local _*context _*workflow model]


  (let [UI-STATE (make-ui-state local)

        *context   (app-model/get-context* model)
        *workflow (app-model/get-workflow-cfg* model)
        *editors   (::editors local)

        status    (get-status UI-STATE)
        executor  (app-model/get-xctor model)
        exec-chan (app-model/get-xctor-chan model)

        {steps :steps
         header :name
         } @*workflow


        ;; processing handler : todo: extract from rum component
        process-wf! (fn [model]

                     (merge-result UI-STATE {
                                              ::header header
                                              ::wf-status ::running
                                              ::steps steps
                                              ::start (u/now)
                                              })


                     (set-status UI-STATE ::results-ui)

                      ;; TODO: fix names
                     (let [opts {:execute (fn [executor]
                                            ;; use save
                                             (app-model/get-xctor-chan model)
                                             )
                                  :process-handler (partial workflow-handler (get-result* UI-STATE))
                                  }
                           worker (wf/->ResultProcessor executor opts)]

                       (wf/process-results! worker)
                       )
                     )

        ]


    [:div
     (<graph>)

     [:header header]


     (when (#{::steps-ui} status)
       [:div
        [:div.tip "Choose your workflow:"]

        (ui/menubar "" [["reset" (fn []
                                   ;; todo:
                                   )]
                         []

                         (ajax-step-mi model UI-STATE)
                         (generate-wf-mi UI-STATE)
                         (expand-test-mi model)
                         (editor-mi model UI-STATE)
                         (preview-mi model UI-STATE)
                         (infinity-mi model)

                         ])

        [:div.tip "—"]

        (ui/menubar "" [(run-wf-mi model process-wf!)])

        [:div.tip ""] ;; fixme:

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
                                           (if (wf/sid? param)
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
        ;; todo:
        (ui/menubar header [["run (normal)" (fn[] (run-wf model process-wf!))]
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
                       [
                         (re-run-mi UI-STATE)
                         (generate-wf-mi UI-STATE)
                        ]
                       [(stop-wf-mi (app-model/get-xctor model))])

             sort-steps (fn [steps]
                          ;; todo:
                          ;(into (sorted-map-by <) steps)
                          steps
                          )]
         [:div
          (wf-ui/<wf-status-ui> status)

          (<wf-progress-ui> start (done-percentage result actual-steps))
          (ui/menubar header actions)

          (<wf-results-ui> header result (sort-steps actual-steps) *editors )

          ]

         ))
]))






(rum/defcs <server-ui> < rum/reactive
                         (rum/local nil  ::socket)
  [local model server]
  (let [actions [ ["init ws" (fn[]
                                  (ws/start server)
                                (reset! (::socket local) (ws/get-socket server)))]

                  ["client-server wf"
                   (fn[]
                     (app-model/start! model
                                       (fn [model]
                                         (let [opts {
                                                      :channel (app-model/get-xctor-chan model)
                                                      :op-handler (partial ws/server-wf-handler model)
                                                      }
                                               xctor (app-model/get-xctor model)
                                               worker (wf/->AsyncWFProcessor xctor opts)]

                                           (wf/process-results! worker)
                                           )
                                         ))


                     )
                   ]
                  ]
        socket @(::socket local)
        ]
    [:div
       (ui/menubar "Server:"
                   (if socket
                     (into actions [["client ping"
                                     (fn[]
                                       (.send socket (ws/write-transit [:client-ping "Hello"]))

                                       )]])
                     actions
                     )
                   )]))


(rum/defcs <app-ui>
  < rum/reactive [local *STATE]

  ;; todo: put model in state, so it can be reset
  (let [model (:ui-model @*STATE)
        server (:server @*STATE)]

    [:div#app

     (<server-ui> model server)
     (<wf-ui>
       (cursor [:context])
       (cursor [:workflow])
       model

       )

     ]))


;; init

(when-not (::initialized @*APP-STATE)

  ;; (println "init!")


  (let [model (app-model/wf-state
                (cursor [:context])
                (cursor [:workflow])
                (cursor [:xctor])
                (cursor [:xctor-chan])
                )]

    (swap! *APP-STATE merge
           {:ui-model model
            :server (ws/ws-server "/api/websocket" model)
            ::initialized true})
    )

  )




(rum/mount (<app-ui> *APP-STATE)
           (. js/document (getElementById "app")))


; todo:

(add-watch *APP-STATE :watcher
  (fn [key atom old-state new-state]
    (rum/mount (<app-ui> *APP-STATE)
               (. js/document (getElementById "app")))))




(defn on-js-reload []
  ;; todo: force close all channels

  (.clear js/console))




