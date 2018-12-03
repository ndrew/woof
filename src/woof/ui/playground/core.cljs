(ns woof.ui.playground.core
  (:require
    [cljs.reader]
    [rum.core :as rum]
    [woof.core.runner :as runner]
    [woof.data :as d]
    [woof.ui :as ui]
    [woof.ui.wf :as default-ui]
    [woof.utils :as u]
    [woof.wf :as wf]
    [woof.wf-data :as wdata]
    ;[woof.wf-ui :as wf-ui]

    [woof.wf.edn-editor.frontend :as cfg-wf]
    [woof.wf.popup.example1 :as popup]
    [woof.wf.simple.infinite :as infinite-wf]
    [woof.wf.simple.wf :as simple-wf]
    [woof.wfc :as wfc]
    [woof.ws :as webservice]
    ; [woof.xform :as x]

    [woof.ui.playground.ui :as pui]
    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; -------------------------- ui-state

;; although you can handle ui state as you want,
;; here is a playground for running wfs


;; --- exports

(declare <ui>)
(declare init!)

(declare reload!)

(declare run-ui!) ;; run
(declare reset-ui!) ;; end






(defonce *UI-STATE (atom
                     {
                      ;; test workflows
                      :basic-workflows   []
                      :complex-workflows [


                                          ;["UI loop" ui-loop-wf]
                                          ;[]
                                          ;["file browser" files-ws-wf]
                                          ;[]
                                          ;[]
                                          ;["rpc via webservice" ws-wf]
                                          ;[]
                                          ]

                      ;;;;;;;;

                      :screen            :wf-selector


                      ;; workflow specific map
                      :compile           nil

                      :wf                nil

                      ;; current ui
                      :rum-ui            nil

                      ;;;;;;;;;;
                      :app               {}
                      }))




;; ----------




;;
;; main state atom for workflow runner


(defn- ui-wf-state [*STATE WF opts]

  (let [params (wfc/get-params WF)

        xtor (wfc/wf-xtor WF)
        processor (wf/->ResultProcessor xtor opts)

        start-fn (fn []
                   (wf/process-results! processor)
                   ;; :status-actions
                   )
        stop-fn  (fn [] (wf/end! xtor))
        reset-fn (fn [] (reset-ui! *STATE))

        ]

    {

   ;; wf
      :params params

      :steps (wfc/get-steps WF)
      :context-map (wfc/get-context-map WF)


    ;; ui
      :status :woof.app/not-started
      :status-actions (pui/status-actions start-fn stop-fn reset-fn
                                      (:actions params))


    ;; lifecycle
      :start! start-fn
      :stop! stop-fn
      :reset! reset-fn


    ;; opts
      :opts opts


    ;; default wf stuff
      :history []
      :result {}

      }

    )

  )


(defn ui-fn [*STATE <ui> & {:keys [auto-start state-fn]}]

  (fn [WF opts]
        (let [f (if (fn? state-fn)
                  state-fn
                  identity)
              ui-wf (f (ui-wf-state *STATE WF opts))

              ]

          ;; pass here initial ui state
          (run-ui! *STATE
                    ui-wf
                    <ui>)

          ;; auto-start
          (if auto-start
            ((:start! ui-wf)))

          ))
  )



(defn init-ui! [params-fn
                compile-fn
                ]
  (fn [*STATE]
    (let [params (params-fn)]
      (swap! *STATE merge
             {
               :screen :compile
               :compile {
                          :params params
                          :fn compile-fn
                          }
               })
      )
    )
  )


;;
;; wfs
;;






(defn infinite-wf-runner [*STATE params]
  ;

    (runner/run-wf
      (fn[] params)
      infinite-wf/wf!  ;; (fn [params] -> {:wf <wf>, :params {}})

      (fn [params]
        (runner/merge-full-opts
          (infinite-wf/opts-fn params)
          (pui/ui-opts *STATE params)))


      (ui-fn *STATE
             (fn [*STATE]

               (default-ui/<wf-ui> "INFINITE:" *STATE))
             :auto-start false)
      )
  )

(def infinite-wf (init-ui!
                   infinite-wf/prepare-params!
                   infinite-wf-runner))


;;
;; example of higher order wf for handling ui





(defn popup-wf-runner [*STATE params]
  ; todo:

  (runner/run-wf
      (fn [] params)
      (partial popup/wf! *STATE)  ;; (fn [params] -> {:wf <wf>, :params {}})
      (partial pui/ui-opts *STATE)

      (fn [WF opts]  ; WF is wf-impl

        (let [ui-wf (ui-wf-state *STATE WF opts)
              params  (:params ui-wf)
              start-fn (:start! ui-wf)
              ]


          ;; pass here initial ui state
          (run-ui! *STATE
                    ui-wf
                    (fn [*STATE]
                      (popup/<popup-ui> *STATE (:editor-chan params))))

          ;; auto-start
            (start-fn)
          )
        )
    )
  )


(def popup-wf (init-ui!
                popup/prepare-params
                popup-wf-runner))



(defn config-ws-custom-ui [*UI-STATE]
  (let [data @*UI-STATE]
    [:div
     [:header (d/pretty (get-in @*UI-STATE [:wf-state :file :path]))]

     [:pre (get-in @*UI-STATE [:wf-state :file :contents])]
     ]
    )
  )



(defn config-ws-runner [*STATE params]
    ;; init-fn ; (fn [] => defaults )

  ;; wf-fn   ; (fn [params] -> {:wf <wf>, :params {}})
  ;; opts-fn ; (fn [params] -> {:opts <>, :params {}}

  ;; run-fn ; (fn [wf-impl opts])

  (let [init-ui-state-fn (fn [ui-wf]
                           (merge ui-wf
                                  {    ;; others
                                    :api (get-in ui-wf [:params :api])
                                    :wf-state {:file {}}
                                    }
                                  )
                           )
        done-fn (fn[data]
                  (.log js/console @*STATE)
                  (.log js/console data)


                  (swap! (rum/cursor-in *STATE [:app])
                        assoc :config
                              (cljs.reader/read-string
                                (get-in @*STATE [:wf :wf-state :file :contents])))
                  ;; wf is done

                  )
        ]

    (runner/run-wf
      (fn [] params) ;; defaults

      (partial cfg-wf/wf! (rum/cursor-in *STATE [:wf]))  ;; (fn [params] -> {:wf <wf>, :params {}})

      (fn [params]
        (runner/merge-full-opts
          (pui/ui-opts *STATE params
                   :done done-fn)
          ;; webservice is last - because it's channel
          (webservice/ws-opts "/api/config" *STATE params) ;; (cfg-wf/opts-fn *STATE params)
          ))

      (ui-fn *STATE
             (fn [*STATE]
               (default-ui/<wf-ui> "CONFG:" *STATE
                                   :results? false
                                   :custom-ui config-ws-custom-ui
                                   )
               )
             :auto-start true
             :state-fn init-ui-state-fn)
      )
    )
  )


(def config-ws (init-ui!
                  (fn [] {
                           :initial-steps {
                                            ::yo [:log "hello there"]

                                            ;; todo: wait for ws to init
                                            ::init [:send! [:current nil]]
                                            }

                           })
                  config-ws-runner))







(defn reset-ui! [*STATE]
  (init!))


(defn run-ui!
  [*STATE wf-map ui-fn]   ;; init fn
    (swap! *STATE merge
           {
             :screen :run
             :wf wf-map
             :rum-ui ui-fn
             })

)









;; shows list of available wfs

(rum/defc <wf-list> < rum/reactive [*STATE]
  (let [inject-state (fn[item]
                        (if-let [[h action] item]
                          (if action
                            [h (partial action *STATE)]
                            item)))

         { basic-items   :basic-workflows
           complex-items :complex-workflows} @*STATE]


    [:div
     [:div (ui/menubar "Simple workflows:" (map inject-state basic-items))]
     ;[:hr]
     ;[:div (ui/menubar "Complex workflows:" (map inject-state complex-items))]
     ]
    ))



(rum/defc <wf-compilation> < rum/reactive [*STATE]

  [:div
   [:header "*Compilation*"]
   (into [:div.params-ui
             [:header "Params:"]]

            (map (fn[[k v]]
                   [:div
                    [:span (d/pretty k)]

                    (ui/data-editor (fn[new-v]
                                      ;(.log js/console new-v)
                                      (reset! (rum/cursor-in *STATE [:compile :params k]) new-v))
                                    v)
                    ]
                   )
                 @(rum/cursor-in *STATE [:compile :params])
                 ))

      (ui/menubar "Compile WF:" [
                               ["run" (fn []
                                        (let [{ compile-fn :fn
                                                params :params
                                              } @(rum/cursor-in *STATE [:compile])]

                                          (compile-fn *STATE params)
                                          )
                                        )]
                               ])


   ]

)


(rum/defc <ui> < rum/reactive [*STATE]
  (let [{
          screen :screen
        } @*STATE]

    [:div
     [:div "woof playground allows you to execute ui workflows. "
      [:span {:style {:color "gray"
                      }} "UI " (d/pretty screen)]
      ]


     [:h5 "App state:"]
     [:pre (d/pretty (get-in @*STATE [:app]))]

     [:hr]
     (condp = screen
       :wf-selector (<wf-list> *STATE)
       :compile (<wf-compilation> *STATE)
       :run (let [{
                    ui  :rum-ui
                    :or {
                         ui (partial default-ui/<wf-ui> "WF:")}
                    } @*STATE]
              (ui (rum/cursor-in *STATE [:wf]))
              )

       [:h1 "Unknown screen" ]
       )
     ]

    ))


;; initializers

(defn get-basic-wfs []
  (println "get wfs1")
  [
   ["simplest wf"
    (init-ui!
      (fn [] {:hello :world})
      (fn [*STATE params]
        (runner/run-wf
          ;; return the params from 'compile' stage
          (fn [] params)

          ;; (fn [params] -> {:wf <wf>, :params {}})
          simple-wf/wf!

          ;;
          (fn [params]
            (println "PUI" params)
            (pui/ui-opts *STATE params
                         :done (fn [data]
                                    (println "GOT DATA" data)
                                    data
                                    (swap! *STATE merge {:app {:result data}} )
                                    )))
          ;;
          (ui-fn *STATE
                 (fn [*STATE]
                   [:div
                    "finite workflow"
                    (default-ui/<wf-ui> "SIMPLE WORKFLOW:" *STATE)]
                   )
                 :auto-start true
                 )
          )
        ))
    ]
   []
   []
   []
   ["1.config" config-ws]
   []
   ["infinite" infinite-wf]
   []
   ["popup" popup-wf]
   []
   ;["ouroboros" ouroboros-wf]
   ;["expand" expand-wf]
   ]
  )





;; API


(defn init!
  "initializes ui state"
  ([]
   (println "INIT")
   (swap! *UI-STATE merge
          {

           ;; ui state
           :screen :wf-selector

           ;; wf state
           :compile nil
           :wf nil ;; reset the wf state
           })
    )
  ([mount-fn]
  (println "MOUNT")
   (add-watch *UI-STATE :woof-main
              (fn [key atom old-state new-state]
                (mount-fn)))

   (when-not (::initialized @*UI-STATE)
     (swap! *UI-STATE merge {}
                            {::initialized true
                             ;; data
                             :basic-workflows (get-basic-wfs)

                             })))
  )


(defn reload! []
  ;(remove-watch *UI-STATE :woof-main)
  (swap! *UI-STATE merge {
                          ::initialized false
           }))


(def <app> #(<ui> *UI-STATE))



;; -API

