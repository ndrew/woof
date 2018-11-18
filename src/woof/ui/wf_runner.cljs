(ns woof.ui.wf-runner
  (:require
    [cljs.core.async :as async]
    [rum.core :as rum]

    [woof.core.runner :as runner]
    [woof.data :as d]
    [woof.wf-data :as wdata]

    [woof.example.big-wf :as big-wf]
    [woof.example.files-ui :as files-ws]
;    [woof.example.infinite :as infinite]
    [woof.example.ouroboros :as ouroboros]
    [woof.example.ui-loop :as ui-loop]
    [woof.example.ws :as ws] ;; todo: rename

    [woof.ui :as ui]
    [woof.ui.steps :as steps-ui]
    [woof.ui.wf :as default-ui]

    [woof.utils :as u]

    [woof.wf :as wf]
    [woof.wf-ui :as wf-ui]


    [woof.wf.edn-editor.frontend :as cfg-wf]
    [woof.wf.popup.example1 :as popup]
    [woof.wf.simple.wf :as simple-wf]
    [woof.wf.simple.infinite :as infinite-wf]


    [woof.wfc :as wfc]
    [woof.ws :as webservice]

    [woof.xform :as x]
    [woof.ws :as webservice]

    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))





(declare init!)

;; run
(declare run-ui!)
;; end
(declare reset-ui!)


;; -------------------------- ui-state

;; although you can handle ui state as you want,
;; here is a default way of doing it



(defn ui-opts
  "returns an options map for the woof.wf with a before-process handler
  for the ui updates."
  [*STATE params & {:keys [done]}]
  (let [cursor (partial rum/cursor-in *STATE)

        *result  (cursor [:wf :result])
        *history (cursor [:wf :history])

        *steps  (cursor [:wf :steps])
        *status (cursor [:wf :status])

        all-params (merge params
                          {
                             ;; provides state atom for the ui updates
                             ;; :*state *STATE ; just in case
                          })

        before-process (fn [wf-chan xtor]
                         (reset! *status :woof.app/running)
                         )

        op-handlers-map {

                   :wf-update (fn[data]
                                (swap! *steps merge (first data))
                                (swap! *result merge (second data))

                                )

                   :process (fn[data]
                              (swap! *history conj
                                     (wdata/inline-results data))
                              (swap! *result merge data)

                              )

                   :done (fn [data]
                           (if (fn? done)
                             (done data))

                           (swap! *history conj
                                  (wdata/inline-results data))
                           (reset! *status :woof.app/done))

                   :error (fn [data]
                            (.error js/console "ERROR" data)
                            (reset! *status :woof.app/error))

                   }
        ]
    {
      :params all-params
      :opts {
              :before-process before-process
              :op-handlers-map op-handlers-map
              }
      }
    )
  )



(defn status-actions [start-fn stop-fn reset-fn actions]
  {
    :woof.app/not-started [
                            ["start" start-fn]
                            ]

    :woof.app/done        [
                            ["finish" (fn[]
                                        (stop-fn)
                                        (reset-fn))]
                            ;; todo: reset wf
                            ]

    :woof.app/running     (into actions
                                [[]
                                 ["stop" stop-fn]
                                 ]
                                )
    :woof.app/error       [
                            ["start" start-fn]
                            ; ["restart" reset-fn]
                            ]
    ; :woof.app/stopped     "error"
    })






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
      :status-actions (status-actions start-fn stop-fn reset-fn
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





(defn simple-popup-runner [*STATE params]
  (runner/run-wf
    (fn [] params)
    simple-wf/wf!  ;; (fn [params] -> {:wf <wf>, :params {}})
    (partial ui-opts *STATE)
    (ui-fn *STATE
           (fn [*STATE]
             (default-ui/<wf-ui> "SIMPLE WORKFLOW:" *STATE))
           :auto-start false)
    ))


(def simple-popup (init-ui!
                    (fn [] {:hello :world})
                    simple-popup-runner))



(defn infinite-wf-runner [*STATE params]
  ;

    (runner/run-wf
      (fn[] params)
      infinite-wf/wf!  ;; (fn [params] -> {:wf <wf>, :params {}})

      (fn [params]
        (runner/merge-full-opts
          (infinite-wf/opts-fn params)
          (ui-opts *STATE params)))


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
      (partial ui-opts *STATE)

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
                  ;; wf is done

                  )
        ]

    (runner/run-wf
      (fn [] params) ;; defaults

      (partial cfg-wf/wf! (rum/cursor-in *STATE [:wf]))  ;; (fn [params] -> {:wf <wf>, :params {}})

      (fn [params]
        (runner/merge-full-opts
          (webservice/ws-opts "/api/config" *STATE params) ;; (cfg-wf/opts-fn *STATE params)
          (ui-opts *STATE params
                   :done done-fn)))

      (ui-fn *STATE
             (fn [*STATE]
               (default-ui/<wf-ui> "CONFG:" *STATE
                                   :results? false
                                   :custom-ui (fn [*UI-STATE]
                                                [:div
                                                 (d/pretty
                                                   (keys @*UI-STATE))
                                                 ]
                                                )
                                   )
               )
             :auto-start true
             :state-fn init-ui-state-fn)
      )
    )
  )


(def config-ws (init-ui!
                  (fn [] {
                           :initial-steps {::yo [:log "hello there"]}
                           })
                  config-ws-runner))



(defonce *UI-STATE (atom
    {
      ;; test workflows
      :basic-worflows [
                        ["simplest wf" simple-popup]
                        []
                        ["infinite" infinite-wf]
                        []
                        ["popup"   popup-wf ]
                        []
                        ["config" config-ws]

                        ;["ouroboros" ouroboros-wf]

                        ;["expand" expand-wf]
                        ]
      :complex-workflows [


                            ;["UI loop" ui-loop-wf]
                            ;[]
                            ;["file browser" files-ws-wf]
                            ;[]
                            ;[]
                            ;["rpc via webservice" ws-wf]
                            ;[]
                            ]

      :screen :wf-selector


      ;; workflow specific map
      :compile nil

      :wf nil

      ;; current ui
      :rum-ui nil

}))



(declare <ui>)


;; API


(defn init!
  "initializes ui state"
  ([]
   (swap! *UI-STATE merge
         {
           :screen :wf-selector

           :compile nil
           :wf nil ;; reset the wf state
         })
   )
  ([mount-fn]

  (add-watch *UI-STATE :woof-main
             (fn [key atom old-state new-state]
               (mount-fn)))

  (when-not (::initialized @*UI-STATE)
    (swap! *UI-STATE merge {} {::initialized true})))
  )


(def <app> #(<ui> *UI-STATE))



;; -API



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

         { basic-items :basic-worflows
           complex-items :complex-workflows} @*STATE]


    [:div
     [:div (ui/menubar "Simple workflows:" (map inject-state basic-items))]
     ;[:hr]
     ;[:div (ui/menubar "Complex workflows:" (map inject-state complex-items))]
     ]
    ))



(rum/defc <wf-compilation> < rum/reactive [*STATE]

  [:div
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


    (condp = screen
      :wf-selector (<wf-list> *STATE)
      :compile (<wf-compilation> *STATE)
      :run (let [{
                  wf :wf
                  ui  :rum-ui
                  :or {ui (partial default-ui/<wf-ui> "WF:")}
                  } @*STATE]
             (ui (rum/cursor-in *STATE [:wf]))
             )

      [:pre "????"]
      )

    ))



