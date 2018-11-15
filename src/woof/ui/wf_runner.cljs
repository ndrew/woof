(ns woof.ui.wf-runner
  (:require
    [cljs.core.async :as async]
    [rum.core :as rum]

    [woof.core.runner :as runner]
    [woof.data :as d]
    [woof.wf-data :as wdata]

    [woof.example.big-wf :as big-wf]
    [woof.example.files-ui :as files-ws]
    [woof.example.infinite :as infinite]
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


    [woof.wfc :as wfc]
    [woof.ws :as webservice]

    [woof.xform :as x]
    [woof.ws :as webservice]

    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))





(declare init!)

(declare init-ui!)
(declare reset-ui!)



;; -------------------------- ui-state

;; although you can handle ui state as you want,
;; here is a default way of doing it



(defn ui-opts
  "returns an options map for the woof.wf with a before-process handler
  for the ui updates."
  [*STATE params]
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

        start-fn (fn [] (wf/process-results! processor))
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


      :history []
      :result {}

    ;; other
      :api (:api params)

      }

    )

  )

(defn ui-fn [*STATE <ui> & {:keys [auto-start]}]

  (fn [WF opts]
        (let [ui-wf (ui-wf-state *STATE WF opts)

              params  (:params ui-wf)
              start-fn (:start! ui-wf)
              ]

          ;; pass here initial ui state
          (init-ui! *STATE
                    ui-wf
                    <ui>)

          ;; auto-start
          (if auto-start
            (start-fn))

          ))
  )


(defn simple-popup [*STATE]

  (let [
         ;; todo: editing defaults
         defaults-fn (fn [] {:hello :world})
         ]

    (runner/run-wf
      defaults-fn
      simple-wf/wf!  ;; (fn [params] -> {:wf <wf>, :params {}})
      (partial ui-opts *STATE)
      (ui-fn *STATE
             (fn [*STATE]

               (default-ui/<wf-ui> "SIMPLE WORKFLOW:" *STATE))
             :auto-start false)
      )

    )

  )


;;
;; example of higher order wf for handling ui


(defn popup-wf [*STATE]
  ; todo:

  (runner/run-wf
      (fn []
        ;; defaults
        (popup/prepare-params))

      (partial popup/wf! *STATE)  ;; (fn [params] -> {:wf <wf>, :params {}})
      (partial ui-opts *STATE)

      (fn [WF opts]  ; WF is wf-impl

        (let [ui-wf (ui-wf-state *STATE WF opts)

              params  (:params ui-wf)
              start-fn (:start! ui-wf)
              ]


          (.log js/console ui-wf)

          ;; pass here initial ui state
          (init-ui! *STATE
                    ui-wf
                    (fn [*STATE]
                      (popup/<popup-ui> *STATE (:editor-chan params))))

          ;; auto-start
            (start-fn)

          )


        )

    )

  )








(defn config-ws [*STATE]
    ;; init-fn ; (fn [] => defaults )

  ;; wf-fn   ; (fn [params] -> {:wf <wf>, :params {}})
  ;; opts-fn ; (fn [params] -> {:opts <>, :params {}}

  ;; run-fn ; (fn [wf-impl opts])

  (runner/run-wf
      (fn []
        {}) ;; defaults
      (partial cfg-wf/wf! *STATE)  ;; (fn [params] -> {:wf <wf>, :params {}})
      (partial cfg-wf/opts-fn *STATE)

      (fn [WF opts]  ; WF is wf-impl
        (cfg-wf/ui-fn WF opts init-ui!))

    )
)






(defonce *UI-STATE (atom
    {
      ;; test workflows
      :basic-worflows [
                        ["simplest wf" simple-popup]
                        []
                        ["popup"   popup-wf ]

                        ;[]
                        ;["config" config-ws]

                        ;["ouroboros" ouroboros-wf]
                        ;["infinite" infinite-wf]
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


      ;; workflow specific map
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


(defn init-ui!
  [*STATE wf-map ui-fn]   ;; init fn
    (swap! *STATE merge
           {
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






(rum/defc <ui> < rum/reactive [*STATE]
  (let [{
          wf :wf
          ui  :rum-ui
          :or {ui (partial default-ui/<wf-ui> "WF:")}
        } @*STATE]

    (if-not wf
      (<wf-list> *STATE)
      (ui (rum/cursor-in *STATE [:wf])))))



