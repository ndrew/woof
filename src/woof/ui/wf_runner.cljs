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
    [woof.wf.popup.ui :as popup]
    [woof.wfc :as wfc]
    [woof.ws :as webservice]

    [woof.xform :as x]
    [woof.ws :as webservice]

    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; -------------------------- ui-state

;; although you can handle ui state as you want,
;; here is a default way of doing it



(defn ui-opts
  "returns an options map for the woof.wf with a before-process handler
  for the ui updates."
  [*STATE params]
  (let [cursor (partial rum/cursor-in *STATE)

        *result (cursor [:wf :result])
        *steps (cursor [:wf :steps])

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
                              ;(swap! (cursor [:history]) conj
                              ;       (wdata/inline-results data))
                              (swap! *result merge data)

                              )

                   :done (fn [data]
                           ;(swap! (cursor [:history]) conj
                           ;       (wdata/inline-results data))

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






;; --------------------------

;; UI playground

;; there should be a way to have some kind of customizable UI for workflows:

;; so far
;;
;; + ui workflow consists of:
;;
;;   * workflow arguments
;;       will be passed as & {:key} args to further 'constructors'
;;
;;     * steps-fn - provides initial steps
;;        usually there will be infinite expand action - for ui loop
;;
;;     * context  - provides context map
;;        usually more specific context will be merged to a more generic context map
;;
;;     * actions
;;        provides specific start and stop function for current workflow
;;        provides list of available actions for wf per status (not-started, working, error, .. )
;;
;;     * ui
;;        provides a custom rum component for wf or generic one
;;





;;





(defn default-ui-fn [header & r]
  (fn [*STATE]
    (default-ui/<wf-ui> header *STATE)))



;;
;; main state atom for workflow runner

(declare init!)
(declare init-ui!)

;; wf constructor ui


;; wf! opts-fn init-fn



;; create ui only wf - for testing runner


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


;;
;; example of higher order wf for handling ui

(defn popup-wf [*STATE]
  ; todo:

  (runner/run-wf
      (fn []
        (popup/prepare-params)
        ) ;; defaults
      (partial popup/wf! *STATE)  ;; (fn [params] -> {:wf <wf>, :params {}})
      (partial ui-opts *STATE)
      (fn [WF opts]  ; WF is wf-impl

        (let [params (wfc/get-params WF)
              context-map (wfc/get-context-map WF)
              steps (wfc/get-steps WF)

              {
                *wf-state :*state ;; get state via params?
                actions :actions
                } params

              xtor (wfc/wf-xtor WF)

              start-fn (fn []
                         (wf/process-results! (wf/->ResultProcessor xtor opts)))

              stop-fn  (fn []
                         (wf/end! xtor))

              reset-fn (fn []
                         (init-ui! *STATE))

              ]


          ;; pass here initial ui state

         ; (js-debugger)

          (init-ui! *STATE
                      {
                        :steps steps
                        :context-map context-map

                        :api (:api params)

                        :params params

                        ; :args args
                        :status :woof.app/not-started
                        :status-actions (status-actions start-fn stop-fn reset-fn actions)

                        :start! start-fn
                        :stop! stop-fn
                        :reset! reset-fn



                        :result {}

                        ;; the place where workflow can store data
                        :wf-state {}


                        }
                      (fn [*STATE]
                        (popup/<popup-ui> *STATE (:editor-chan params))
                        )


                      )

          ;; auto-start
            (start-fn)

          )


        )

    )

  )










(defonce *UI-STATE (atom
    {
      ;; test workflows
      :basic-worflows [
                        ["popup"   popup-wf ]
                        ;[]
                        ;["config" config-ws]
                        ;[]

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



(defn init-ui!
  ([*STATE]                 ;; reset function
   (init!))

  ([*STATE wf-map ui-fn]   ;; init fn
    (swap! *STATE merge
           {
             :wf wf-map
             :rum-ui ui-fn
             })

   )

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
          :or {ui default-ui/<wf-ui>}
        } @*STATE]

    (if-not wf
      (<wf-list> *STATE)
      (ui (rum/cursor-in *STATE [:wf])))))



