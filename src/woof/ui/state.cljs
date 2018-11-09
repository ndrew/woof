(ns woof.ui.state
  "state handling for the woof ui"
  (:require
    [cljs.core.async :as async]
    [woof.data :as d]
    [woof.wf :as wf]
    [woof.wfc :as wfc]
    [woof.xform :as x]
    [woof.utils :as u]

    [rum.core :as rum]
    [woof.ws :as webservice]

    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; although you can handle ui state as you want,
;; here is a default way of doing it



(defn ui-opts
  "returns an options map for the woof.wf with a before-process handler
  for the ui updates."
  [*STATE params]
  (let [cursor (partial rum/cursor-in *STATE)

        params { ;; provides state atom for the ui updates
                 :*state *STATE
                 }
        before-process (fn [wf-chan xtor]
                         (reset! (cursor [:wf :status]) :woof.app/running)
                         )

        op-handlers-map {

                   :wf-update (fn[data]
                                ; (swap! (cursor [:steps]) merge (first data))
                                ; (swap! (cursor [:result]) merge (second data))

                                )

                   :process (fn[data]
                              ;(swap! (cursor [:history]) conj
                              ;       (wdata/inline-results data))
                              ;(swap! (cursor [:result]) merge data)

                              )

                   :done (fn [data]
                           ;(swap! (cursor [:history]) conj
                           ;       (wdata/inline-results data))

                           (reset! (cursor [:wf :status]) :woof.app/done))

                   :error (fn [data]
                            (.error js/console "ERROR" data)
                            (reset! (cursor [:wf :status]) :woof.app/error))

                   }
        ]
    {
      :params params
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




(defn ui-wf
  ""
  [wf! opts-fn init-fn]

  (fn [*STATE]  ;; pass *STATE via params

    (let [
           initial-params (init-fn) ;; todo:

           ;; inits wf
           {
             actions :actions

             wf-fn :wf                 ;; workflow function
             wf-default-params :params
             } (wf! *STATE) ;; pass cursored value?

           ;; inits ws opts fn

           {
             params :params
             opts :opts
            } (opts-fn *STATE (merge wf-default-params initial-params))

           ]

        (init-fn (wf-fn params) opts)
      )
    )
  )

