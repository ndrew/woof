(ns woof.ui.playground.ui
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
    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



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
   :woof.app/not-started [["start" start-fn]]

   :woof.app/done        [["finish" (fn[] ;; todo: reset wf
                                      (stop-fn)
                                      (reset-fn))]]

   :woof.app/running     (into actions
                               [[]
                                ["stop" stop-fn]])
   :woof.app/error       [["start" start-fn]
                          ; ["restart" reset-fn]
                          ]
   ; :woof.app/stopped     "error"
   })
