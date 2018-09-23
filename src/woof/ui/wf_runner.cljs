(ns woof.ui.wf-runner
  (:require
    [cljs.core.async :as async]
    [cljs.test :as test :refer-macros [deftest is testing run-tests async]]

    [clojure.data :as cd]


    [goog.string :as gstring]
    [goog.string.format]

    [rum.core :as rum]

    [woof.app-data :as app-model]

    [woof.data :as d]
    [woof.graph :as g]
    [woof.wf :as wf]
    [woof.wf-data :as wdata]

    [woof.ui :as ui]
    [woof.wf-ui :as wf-ui]

    [woof.ui.context :as ctx-ui]
    [woof.ui.steps :as steps-ui]
    [woof.ui.results :as r]

    [woof.utils :as u]

    [woof.test-data :as test-data]
    ; [woof.wf-tester-ui :as tester-ui]

    [woof.example.ui-loop :as ui-loop]
    [woof.example.ws :as ws]
    [woof.example.popup :as popup]
    )


  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]
    [woof.utils-macros :refer [put!?]]))






;;
;; main state atom for workflow runner

(defonce *UI-STATE (atom {

  :status :woof.app/not-started

  :wf nil

  :ui-chan nil
  :history []

  :result {}
  :steps {}

  :rum-ui nil
}))




(defn init! []
  (swap! *UI-STATE merge
         {
           :wf nil

           :status :woof.app/not-started
           :history []

           :result {}
           :steps {}

           })
  )




;; what if pass a multi method

(defn init-runner-wf [*STATE
                    ;endpoint-url ;; "/api/websocket"
                    ;responce-fn  ;; ws output to steps

                    params

                    context-fn
                    steps-fn
                    actions-fn

                    ui-fn
                    ]

  ;; todo have & params - for ui chan


  (let [
        args (apply concat params)

        cursor (partial rum/cursor-in *STATE)

        context-map (apply context-fn args)
        steps (apply steps-fn args)

        xtor (wf/build-executor (wf/make-context context-map) steps)

        processing-opts { ;:execute start-wf-with-transducers!
            ; :before-process before-processing!
            ;; :process-handler op-handler
            :op-handlers-map {

                               :wf-update (fn[data]
                                            ; (swap! *resulting-map assoc-in [::steps] (first data))
                                            ; (swap! *resulting-map assoc-in [::result] (second data))
                                            (swap! (cursor [:steps]) merge (first data))
                                            (swap! (cursor [:result]) merge (second data))
                                            )

                               :process (fn[data]
                                          (swap! (cursor [:history]) conj
                                                 (wdata/inline-results data))

                                          (swap! (cursor [:result]) merge data)
                                          )
                               :done (fn [data]
                                       (swap! (cursor [:history]) conj
                                                 (wdata/inline-results data))

                                       (reset! (cursor [:status]) :woof.app/done)
                                        ; (async/close! @(cursor [:ui-chan]))
                                       )
                               :error (fn [data]
                                        (.error js/console "ERROR" data)
                                        (reset! (cursor [:status]) :woof.app/error)
                                        )

                               }
            ;; :timeout 1000
            }

         ;; how to return these
         {
           wf-stop-fn :stop!
           wf-start-fn :start!
           wf-reset-fn :reset!

           actions :actions
           } (apply actions-fn args)

        start-fn (fn[]
                   (let [f (fn []
                             (let [proc (wf/->ResultProcessor xtor processing-opts)]
                               (wf/process-results! proc))

                             (reset! (cursor [:status]) :woof.app/running))]

                   (let [v (if wf-start-fn (wf-start-fn))]
                     (if (u/channel? v)
                       (go
                          (if-let [nu-v (async/<! v)]
                            (f)))
                       (f))))
                   )

        start-action ["start" start-fn]

        stop-fn (fn[]
                      (wf/end! xtor)

                      (if wf-stop-fn
                        (wf-stop-fn))
                      )

        stop-action ["stop" stop-fn]


        reset-fn (fn []
                    (if wf-reset-fn
                       (wf-reset-fn))

                    (init!))


        status-actions  {
                        :woof.app/not-started [start-action]

                        :woof.app/done        [["FINISH" reset-fn]]

                        :woof.app/running     (into actions [ [] stop-action])
                        ; :woof.app/stopped     "error"
                        :woof.app/error       [start-action ["RESTART" reset-fn]]
                        }

        ]


    (swap! *STATE merge
           {
             :wf {
                   :status-actions status-actions

                   :steps steps
                   :context-map context-map

                   ;; :xtor xtor

                   :start! start-fn
                   :stop! stop-fn

                   }

             :status :woof.app/not-started
             :history []

             :rum-ui (apply ui-fn args)
             })
  )
)



(rum/defcs <wf-ui> < rum/reactive
  [local *STATE]

  (let [cursor (partial rum/cursor-in *STATE)
        {status :status
         wf :wf
         full-history :history} @*STATE]
    [:div.wfui
     [:h5 "Example of using workflow with infinite expand handler as ui-loop."]



     (ctx-ui/<context> (cursor [:wf :context-map]))
     (steps-ui/<steps> (cursor [:wf :steps]) @(cursor [:wf :context-map]))

     (wf-ui/<wf-menu-ui> "wf:" status @(cursor [:wf :status-actions]))

     (let [history (reverse @(cursor [:history]))]
       [:.log

        (r/<wf-results-ui> "result"
                     @(cursor [:result])
                     @(cursor [:steps])
                     (atom {:pre   {}
                             :post {}}) ;; *editors
                     )

        [:div
          [:h2 "result:"]
          [:pre (d/pretty (first history))]
          [:h4 "last added"]
          [:pre
           (let [[cur prev] (first (partition 2 history))
                 [added _ _] (cd/diff cur prev)
                 ]
             (d/pretty added)

             )
           ]
         ]
        ]
       )

     ]
    )
  )







;wf runner ui prototype:
;* representing wf as:
;    - params...
;    - (context-fn params...) -> context map
;    - (step-fn params...) -> initial steps
;    - (actions-fn params...) -> actions map
;where action map is
;{ :start! (fn[]) -> channel/nil ;  if channel — wait for it before starting wf
;  :stop! (fn[]) -> ; clean-up
;  :reset! (fn[]) -> ; some global clean-up
;  :actions <menu-actions> ; available actions during wf in format as for menus
;}


(defn default-ui-fn []
  (fn [*STATE]
    (<wf-ui> *STATE)))

(defn ui-loop-wf [*STATE]
  (init-runner-wf *STATE
    {:ui-chan (async/chan)}
    ui-loop/context-map-fn
    ui-loop/steps-fn
    ui-loop/actions-fn
    default-ui-fn)
  )


(defn popup-wf [*STATE]
  (init-runner-wf *STATE
    {
       :ui-chan (async/chan)
       :editor-chan (async/chan)
     ; :state *STATE
     }
    popup/context-map-fn
    popup/steps-fn
    popup/actions-fn
    popup/ui-fn)
  )



(defn ws-wf [*STATE]
  (init-runner-wf *STATE
    (ws/prepare-params! "/api/websocket")
    ws/context-map-fn
    ws/steps-fn
    ws/actions-fn
    default-ui-fn
  )
)



;; wf can provide its own ui

(rum/defcs <wf-runner-ui> < rum/reactive

  [local *STATE]

  (let [{wf :wf
         ui  :rum-ui} @*STATE]

    (if wf
      (if ui
        (ui *STATE)
        (<wf-ui> *STATE))
      [:div
       (ui/menubar "WF:" [
                           ["popup" (partial popup-wf *STATE)]
                           ["UI loop" (partial ui-loop-wf *STATE)]
                           ["WS" (partial ws-wf *STATE)]
                           ])]
      )

))
