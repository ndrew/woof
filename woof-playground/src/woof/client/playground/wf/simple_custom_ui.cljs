(ns ^:figwheel-hooks woof.client.playground.wf.simple-custom-ui
  (:require
    [cljs.core.async :as async]
    [rum.core :as rum]

    [woof.client.playground.ui.wf :as wf-ui]
    [woof.client.stateful :as st-wf]
    [woof.data :as d]
    [woof.wf :as wf]
    [woof.utils :as utils]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))

;;;;;
;; more complex example

(declare <custom-wf-ui>)

(defn initialize-test-wf! []
  (let [
        init-evt-loop (fn [params]
                        ;; this should prepare all stuff needed for ctx and steps
                        (let [evt-loop-chan (st-wf/&chan params (wf/rand-sid "evt-loop"))]
                             {
                              ::evt-loop-chan evt-loop-chan ;; keep the evt loop chan
                              }))

        ctx-evt-fn (fn [params]
                     {
                      :evt-loop {
                                 :fn       (fn [in-chan] in-chan)
                                 :infinite true
                                 :expands? true
                                 }

                      :collect {
                                :fn (fn [v]
                                      v)
                                :collect? true
                                }
                      })

        ]
    {

     :init-fns  [st-wf/chan-factory-init-fn
                 init-evt-loop]

     :ctx-fns   [ctx-evt-fn

                 (fn [params]
                   {
                    :print {:fn (fn [v]

                                  v)}

                    :timer {:fn       (fn [max]
                                        (let [ch (async/chan)]
                                             (go
                                               (dotimes [n max]
                                                 (async/put! ch {:t (utils/now)})
                                                 (async/<! (utils/timeout 1000))
                                                 )

                                               (async/put! ch {:msg "Timer ended"})
                                               )
                                             ch)
                                        )
                            :infinite true
                            }

                    :format-time {:fn (fn [t]
                                        (if-let [tt (get t :t)]
                                                (.toString (js/Date. tt))
                                                "Timer ended!")
                                        )}

                    }
                   )]

     :steps-fns [(fn [params]
                   {
                    ::evt-loop [:evt-loop (::evt-loop-chan params)]
                    ::timestamps [:collect ::evt-loop]
                    }
                   )

                 (fn [params]
                   {

                    ;; ::initial-print [:print "hello"]

                    ::timer  [:timer 100]
                    ::t [:format-time ::timer]

                    })

                 ]

     :opt-fns   [;; ls/ls-opts-fn
                 st-wf/chan-factory-opts-fn
                 ]

     }
    )

  )


(defn initialize-test-wf-w-state! [*wf]
  {

   ;; how to provide a custom ui for actions - we need to pass state here
   :ui-fn       (partial wf-ui/<wf-UI> <custom-wf-ui>)

   :title       "Workflow with Event Loop and custom UI"

   :explanation [:div.explanation
                 [:p "Woof workflows can simulate event loop via infinite expand step. So the UI can send new steps to the workflow,"
                  "workflow run these steps, modify state which will become reflected in the UI."
                  ]

                 [:p "By clicking the button - we can add new steps to event loop. Also available via 'ui event' workflow action"]
                 ]

   :wf-actions  {
                 ; :not-started []
                 :running [

                           ["simulate click" (fn []
                                               (let [loop-chan (st-wf/&wf-init-param *wf ::evt-loop-chan)]
                                                    (async/put! loop-chan
                                                                {(wf/rand-sid "ui-") [:print (utils/now)]})
                                                    )
                                               )]

                           ["send to evt loop"
                            (fn []
                              (let [step (d/to-primitive (js/prompt "provide step as [:handler-id <params>], e.g. [:print \"some value\"]"))]
                                   (let [loop-chan (st-wf/&wf-init-param *wf ::evt-loop-chan)]
                                     (async/put! loop-chan
                                                 {(wf/rand-sid "emit-") step})
                                     )
                                   )
                              )
                            ]

                           ]
                 ; :done        []
                 }

   }
  )



(defn wf-with-ui-initializer [*NODE]
  (merge
    (initialize-test-wf!)
    (initialize-test-wf-w-state! *NODE))
  )



;;; ui

;; exposes init param by it's key
(defn &wf-init-param [wf k]
  (get-in wf [:runtime :initial :params k]))

(rum/defcs <custom-wf-ui> < rum/reactive
                            (rum/local true ::inline-results?)
                            (rum/local true ::sort-results?)
  [local *wf]

  (let [wf @*wf]
    [:div

     (if (not= :not-started (:status wf))
       [:div


        [:.timer "Timer — " (get (:result wf) ::t "")]

        [:hr]

        [:div
         "Event loop test: "
         [:button {:style {:font-size "12pt"}
                   :on-click (fn [e]
                               (let [loop-chan (&wf-init-param wf ::evt-loop-chan)]
                                    (async/put! loop-chan
                                                {(wf/rand-sid "click-") [:print (utils/now)]})
                                    )
                               )}
          "Click me!"]
         ]
        (into [:.dates [:header {:style {:margin-top "1rem"}} "Click log (showing last 10 clicks)"]]
              (map
                (fn [t]
                  [:pre (str "click — " t)])
                (take 10 (reverse (get-in (:result wf) [::timestamps] []))))
              )


        [:hr]
        ]
       )

     [:pre "↓ this is the default UI for displaying wf results ↓"]
     (wf-ui/<default-body> *wf)

     ]
    )

  )

