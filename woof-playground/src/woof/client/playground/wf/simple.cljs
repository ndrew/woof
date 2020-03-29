(ns ^:figwheel-hooks woof.client.playground.wf.simple
  (:require
    [rum.core :as rum]

    ;; v2 deps
    [woof.client.stateful :as st-wf]

    [woof.client.playground.ui :as ui]
    [woof.wf :as wf]
    [cljs.core.async :as async]
    [woof.utils :as u]
    [woof.client.playground.ui.wf :as wf-ui]
    [woof.base :as base])
  (:import [goog.net.XhrIo ResponseType])

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;;
;; alpha workflow example


;; here we test the idea of WF initializer function:
;;   we need to return a WF map containing:
;;    * necessary stuff: init-fn/ctx-fn/steps-fn
;;    * optional stuff:
;;        ui fn, wf actions, etc
;;        also we pass WF atom where this map will live

;; todo: come with better wf example

(defn basic-wf-initializer [*SWF]
  {

   :init-fns    [(fn [params]
                   ;; these don't matter now
                   {:IN :some-wf-parameters}
                   )]

   :title       "Finite WF with default UI"

   :explanation [:div.explanation

                 [:p "This is an introductory playground workflow. Playground provides the workflow with a possibility to have an UI."]

                 [:p "This workflow is finite, that is it will stop after all steps are processed."]

                 [:p "Hypothesis: Function composition (f(g(x))) can be done with woof workflows."
                  "Example of this can be map enrichment (like ring middle-ware)"]

                 [:pre {:style {:font-size "10pt"}} [:code
"
// start with some initial data passed to :id step handler
  ::value [:id {:initial :map}]

// do f(value)
  ::f [:f ::value]

// do z(f(value)) — z is async, so further steps will wait for the result
  ::z [:z-async ::f]

// do g(z(f(value)))
  ::g [:g ::z]
"
                  ]]

                 ]


   :ctx-fns     [(fn [params]
                   {

                    :id {:fn identity}

                    :f {:fn (fn [v]
                              (prn ":F")
                              (assoc v :f true)
                              )}

                    :g {:fn (fn [v]
                              (prn ":G")
                              (assoc v :g true))}


                    :z-async {:fn (fn [v]
                                    (let [ch (async/chan)
                                          t 3000 ]

                                         (prn ":Z - starting")
                                         (go
                                           (async/<! (u/timeout t))
                                           (prn ":Z - done")
                                           (async/put! ch (assoc v :z true)))

                                         ch))}

                    }
                   )]

   :steps-fns   [(fn [params]
                   {

                    ::value [:id {:initial :map}]

                    ::f [:f ::value]

                    ::z [:z-async ::f]

                    ::g [:g ::z]


                    })

                 ]
   :opt-fns     []


   :wf-actions  {
                 :done [["log WF state" (fn []
                                                      (prn "Workflow is ready. WF state is:")
                                                      (.log js/console @*SWF))
                         ]]}

   }
  )


;;;;;
;; more complex example

(defn init-evt-loop [params]
  ;; this should prepare all stuff needed for ctx and steps
  (let [evt-loop-chan (st-wf/&chan params (wf/rand-sid "evt-loop"))]
    {
     ;; keep the evt loop chan
     ::evt-loop-chan evt-loop-chan
     }
    )
  )

(defn ctx-evt-fn [params]
  {
   :evt-loop {
              :fn       (fn [in-chan] in-chan)
              :infinite true
              :expands? true
              }

   :collect {
             :fn (fn [v]
                   v
                   )
             :collect? true
             }

   }
  )

(defn steps-evt-fn [params]
  {

   ::evt-loop [:evt-loop (::evt-loop-chan params)]

   ::timestamps [:collect ::evt-loop]

   }
  )


(defn initialize-test-wf! []
  {

   :init-fns  [init-evt-loop
               st-wf/chan-factory-init-fn
               ]

   :ctx-fns   [ctx-evt-fn

               (fn [params]
                 {
                  :print {:fn (fn [v]

                                v)}

                  }
                 )]

   :steps-fns [steps-evt-fn

               (fn [params]
                 {
                  ::initial-print [:print "hello"]
                  })

               ]

   :opt-fns   [;; ls/ls-opts-fn
               st-wf/chan-factory-opts-fn
               ]

   }
  )



;; exposes init param by it's key
(defn &wf-init-param [wf k]
  (get-in wf [:runtime :initial :params k]))


(rum/defcs <custom-wf-ui> < rum/reactive
                            (rum/local true ::inline-results?)
                            (rum/local true ::sort-results?)
  [local wf]

  [:div

   (if (not= :not-started (:status wf))
     [:div


      [:button {:style {:font-size "12pt"}
                :on-click (fn [e]
                            (let [loop-chan (&wf-init-param wf ::evt-loop-chan)]
                                 (async/put! loop-chan
                                             {(wf/rand-sid "click-") [:print (u/now)]})
                                 )
                            )}
       "Click me!"
       ]

      (into [:.dates]
            (map
              (fn [t]
                [:pre (str "click — " t)])
              (reverse (get-in (:result wf) [::timestamps] [])))
            )


      ]

     )



      [:pre "↓ this is the default UI for displaying wf results ↓"]
      (wf-ui/<default-wf-body-ui> wf)

   ;[:hr]
   ;[:pre (d/pretty (into (sorted-map) wf))]
   ]
  )


(defn initialize-test-wf-w-state! [*wf]
  {


   ;; how to provide a custom ui for actions - we need to pass state here
   :ui-fn      (partial wf-ui/<default-wf-ui> <custom-wf-ui>)

   :title      "Workflow with Event Loop and custom UI"

   :explanation [:div.explanation
                 [:p "Woof workflows can simulate event loop, so we can use wf with the UI"]
                 [:p "By clicking the button - we can add new steps to event loop"]
                 ]

   :wf-actions {
                ; :not-started []
                :running [

                          ["ui event" (fn []
                                          (let [loop-chan (st-wf/&wf-init-param *wf ::evt-loop-chan)]
                                               (async/put! loop-chan
                                                           {(wf/rand-sid "ui-") [:print (u/now)]})
                                               )
                                          )]

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




