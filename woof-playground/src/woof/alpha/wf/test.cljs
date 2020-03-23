(ns ^:figwheel-hooks woof.alpha.wf.test
  (:require
    [rum.core :as rum]

    ;; v2 deps
    [woof.client.stateful :as st-wf]

    [woof.playground.v1.ui :as ui]
    [woof.data :as d]
    [woof.wf :as wf]
    [cljs.core.async :as async]
    [woof.utils :as u]
    )
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

(defn simplest-wf-initializer [*SWF]
  {

   :init-fns   [(fn [params]
                  {:IN :some-wf-parameters}
                  )
                ]

   :ctx-fns    [(fn [params]
                  {
                   :intro  {:fn (fn [v]
                                  (prn "INTRO:" v)
                                  v)
                           }
                   :wait {
                           :fn       (fn [t]
                                       (let [ch (async/chan)]

                                            (go
                                              (async/<! (u/timeout t))
                                              (async/put! ch "DONE")
                                              )

                                            ch
                                            ))

                           }
                   }
                  )]

   :steps-fns  [(fn [params]
                  {
                   ::intro-1 [:intro "Example of the simplest WF possible"]
                   ::intro-2 [:intro ""]

                   ::wait [:wait 2000]
                   })

                ]
   :opt-fns    []


   ;; ui specific keys, optional - TODO: make these namespaced
   :title      "Finite WF with default UI"
   :explanation [:div

[:p {:style {:color "red"}} "This is a simplest workflow example. Finite!!!"]
[:pre "ctx:\n
:intro - displays a test message
:wait - waits for some time
"]

[:pre "steps:\n
  ::intro-1 [:intro \"Example of the simplest WF possible\"]
  ::intro-2 [:intro \"\"]
  ::wait [:wait 2000]"]
]
   :wf-actions {
                :done [["custom action (on done) " (fn []
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
   :test     {:fn (fn [v] v)}

   :evt-loop {
              :fn       (fn [in-chan] in-chan)
              :infinite true
              :expands? true
              }
   }
  )

(defn steps-evt-fn [params]
  {

   ::evt-loop [:evt-loop (::evt-loop-chan params)]
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
                  :test {:fn (fn [v] v)}

                  }
                 )]

   :steps-fns [steps-evt-fn

               (fn [params]
                 {
                  ::YO [:test "YO"]
                  })

               ]

   :opt-fns   [;; ls/ls-opts-fn
               st-wf/chan-factory-opts-fn
               ]

   }
  )


(rum/defc <example-node-ui> < rum/reactive [*wf]
  (let [wf @*wf
        status (:status wf)
        title (:title wf) ;; (pr-str (:id wf))
        ]

    [:div
     (ui/<wf-menu-ui>
       title status
       (:actions wf))


     (let [r (:result wf)]
       [:pre
        (pr-str r)
        ]
       )

     [:hr]
     [:pre (d/pretty (into (sorted-map) wf))]
     ]
    )
  )




(defn initialize-test-wf-w-state! [*NODE]
  {
   ;; for we provide a ui fn
   :ui-fn   <example-node-ui>


   :title     "More Complex Workflow"

   :wf-actions {
                ; :not-started []
                :running [
                          ["dummy event " (fn []
                                            (prn "dummy event")

                                            #_(let [loop-chan (st-wf/&wf-init-param *NODE ::evt-loop-chan)]
                                                (async/put! loop-chan
                                                            {(wf/rand-sid "ui-")
                                                             [:ls-write ["PREVIEW"

                                                                         (str "I am a post\n" (u/now))
                                                                         ]]})
                                                )
                                            )]

                          #_["ui event" (fn []
                                          (let [loop-chan (&wf-init-param *wf ::evt-loop-chan)]
                                               (async/put! loop-chan
                                                           {(wf/rand-sid "ui-") [:test (u/now)]})
                                               )
                                          )]

                          ]
                ; :done        []
                }


   }
  )



(defn dummy-wf-initializer [*NODE]
  (merge
    (initialize-test-wf!)
    (initialize-test-wf-w-state! *NODE))
  )




