(ns ^:figwheel-hooks woof.alpha.wf.test
  (:require
    [rum.core :as rum]

    ;; v2 deps
    [woof.v2.wf.stateful :as st-wf]
    [woof.playground.state :as state]

    [woof.alpha.ui.wf :as wf-ui]

    [woof.playground.v1.ui :as ui]
    [woof.data :as d]
    [woof.wf :as wf])
  (:import [goog.net.XhrIo ResponseType])

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; alpha workflow example

(defn simplest-wf-initializer [*SWF]
  {

   :init-fns  [(fn [params]
                 {:IN :some-wf-parameters}
                 )
               ]

   :ctx-fns   [(fn [params]
                 {
                  :test {:fn (fn [v] v)}
                  }
                 )]

   :steps-fns [(fn [params]
                 {
                  ::step [:test "Hello!"]
                  })

               ]
   :opt-fns   []


   ;; ui specific keys, optional
   :title     "Simplest UI Workflow"
   :wf-actions {
                :done [["your wf is ready!" (fn []
                                              (prn "dummy event"))]]}

   }
  )


;;;;;


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
  ;; should these be passed to here?
  (let [CFG (st-wf/channel-map)]
    {

     :title     "Write to Local Storage Workflow"

     :init-fns  [init-evt-loop

                 (partial st-wf/chan-factory-init-fn_ (st-wf/&channel-map CFG))
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
                 st-wf/chan-factory-opts]

     }
    )
  ;;
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
   ;; for now do not provide a ui fn
   ;; :ui-fn   <example-node-ui>

   :actions (st-wf/default-actions-map
              (partial st-wf/wf-init! *NODE)
              (partial state/swf-run! *NODE)
              (partial state/swf-stop! *NODE)
              {
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
               })
   }
  )



(defn dummy-wf-initializer [*NODE]
  (merge
    (initialize-test-wf!)
    (initialize-test-wf-w-state! *NODE))
  )




