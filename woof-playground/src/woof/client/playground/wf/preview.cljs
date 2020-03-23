(ns woof.client.playground.wf.preview
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]

    [woof.data :as d]

    [woof.playground.v1.ui :as ui]
    [woof.client.stateful :as st-wf]
    [woof.client.local-storage :as ls]

    [woof.utils :as u]
    [woof.wf :as wf]
    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))

;; ui

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
        (pr-str (::preview r))
        ]
       )

     [:hr]
     [:pre (d/pretty (into (sorted-map) wf))]
     ]
    )
  )

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
   ::preview  [:ls-infinite-read "PREVIEW"]

   ::evt-loop [:evt-loop (::evt-loop-chan params)]
   }
  )


(defn dummy-write [*NODE]
  (let [loop-chan (st-wf/&wf-init-param *NODE ::evt-loop-chan)]
       (async/put! loop-chan
                   {(wf/rand-sid "ui-") [:ls-write ["PREVIEW" (u/now)]]})
       )
  )

;; function that initializes the specific workflow for the node
(defn init-preview-wf! [*NODE]
  {

   :ui-fn <example-node-ui>

   :init-fns  [init-evt-loop
               ls/ls-init-fn

               st-wf/chan-factory-init-fn

               ]

   :ctx-fns   [ls/ls-ctx-fn
               ctx-evt-fn]

   :steps-fns [steps-evt-fn]

   :opt-fns   [ls/ls-opts-fn
               st-wf/chan-factory-opts-fn]


   :title     "Local Storage Workflow"

   :wf-actions  {
                 ; :not-started []
                 :running [
                           ["dummy write " (partial dummy-write *NODE)]

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

