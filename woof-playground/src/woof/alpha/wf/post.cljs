(ns woof.alpha.wf.post
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]

    [woof.data :as d]

    [woof.playground.v1.ui :as ui]
    [woof.client.stateful :as st-wf]
    [woof.client.browser.local-storage :as ls]

    [woof.utils :as u]
    [woof.wf :as wf]
    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


(defonce *STATE (atom {
                       ::post "An example post"
                       }))

;; ui

(rum/defc <post-ui> < rum/reactive [*wf]
  (let [wf @*wf
        status (:status wf)
        title (:title wf) ;; (pr-str (:id wf))
        ]

    [:div
     (ui/<wf-menu-ui> title status (:actions wf))

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


;; function that initializes the specific workflow for the node
(defn init-post-wf! [*NODE]
  {

   :title     "Write to Local Storage Workflow"

   :ui-fn     <post-ui>

   :init-fns  [(fn [params]
                 ;; this should prepare all stuff needed for ctx and steps
                 (let [evt-loop-chan (st-wf/&chan params (wf/rand-sid "evt-loop"))]
                      {
                       ;; keep the evt loop chan
                       ::evt-loop-chan evt-loop-chan
                       }
                      )
                 )

               ls/ls-init-fn

               st-wf/chan-factory-init-fn
               ]

   :ctx-fns   [ls/ls-ctx-fn
               (fn [params]
                 {
                  :test     {:fn (fn [v] v)}

                  :evt-loop {
                             :fn       (fn [in-chan] in-chan)
                             :infinite true
                             :expands? true
                             }
                  }
                 )]

   :steps-fns [(fn [params]
                 {
                  ::preview  [:ls-infinite-read "PREVIEW"]

                  ::evt-loop [:evt-loop (::evt-loop-chan params)]
                  }
                 )]

   :opt-fns   [ls/ls-opts-fn
               st-wf/chan-factory-opts-fn]


   :wf-actions  {
                 :running [

                           ["dummy write " (fn []

                                             (let [loop-chan (st-wf/&wf-init-param *NODE ::evt-loop-chan)]
                                                  (async/put! loop-chan
                                                              {(wf/rand-sid "ui-")
                                                               [:ls-write ["PREVIEW"

                                                                           (str "I am a post\n" (u/now))
                                                                           ]]})
                                                  )
                                             )]
                           ]
                 }


   }
  )


#_(defn post-wf-state! []
  (st-wf/wf "local-storage-post"
            (fn [*NODE]
              (init-post-wf! *NODE)))
  )


