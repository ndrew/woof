(ns woof.ui.wf
  (:require
    [cljs.core.async :as async]
    [clojure.data :as cd]

    [rum.core :as rum]

    [woof.data :as d]

    [woof.wf :as wf]
    [woof.wf-ui :as wf-ui]


    [woof.ui :as ui]
    [woof.ui.context :as ctx-ui]
    [woof.ui.steps :as steps-ui]
    [woof.ui.results :as r]

    [woof.utils :as u]

    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



;;
;; default ui for the workflow


(rum/defc <opts-ui> < rum/reactive [*opts]
    [:div
      ; [:pre (d/pretty *opts)]
     (ctx-ui/<context> "internal wf params" *opts)
     ]
)

;; default wf runner ui
(rum/defcs <wf-ui> < rum/reactive
  [local header *STATE]

  (let [cursor (partial rum/cursor-in *STATE)

        {status :status
         wf :wf
         full-history :history
         } @*STATE]

    [:div.wfui
     [:h5 header]

     ;; pre-wf stuff: steps



     ;; wf actions menu
     (wf-ui/<wf-menu-ui> "wf:" status @(cursor [:wf :status-actions]))



     [:div
      (steps-ui/<steps> (cursor [:wf :steps]) @(cursor [:wf :context-map]))
      ]

     [:div
      (<opts-ui> (cursor [:wf :opts]))
      ]



     ;; results

     (let [history (reverse @(cursor [:history]))]
       [:.log

        ;; results
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

        [:hr]
        ;; todo: migrate old ui
        (r/<wf-results-ui> "result"
                           @(cursor [:result])
                           @(cursor [:steps])
                           (atom {:pre   {} ;; *editors
                                  :post {}}))


        ])
     ]
    )
  )




(defn default-ui-fn [header & r]
  (fn [*STATE]
    (<wf-ui> header *STATE)))

