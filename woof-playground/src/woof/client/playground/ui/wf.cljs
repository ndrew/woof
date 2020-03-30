(ns ^:figwheel-hooks woof.client.playground.ui.wf
  (:require
    [rum.core :as rum]

    ;; common workflow stuff and ui
    [woof.client.playground.ui :as ui]
    [woof.data :as d]
    [woof.base :as base]
    [woof.playground.v1.utils :as v1u])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



(defn safe-pretty [v]
  (try
    [:pre (d/pretty v)]
    (catch js/Error e
      (do
        (.error js/console e)
        [:pre (pr-str (js->clj e))]))
    )
  )




;;
(rum/defcs <default-wf-details-ui> < rum/reactive
                                     (rum/local true ::inline-results?)
                                     (rum/local true ::sort-results?)
  [local wf]

  [:div.wf-details

   (if-let [initial-steps (get-in wf [:runtime :initial :steps])]
     ;; TODO: better UI for steps, for now use same ui as for results
     (ui/<results-ui> "INITIAL STEPS"
                      (get-in wf [:runtime :initial])
                      initial-steps)
     )

   (if-let [results (:result wf)]
     (if (not= :not-started (:status wf))
       (ui/<results-ui> "RESULTS"
                        (get-in wf [:runtime :initial])
                        results)
       )
     )
   ]

  )

(rum/defc <default-body> < rum/static
  [wf]
  (condp = (:status wf)
    :not-started [:div
                  "Hit run! to start a workflow"
                  ]
    :running (<default-wf-details-ui> wf)                      ; [:pre "..."]
    :done (<default-wf-details-ui> wf)                         ; (safe-pretty (:result wf))
    :error [:pre.wf-error "Error:\n" (safe-pretty (:result wf))]
    )
  )


;; default ui for wf runner
(rum/defc <default-wf-ui> < rum/reactive
  [<body-fn> *wf]
  (let [wf @*wf
        status (:status wf)
        title (:title wf)
        explanation (:explanation wf)
        ]

    [:div.default-wf-ui
     (ui/<wf-menu-ui> title status (:actions wf))

     (cond
         (string? explanation) [:pre explanation]
         (vector? explanation) explanation
         (fn? explanation) (explanation)
         )

     (try
       (<body-fn> wf)
       (catch js/Error e
         [:pre (pr-str e)]
         )
       )
     ]
    )
  )

