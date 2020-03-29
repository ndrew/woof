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

(rum/defcs <default-wf-body-ui> < rum/reactive
                               (rum/local true ::inline-results?)
                               (rum/local true ::sort-results?)
  [local wf]

  [:div.wf-body

   (if-let [initial-steps (get-in wf [:runtime :initial :steps])]
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

    ;[:hr]

   ;; this should be the easiest way to display wf results

   #_[:div.wf-body-menu
    (ui/menubar "Display results as:" [
                                       ["inline" (fn [] (swap! (::inline-results? local) not))]
                                       ["sort" (fn [] (swap! (::sort-results? local) not))]
                                       ])
    ]

   #_(if @(::inline-results? local)
     [:pre
      "Results (inlined)\n"
      (d/pretty (base/inline-results (:result wf)))
      ]
     )

   #_(if @(::sort-results? local)
     [:pre
      "Results (sorted)\n"
      (d/pretty (into (sorted-map) (:result wf)))
      ]
     )

   #_[:pre
    "Results (vstr)\n"

    ;; fixme: how to substitute fully qualified keywords with shorter ones?
    ;; ugly way to shorten the fully qualified keywords
    (binding [v1u/*curr-ns* (.substr (.replace (str :woof.client.playground.wf.simple/test) "/test" "") 1)]
      (v1u/vstr (:result wf)))]


   ]

  )

(rum/defc <default-body> < rum/static
  [wf]
  (condp = (:status wf)
    :not-started [:div
                  "Hit run! to start a workflow"
                  ]
    :running (<default-wf-body-ui> wf)                      ; [:pre "..."]
    :done (<default-wf-body-ui> wf)                         ; (safe-pretty (:result wf))
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

      (<body-fn> wf)
     ]
    )
  )

