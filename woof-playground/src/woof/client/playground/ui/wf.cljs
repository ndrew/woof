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




(rum/defc <default-body> < rum/static
  [wf]

  [:div {:style {:padding ".5rem" :border "1px solid black"}}
   (condp = (:status wf)
     :not-started   [:pre "Ready to start!"]
     :running [:pre "..."]
     :done (safe-pretty (:result wf))
     :error [:pre "Error: " (safe-pretty (:result wf))]
     )
   ]
  )


(rum/defcs <default-wf-body-ui> < rum/reactive
                               (rum/local true ::inline-results?)
                               (rum/local true ::sort-results?)
  [local wf]

  [:div.wf-body

   ;; this should be the easiest way to display wf results

   #_[:pre
    (str
      "Inline: " @(::inline-results? local) "\n"
      "Sort: " @(::sort-results? local) "\n"
      )
    ]

   [:div.wf-body-menu
    (ui/menubar "Display results as:" [
                                       ["inline" (fn [] (swap! (::inline-results? local) not))]
                                       ["sort" (fn [] (swap! (::sort-results? local) not))]
                                       ])
    ]

   (if @(::inline-results? local)
     [:pre
      "Results (inlined)\n"
      (d/pretty (base/inline-results (:result wf)))
      ]
     )

   (if @(::sort-results? local)
     [:pre
      "Results (sorted)\n"
      (d/pretty (into (sorted-map) (:result wf)))
      ]
     )

   [:pre
    "Results (vstr)\n"

    ;; fixme: how to substitute fully qualified keywords with shorter ones?
    ;; ugly way to shorten the fully qualified keywords
    (binding [v1u/*curr-ns* (.substr (.replace (str :woof.client.playground.wf.simple/test) "/test" "") 1)]
      (v1u/vstr (:result wf)))]


   ]

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

