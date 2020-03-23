(ns ^:figwheel-hooks woof.client.playground.ui.wf
  (:require
    [rum.core :as rum]

    ;; common workflow stuff and ui
    [woof.playground.v1.ui :as ui]
    [woof.data :as d])
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

;; default ui for wf runner
(rum/defc <default-wf-ui> < rum/reactive
  [<body-fn> *wf]
  (let [wf @*wf
        status (:status wf)
        title (:title wf)
        explanation (:explanation wf)
        ]

    [:div
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

