(ns ^:figwheel-hooks woof.alpha.ui.wf

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

;; default ui for wf runner
(rum/defc <default-wf-ui> < rum/reactive [*wf]
  (let [wf @*wf
        status (:status wf)
        title (:title wf)]

    [:div

     (ui/<wf-menu-ui> title status (:actions wf))


     (condp = status
       :not-started   [:pre "ready to start!"]

       :running [:pre "..."]

       :done (safe-pretty (:result wf))

       :error [:pre "Error: "
               (safe-pretty (:result wf))
               ]


       )


     ;[:hr]
     ;(safe-pretty (into (sorted-map) wf))


     ]
    )
  )

