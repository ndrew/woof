(ns woof.playground.old.context
  (:require
    [rum.core :as rum]

    [woof.data :as d]
    [woof.playground.v1.ui :as ui]
    [woof.playground.old.ui :as old-ui]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]
    [woof.utils-macros :refer [put!?]]))




(rum/defcs <step-handler-ui> < rum/reactive {:key-fn (fn [k _ _ _] k)}
                               (rum/local ""   ::args)
                               (rum/local nil  ::result)

  [local selected-hid *hid handler-map]
  [:div.action-info
   (ui/menubar (str
                 (d/pretty selected-hid)
                 (d/pretty (dissoc handler-map :fn)))

               [["⇽" (fn [] (reset! *hid nil))]])

   [:div

    [:div "args: "
     (old-ui/data-editor (fn[new-v]
                       ;; todo: handle channels
                       ;; todo: handle exceptions
                       (reset! (::args local) new-v)
                       (reset! (::result local)
                               ((:fn handler-map) new-v))
                       )
                     @(::args local))
     ]
    [:div "result: " [:span.edn.result (d/pretty @(::result local))]]
    ]
   ]
  )


(rum/defcs <context> < rum/reactive
                       (rum/local nil  ::hid)

  [local *CONTEXT-MAP]

  [:div.context-ui
   (if-let [selected-hid @(::hid local)]
     (<step-handler-ui> selected-hid (::hid local) (get @*CONTEXT-MAP selected-hid))
     ;; else
     [:div.actions
      (ui/menubar "step handlers"
                  (map
                    (fn [[k v]] [(d/pretty k)
                                 (fn []
                                   (reset! (::hid local) k)
                                   ) ]) @*CONTEXT-MAP))
      ]
     )


   ]
  )

