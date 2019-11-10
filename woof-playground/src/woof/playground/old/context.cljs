(ns woof.playground.old.context
  (:require
    [cljs.core.async :as async]
    [cljs.test :as test :refer-macros [deftest is testing run-tests async]]

    [goog.string :as gstring]
    [goog.string.format]

    [rum.core :as rum]

    [woof.playground.old.app-data :as app-model]

    [woof.data :as d]
    [woof.graph :as g]
    [woof.wf :as wf]

    [woof.playground.old.ui :as ui]
    [woof.playground.old.wf-ui :as wf-ui]
    [woof.utils :as u]

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
     (ui/data-editor (fn[new-v]
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

