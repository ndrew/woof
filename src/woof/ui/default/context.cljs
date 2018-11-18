(ns woof.ui.default.context
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]

    [woof.data :as d]
    [woof.graph :as g]
    [woof.wf :as wf]
    [woof.ws :as ws]

    [woof.ui :as ui]
    [woof.utils :as u]

    )


  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]
    [woof.utils-macros :refer [put!?]]))




(rum/defcs <step-handler-ui> < rum/reactive {:key-fn (fn [k _ _ _] k)}
  (rum/local ""   ::args)
  (rum/local nil  ::result)

  [local selected-hid *hid
   handler-map]

    [:div.action-info
     (d/pretty selected-hid)


     (ui/menubar (str
                   (d/pretty selected-hid)
                   (if (map? handler-map)
                     (d/pretty (dissoc handler-map :fn))
                     "")
                   )

                 [["⇽" (fn [] (reset! *hid nil))]])

     [:div

      [:div "args: "
       (ui/data-editor (fn[new-v]
                         ;; todo: handle channels
                         ;; todo: handle exceptions
                         (reset! (::args local) new-v)

                         (reset! (::result local)

                                 (if (map? handler-map)
                                   ((:fn handler-map) new-v)
                                   (handler-map new-v))
                           )
                         )
                       @(::args local))
       ]
      [:div "result: " [:span.edn.result (d/pretty @(::result local))]]
      ]
     ]
  )


(rum/defcs <context> < rum/reactive
  (rum/local nil  ::hid)

  [local header *CONTEXT-MAP]

  [:div.context-ui
   (if-let [selected-hid @(::hid local)]
     (<step-handler-ui> selected-hid (::hid local) (get @*CONTEXT-MAP selected-hid))
     ;; else
     [:div.actions
       (ui/menubar header
                  (map
                    (fn [[k v]] [
                                  (d/pretty k)

                                 (fn []
                                   (reset! (::hid local) k)
                                   ) ]) @*CONTEXT-MAP))
      ]
     )


   ]
  )


