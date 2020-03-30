(ns ^:figwheel-hooks woof.client.playground.wf.sandbox
  (:require
    [cljs.core.async :as async]
    [rum.core :as rum]

    [woof.client.playground.ui.wf :as wf-ui]
    [woof.client.stateful :as st-wf]
    [woof.data :as d]
    [woof.wf :as wf]
    [woof.utils :as utils]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))

;;
;;
;; use this workflow as a sandbox for
;;
;;

;;
;; context
(defn sandbox-ctx-fn [params]
  {
   :print {:fn (fn [v]
                 (.log js/console v)
                 v)}
   }
  )

;;
;; steps
(defn sandbox-steps-fn [params]
  {

   ::initial-print [:id "hello"]

   })

;;
;; UI
;;
(rum/defcs <custom-ui> < rum/reactive
                         (rum/local true ::inline-results?)
                         (rum/local true ::sort-results?)
  [local wf]

  [:div

   #_(if (not= :not-started (:status wf))
     [:div
      "you custom ui here"
      ]
     )

   ;   [:pre "↓ this is the default UI for displaying wf results ↓"]
   (wf-ui/<default-wf-details-ui> wf)
   ]
  )




(declare init-sandbox-wf!)

(defn sandbox-wf-init! [*NODE]
  (merge
    {
     :ui-fn       (partial wf-ui/<default-wf-ui> <custom-ui>)
     :title       "S A N D B O X"

     :explanation [:div.explanation
                   [:p "return context map via " [:code "sandbox-ctx-fn"]]
                   [:p "return steps via " [:code "sandbox-steps-fn"]]
                   [:p "custom UI in " [:code "<custom-ui>"]]

                   ]
     }

    (init-sandbox-wf! *NODE)
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; impl





;; have an event loop, by default

(defn evt-init-fn [params]
  {
   ::evt-loop-chan (st-wf/&chan params (wf/rand-sid "evt-loop"))
   })


(defn evt-ctx-fn [params]
  {
   :evt-loop {
              :fn       (fn [in-chan] in-chan)
              :infinite true
              :expands? true
              }

   :id {:fn identity}

   :collect  {
              :fn       (fn [v]
                          v)
              :collect? true
              }
   })

(defn evt-steps-fn [params]
  {
   ::evt-loop   [:evt-loop (::evt-loop-chan params)]
   }
  )

;;

(defn emit-step_ [*wf]
  (let [step (d/to-primitive (js/prompt "provide step as [:handler-id <params>], e.g. [:print \"some value\"]"))]
       (let [loop-chan (st-wf/&wf-init-param *wf ::evt-loop-chan)]
         (async/put! loop-chan
                     {(wf/rand-sid "emit-") step})
         )
       )
  )


;;
;; export
;;
(defn init-sandbox-wf! [*wf]
  {

   :init-fns   [evt-init-fn
                st-wf/chan-factory-init-fn]

   :ctx-fns    [evt-ctx-fn
                sandbox-ctx-fn]

   :steps-fns  [evt-steps-fn
                sandbox-steps-fn
                ]

   :opt-fns    [st-wf/chan-factory-opts-fn]

   :wf-actions {
                :not-started []
                :running     [
                              ["emit step" (partial emit-step_ *wf)]
                              ]
                :done        []
                }

   }
  )
