(ns ^:figwheel-hooks woof.v2.playground
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]

    [woof.base :as base]
    [woof.data :as d]
    [woof.playground.common :as cmn]
    [woof.utils :as utils]

    [woof.playground.v1.ui :as ui]
    [woof.playground.v1.utils :as v1u :refer [dstr kstr vstr]]

    [woof.playground.state :as state]

    [woof.utils :as u]

    ;; v2 deps
    [woof.v2.wf.stateful :as st-wf]
    [woof.v2.wf.local-storage :as ls]


    [woof.v2.wf.preview :as preview]
    [woof.v2.wf.post :as post]


    [woof.wf :as wf])

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



;; this will be a project tree
;; each node is a separate workflow

;; updatable storage
(defonce *TREE (atom {
                      ;; internal
                      ::current []

                      ;; workflows
                      :preview  (preview/preview-wf-state!)
                      :post     (post/post-wf-state!)
                      }))


(def init! (partial cmn/default-init! *TREE))
(def reload! (partial cmn/default-reload! *TREE))



;;;;;

;; wf helper functions



(defn- node-keyword?[k]
  (and
    (keyword? k)
    (not (qualified-keyword? k))))


(rum/defc <project-ui> < rum/reactive [*TREE]

   (let [current-selector (get-in @*TREE [::current])
         root? (= [] current-selector)]

     (if root?
       ;; top menu
       (let [select-node! (fn [k]
                            (let [nu-tree (swap! *TREE assoc-in [::current] (conj current-selector k))
                                  nu-selector (::current nu-tree)]
                              ;; init the wf automatically on navigation
                              (st-wf/wf-init! (rum/cursor-in *TREE nu-selector))))]
         [:div.woof
         (ui/menubar ""
                     (map (fn [k] [(name k) (partial select-node! k)])
                          (filter node-keyword? (keys @*TREE))))
          ])
       ;; ui
       (let [*wf (rum/cursor-in *TREE current-selector)]
         [:div.woof
          (ui/menubar (pr-str current-selector)
                      [["←" (fn []
                              (let [foo 123]
                                ;; if wf is running then prompt about ending
                                ;; (when (js/confirm "delete wf?") ...)
                                (swap! *TREE assoc-in [::current] [])
                                )
                              )]
                       ;; in case yo need to init-wf
                       ; ["init wf!" init-wf!]
                       ;; or other global action
                       ;["global action" (fn [] (prn "boo!"))]
                       ])
          [:hr]
          (let [wf @*wf]
            ((:ui-fn wf) *wf)
            )
          ]
         )
       )
     )
  )

(def <app> #(<project-ui> *TREE))