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
    [woof.wf :as wf])

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



(declare init-ls-wf!)

;; this will be a project tree
;; each node is a separate workflow

;; for now, it's a local storage example
(defonce *TREE (atom
    {
     ;; internal
     ::current []
     ::chans {}

     ;;
     :local-storage (assoc
                      (state/empty-swf "local-storage")
                      ::init-wf (fn [*NODE] (init-ls-wf! *NODE)))
     }
    ))


(def init! (partial cmn/default-init! *TREE))
(def reload! (partial cmn/default-reload! *TREE))


;; specific workflow definitions

;; function that initializes the specific workflow for the node
(defn init-ls-wf! [*NODE]
  (let [node @*NODE
        initial-wf (state/state-wf (get node :id))
        updated-wf {

                    :ctx-fns   [(fn [params]
                                  (let [cf (st-wf/&chan-factory params)]

                                    {
                                     :test {:fn (fn [v] v)}

                                     :evt-loop {
                                                  :fn       (fn [in-chan] in-chan)
                                                  :infinite true
                                                  :expands? true
                                                  }
                                     }
                                    )

                                  )]

                    :init-fns  [(fn [params]
                                  ;; this should prepare all stuff needed for ctx and steps

                                  (let [evt-loop-chan (st-wf/&chan params (wf/rand-sid "evt-loop"))]
                                    {
                                     ;; keep the evt loop chan
                                     ::evt-loop-chan evt-loop-chan
                                     }
                                    )
                                  )

                                (partial st-wf/chan-factory-init-fn_
                                         (rum/cursor-in *TREE [::chans]))

                                ]

                    :steps-fns [(fn [params]
                                  (let [ch (st-wf/&chan params (wf/rand-sid "evt-loop"))]
                                    {
                                     ::hello [:test "woof!"]
                                     ::evt-loop [:evt-loop (::evt-loop-chan params)]
                                     }
                                    )
                                  )]

                    :opt-fns   [st-wf/chan-factory-opts]

                    ;; todo: is this a correct way of setting function that will run
                    ::init-wf  (::init-wf node)

                    :title "Local Storage Workflow"
                    }
        wf (merge initial-wf updated-wf)
        ]
    (reset! *NODE wf)
    )
  )


;; wf helper functions

(defn &wf-init-param [*wf k]
  (get-in @*wf [:runtime :initial :params k]))


(defn wf-init! [*NODE]
  (let [f (get @*NODE ::init-wf)]
    (f *NODE)))



(defn ls-actions-map [*wf ]
  {
   ; :not-started []
   :running     [
                 ["ui event" (fn []
                               (let [loop-chan (&wf-init-param *wf ::evt-loop-chan)]
                                 (async/put! loop-chan
                                             {(wf/rand-sid "ui-") [:test (u/now)]})
                                 )
                               )]
                 ]
   ; :done        []
   })


(defn default-actions-map [init-wf! run-wf! stop-wf! wf-actions-map]
  {
   :not-started (conj (get wf-actions-map :not-started [])
                      [] ["run!" run-wf!])
   :running     (conj (get wf-actions-map :running [])
                      [] ["stop!" stop-wf!])
   :done        (conj (get wf-actions-map :done [])
                      [] ["reset!" init-wf!])
   })


(rum/defc <node-ui> < rum/reactive [*wf actions-map]
  (let [wf @*wf
        status (:status wf)
        title (:title wf) ;; (pr-str (:id wf))
        ]

    [:div
     (ui/<wf-menu-ui>
       title status
       actions-map)

     [:pre (d/pretty (into (sorted-map) wf))]
     ]
    )
  )


(defn- node-keyword?[k]
  (and
    (keyword? k)
    (not (qualified-keyword? k))))


(rum/defc <project-ui> < rum/reactive [*TREE]

   (let [current-selector (get-in @*TREE [::current])
         root? (= [] current-selector)


         ]

     (if root?
       (let [select-node! (fn [k]
                            (let [nu-tree (swap! *TREE assoc-in [::current] (conj current-selector k))
                                  nu-selector (::current nu-tree)]
                              ;; init the wf automatically on navigation
                              (wf-init! (rum/cursor-in *TREE nu-selector))))]
         [:div.woof
         (ui/menubar ""
                     (map (fn [k] [(name k) (partial select-node! k)])
                          (filter node-keyword? (keys @*TREE)))
                     )
          ]
         )
       (let [*wf (rum/cursor-in *TREE current-selector)

             init-wf! (partial wf-init! *wf)
             run-wf! (partial state/swf-run! *wf)
             stop-wf! (partial state/swf-stop! *wf)

             wf-actions (ls-actions-map *wf)
             all-actions (default-actions-map init-wf! run-wf! stop-wf! wf-actions)
             ]
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
          (<node-ui> *wf all-actions)
          ]
         )
       )
     )
  )

(def <app> #(<project-ui> *TREE))