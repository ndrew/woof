(ns ^:figwheel-hooks woof.playground.prototype14
  (:require
    [rum.core :as rum]

    ;; client core
    [woof.base :as base]
    [woof.data :as d]
    [woof.wf :as wf]
    [woof.ui :as ui]
    [woof.u :as u]
    [woof.utils :as utils]

    [woof.wfc :as wfc
     :refer [WoofWorkflow
             get-params
             get-context-map
             get-steps]
     ]


    ;; core async
    [cljs.core.async :as async]

    [woof.playground.v1.playground :as pg]
    [woof.playground.v1.utils :refer [dstr kstr vstr] :as v1_utils]
    [woof.playground.v1.ui :as wfui]

    [woof.playground.common :as cmn]

    [clojure.data :as cd]

    [woof.state.core :as state]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; avoid having wf instance in the init-params


(defn ui-model []
  {

   ::dynamic-state {}

   ;; wf map
   :wf {}

   :chans {}    ;; storage for channels

   ; ui state
   :ui {}

   }
  )


;; re-use state map from v1
(defonce *UI-STATE  (atom (ui-model)))

;; sync + diff model?

;; crud, w/o order - a map {} or w order - a vector []
;;   order can be added to a map (if it's map of maps, or there is a sort fn)

;; crud delta - can be infinitely updated map, that will always be merged
;;          or a list of operations like [create, update, delete]

;; data + metadata (for example, ui selection)


;; --- exports

(declare <ui>)

(def init! (partial cmn/default-init! *UI-STATE))
(def reload! (partial cmn/default-reload! *UI-STATE))


;; atom watchers
;; adds sub atom into atom, and start watching for atom updates

(defn close-watchers! [*watchers-map]
  (doseq [[watcher-id *DATA] @*watchers-map]
    (remove-watch *DATA watcher-id))
  (reset! *watchers-map {}))


(defn add-watcher! [*watchers-map watcher-id ch *a]
  (async/put! ch @*a)

  (swap! *watchers-map assoc watcher-id *a)
  (add-watch *a
             watcher-id
             (fn [key atom old-state new-state]
               ;; todo: check for nil; should we close watcher on nil?
               (if-not (nil? new-state)
                 (async/put! ch new-state))))
  ch
  )


;; generic wf parts

(defn wf-store-init-params
  "store the final initial params in state"
  [*wf params]
  ; is this an overkill
  (swap! *wf assoc :woof.playground.v1.playground/init-params-map params)

  {}
  )


;;
;; crud wf


(defn crud-deps-init
  "inject dependencies for crud wf. Should be called prior to other init fns"
  [*STATE params]
  (let [sf (state-factory     (rum/cursor-in *STATE [::dynamic-state]))
        cf (base/chan-factory (rum/cursor-in *STATE [:chans]))
        ]
    {
     ;; provide a channel factory, so wf should not care about closing channels
     ::cf cf

     ;; provide a state factory, so wf could define it's own atoms with watchers
     ::sf sf

     ;; provide an atom for storing watchers
     ::watchers-map   (sub-state sf ::watchers-map {})
     }
    )
  )



;; use case: CRUD
;; find a way how to implement CRUD via a workflow?

;; approach a: data map + delta map

;; start

;;
(defn crud-init
  "init data atoms for crud the workflow"
  [params]
  (let [sf (::sf params)]

    {
     ;; crud data
     ::crud           (sub-state sf ::crud
                                 {:initial-data {:v "data before sync"}})

     ;;
     ::crud-delta-map (sub-state sf ::crud-delta-map {})

     }
    )
  )



(defn crud-steps [params]
  (let [crud* (::crud params)
        crud-delta-map* (::crud-delta-map params)]
    {

     ;; quick way of returning atom data
     ::crud  [:watcher crud*]
     ::delta  [:watcher crud-delta-map*]

     ;; merge
     ::CRUD-data [:delta-merge [::crud ::delta]]

     ;; todo: add details step - for

     })
  )




;; ui loop

;; event loop simulation
;   init:  ::loop (base/make-chan (::cf params) ::loop)
;   ctx:   :event-loop <ch>
;   steps: ::loop [:event-loop (::loop params)]

(defn crud-ui-loop-ctx [params]
  {
   :event-loop {
                :fn       (fn [in-chan]
                            ;; (u/wiretap-chan in-chan (partial println "UI LOOP:"))
                            in-chan)

                :infinite true
                :expands? true
                }})


(defn crud-ui-loop-init [params]  { ::loop  (base/make-chan (::cf params) ::loop) })
(defn crud-ui-loop-steps [params] { ::loop  [:event-loop (::loop params)] })



(defn crud-ui-init [params]
  {

   ::selected-meta  (sub-state (::sf params) ::lb-meta { :selected-idx 0 })
   }
  )

(defn crud-ui-ctx [*ui params]
  {
   :ui-v {
          :fn (fn [v]
                (swap! *ui :v v)
                v)
          }
   }
  )



(defn crud-ui-steps [params]
  (let [meta* (::selected-meta params)]
    {

     ;; another way of returning data collection is via expand function

     ;; convert crud data into UI stuff
     ;; todo: sorting fn
     ;; todo: listbox ui
     ;; todo: listbox metadata - preserving selection


     ::listbox-data [:sort! ::CRUD-data]
     ::listbox-meta [:watcher meta*]

     ::listbox-model [:listbox [::listbox-data ::listbox-meta]]

     ::ui  [:ui-v ::listbox-model]
     }
    )
  )


(defn crud-ctx [params]
  (let [cf (::cf params)
        *watchers-map (::watchers-map params)]
    {
     :hello   {
               :fn (fn [a]
                     (prn a)
                     a)
               }

     :delta-merge {
                   :fn (fn [vs]
                         (let [data (first vs)
                               r (rest vs)]
                           (apply merge data r)
                           )
                         )
                   :collect? true

                   }

     :watcher {
               :fn (fn [*a]
                     (let [ch (base/make-chan cf (wf/rand-sid "watcher-chan"))
                           watcher-id (wf/rand-sid "wid-")
                           ]
                       (add-watcher! *watchers-map
                                     watcher-id
                                     ch
                                     *a)))

               :infinite true
               }

     ;; how to upd state, should there be a step handler for that?
     ;; by passing state + nu value
     ;; by generating new step
     ;; by id + updater step?

     :upd! {
            :fn (fn [wtf]
                  ;;
                  )
            }



     :sort! {
             :fn (fn [m]

                   #_(for [[_ vs] (group-by :v (vals m))]
                       (apply compare :v vs))

                   ;; todo: how to sort the map
                   (map last (sort-by #(get % :v) m))

                   ;(apply last (sort-by #(get % :v) m))
                   )

             }

     ;;

     :listbox {
               :fn (fn [[data mdata]]
                     (let [idx (:selected-idx mdata)]
                       (map-indexed (fn [i a]
                                      (if (= i idx)
                                        (assoc a :selected? true)
                                        a)
                                      )
                                    data)
                       )
                     )
               :collect? true
               }


     }
    )
  )

(defn crud-init-fns [*STATE]
  (let [*wf (rum/cursor-in *STATE [:wf])]
    [
     ;; todo: is the reversed order ok? - no
     ;; d)
     (partial wf-store-init-params *wf)
     ;; c) ui stuff
     crud-ui-init
     crud-ui-loop-init
     ;; b) init crud wf
     crud-init
     ;; a) inject state stuff into wf scope
     (partial crud-deps-init *STATE)
     ]
    )
  )


;;
;; wf opts

(defn wf-store-xtor
  "store the wf xtor, so we can later stop it"
  [*wf wf-chan xtor]
  (swap! *wf assoc ::xtor xtor)
  ; return some value, not a channel!
  :ok)





(defn test-wf-opts-fn
  "opt fn for that cleans state after workflow is completed"

  [params]
  (let [*result (atom {})
        close-stuff! (fn []
                       (partial close-watchers! (::watchers-map params))
                       (partial base/close-chans! (::cf params))

                       (reset! *result nil))

        ]

    {
     :before-process  (partial wf-store-xtor (:wf params))

     :op-handlers-map {
                       :process (fn [interm-result]
                                  ;; todo: log diffs
                                  (let [prev-result @*result]
                                    (reset! *result interm-result)

                                    (binding [v1_utils/*curr-ns* (str (namespace ::this))]
                                      (let [[things-only-in-a things-only-in-b things-in-both] (cd/diff interm-result prev-result)
                                            ]
                                        (.groupCollapsed js/console ":process")


                                        #_(prn (merge-with (fn [v1 v2]
                                                           (vswap! *added assoc )

                                                      (.log js/console v1 " vs " v2)
                                                      v2
                                                      )
                                                    things-only-in-b
                                                    things-only-in-a
                                                    ))
                                        #_(into (sorted-map)
                                              (dstr ))
                                        (.groupEnd js/console)
                                        )
                                      )
                                    )
                                  )

                       :done    (fn [result]
                                  (prn "result" result)
                                  (close-stuff!))

                       :error   (fn [err]
                                  (.error js/console err)
                                  (close-stuff!))
                       }}))



;; helper UIs

(rum/defc <channel-factory-ui> < rum/static [chans]
  [:div

   [:h5 "Channels:"]
   (into
     [:ul]
     (map (fn [[k v]]
                    [:li (pr-str k)]
                    )
                  chans)

     )
   ]
  )

(rum/defc <state-factory-ui> < rum/reactive [state]
  [:div
   [:h5 "State:"]
   (into
     [:ul]
     (map (fn [[k v]]
            [:li
             (ui/menu-item "ℹ️" (fn [] (.log js/console v)))
             [:span {:style {:margin-left "1rem"}} (pr-str k)]
             ]
            )
          state)

     )
   ]
  )



(rum/defcs <debug-ui>  < rum/reactive
                         (rum/local false :show-cf?)
                         (rum/local false :show-sf?)
  [local *STATE]
  [:div
   (ui/menubar "DBG: " [
                   ["channels" (fn [] (swap! (:show-cf? local) not))]
                   ["state"    (fn [] (swap! (:show-sf? local) not))]
                   ])
   (if @(:show-cf? local)
     (<channel-factory-ui> (get-in @*STATE [:chans])))

   (if @(:show-sf? local)
     (<state-factory-ui> (get-in @*STATE [::dynamic-state]))
     )

   ]

  )

;; reactive ui

(rum/defc <ui> < rum/reactive [*STATE]
  (let [; st @*STATE

        ;; state for the wf

        ;; do we need to get this every time?
        *wf (rum/cursor-in *STATE [:wf])
        *ui (rum/cursor-in *STATE [:ui])

        ;; todo:
        IN-init-params (get @*wf :woof.playground.v1.playground/init-params-map {})
        IN-evt-loop (::loop IN-init-params)
        ]
    [:div

     (ui/menubar "prototype 14" [
                                 ["reset wf!" (fn [] (reset! *STATE (ui-model)))]
                                 ])

     (<debug-ui> *STATE)

     [:table.wf-description
      [:tr
       [:td {:col-span 6}
        "CRUD wf example:\n      \n      load some initial data (read), start UI, add more data, update existing data (incl. delete) via deltas"
        ]
       ]
      [:tr
       [:td
        [:header "generic WF"]
        [:pre
"[INIT]
* chan-factory
* state-factory
[CTX]
* atom watcher
"

         ]
        ]
       #_[:td
        [:header "stateful WF"]
       ;; maybe
        ]

       [:td
        [:header "CRUD WF"]
        [:strong "!!!actual test!!!"]
        ]

       [:td {:style {:background-color "rgba(0,0,0,.1333)"}}
        [:header "evt loop"]
        [:p "event loop, for UI" ] ; may be the part of UI wf
        ]

       [:td {:style {:background-color "rgba(0,0,0,.1333)"}}
        [:header "listbox WF"]
        [:p "a ui "]
        ]
       [:td {:style {:background-color "rgba(0,0,0,.1333)"}}
        [:header "capture WF"]
        [:p "[before WF] capture xtor"]
        [:p "[INIT] capture init params: expose the evt-loop"]
        [:p "[OPT] print result/error"]
        ]
       ]
      ]

     (let [run (fn []
                 ;; todo: save this to state
                 (let [wf-cfg {
                               :init-fns  (crud-init-fns *STATE)
                               :ctx-fns   [
                                           crud-ui-loop-ctx
                                           (partial crud-ui-ctx (rum/cursor-in *STATE [:ui]))
                                           crud-ctx
                                           ]

                               :steps-fns [
                                           crud-ui-loop-steps
                                           crud-steps
                                           crud-ui-steps

                                           (fn [params]
                                             {::hello! [:hello "test!"]}
                                             )
                                           ]

                               :opt-fns   [
                                           test-wf-opts-fn
                                           ]
                               }]
                   (.log js/console wf-cfg)
                   (pg/ui-wf *wf wf-cfg)
                   )
                 )]
       [:div.wf
        (ui/menubar "WF" [
                          ["run" run]
                          []

                          ["add new delta item"
                           (fn []
                             ;; ugly
                             (swap! (rum/cursor-in *STATE [::dynamic-state ::crud-delta-map])
                                    assoc (wf/rand-sid "𝚫")
                                    {:v (str "added at " (utils/now))}
                                    )
                             )
                           ]

                          ["delete some delta item"
                           (fn []
                             (let [*crud-delta-map (rum/cursor-in *STATE [::dynamic-state ::crud-delta-map])
                                   kz (keys @*crud-delta-map)
                                   k2delete (rand-nth kz)
                                   ]
                               (swap! *crud-delta-map dissoc k2delete)

                               ; todo: notify wf via sending new command
                               )
                             )
                           ]

                          []

                          ["stop!"
                           (fn []
                             (if-let [xtor (::xtor @*wf)]
                               (base/end! xtor)
                               (println "no :xtor found")))
                           ]

                          ])
        ;(<IN-row> *wf)
        ;(<debug> *wf)
        ;(<OUT-row> *wf)
        ]
       )



     [:div
      [:header "UI"]
      ;; test ui

      (let [lb @*ui]
        (into
          [:ul ]
          (map-indexed (fn [i li]
                 (let [selected? (:selected? li)]
                   [:li
                    {:style (if selected?
                              {:border "1px solid red"
                               :background-color "rgba(250,0,0,.1333)"
                               }
                              {})
                     :on-click (fn [e]

                                 ; this will force the list box to re-render
                                 (swap! (::selected-meta IN-init-params)
                                        assoc :selected-idx i)

                                 ; trigger the params

                                 #_(async/put! IN-evt-loop
                                             {
                                              (wf/rand-sid) [:hello (str "time: " (utils/now))]
                                              }
                                   )
                                 )
                     }
                    (pr-str (:v li))]
                   )

                 )
               lb)

          )
        )


      [:pre
       "initial params:\n"
       (d/pretty
         (dissoc IN-init-params :wf))
       ]

      [:pre "ui data"  (d/pretty @*ui)]

      ]
     ]
    )
  )




;; root ui component for prototype
(def <app> #(<ui> *UI-STATE))
