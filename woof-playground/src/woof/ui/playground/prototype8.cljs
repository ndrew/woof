(ns ^:figwheel-hooks woof.ui.playground.prototype8
  (:require
    [rum.core :as rum]


    ;; client core
    [woof.base :as base]
    [woof.wf :as wf]
    [woof.ui :as ui]
    [woof.u :as u]
    [woof.data :as d]
    [woof.utils :as utils]

    [woof.wfc :as wfc
     :refer [WoofWorkflow
             get-params
             get-context-map
             get-steps]
     ]


    ;; core async
    [cljs.core.async :as async]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))

;; example for workflows

;; data config -> running program


;;

(declare *UI-STATE)                                         ;;
(declare <ui>)                                              ;; (defc <ui> []) - exported ui component
(declare reload!)                                           ;;


(defn init!
  "calls outside mount-fn if the ui state had been changed"
  [mount-fn]

  ;; todo: implement proper subscription
  (add-watch *UI-STATE :woof-main
             (fn [key atom old-state new-state]
               (mount-fn)))

  (when-not (::initialized @*UI-STATE)
    (swap! *UI-STATE merge {::initialized true}))
  )


(defn reload! []
  (swap! *UI-STATE merge {::initialized false}))


;; ----- wf ------------


;; we need to have some kind of registry of available parts

; ready to use
(defn init-fn__peek [params]
  (prn "peek" params))


(defn init-fn__log [params]
  (.log js/console "peeking on init params: " params))


; needs to be specified (via partial)
(defn _opts-fn__state [*wf params]
  {
   :before-process  (fn [wf-chan xtor]
                      (swap! *wf assoc ::xtor xtor)
                      :ok)
   :op-handlers-map {
                     :process (fn [result]
                                (swap! *wf assoc ::result result))
                     :done    (fn [result]
                                (swap! *wf assoc ::result result)
                                (swap! *wf assoc ::status :done)
                                (println "[wf]:done\n" (d/pretty result)))
                     :error   (fn [result]
                                (swap! *wf assoc ::status :error)
                                (println "[wf]:error\n" (d/pretty result)))
                     }
   }
  )

;; exposing data via js api

(defonce REGISTRY (atom {
                         :init-fns {
                                    :peek init-fn__peek
                                    :log init-fn__log
                                    }

                         :ctx-fns {
                                   :test (fn [params]
                                         {
                                          :test {:fn (fn[a]
                                                       (.warn js/console a)
                                                       a)}
                                          })
                                   }
                         :opt-fns {
                                   :default _opts-fn__state
                                   }

                         }))

(defn registry-ds [reg-key]
  (reduce
    (fn [a [k v]]
      (conj a {:text (str k)
               :v (str k)}))
    []
    (get @REGISTRY reg-key))
  )

(defn registry-mapping [reg-key]
  (reduce
    (fn [a [k v]]
      (assoc a (str k) v))
    {}
    (get @REGISTRY reg-key))
  )


;; return registry data

(defn available-init-fns []
  (registry-ds :init-fns))


(defn available-ctx-fns []
  (registry-ds :ctx-fns))


(defn chain-expanded-ctx-cfg [wrapper-step]
  {
   :fn       (fn [sid-list]
               (reduce (fn [a p]
                         (assoc a (wf/rand-sid) [wrapper-step p]))
                       {} sid-list))
   :expands? true
   :collect? false
   }
  )

(defn example-wf [*wf]
  {:init-fns [init-fn__peek]
   :ctx-fns  [(fn [params]
                {
                 :test {:fn (fn[a]
                              (.warn js/console a)
                              a)}
                 :timer {:fn (fn [max]
                               (let [c (async/chan)
                                     ]
                                 (go
                                   (dotimes [n max]
                                     (async/put! c (utils/now))
                                     (async/<! (utils/timeout 1000))
                                     (prn "upd!!!")
                                     )
                                   )
                                 c)
                               )
                         :infinite true
                         }

                 :sample-expand {:fn (fn [n]
                                       (reduce (fn[a i]
                                                 (assoc a (wf/rand-sid (str "expand-" i)) [:test i])
                                                 ) (sorted-map) (range n))

                                       )
                                 :expands? true}

                 :format-time {:fn (fn [t]
                                     (.toString (js/Date. t)))}

                 :collect    {
                              :fn       identity
                              :collect? true
                              }

                 :identity   {:fn identity}

                 :wrap* (chain-expanded-ctx-cfg :format-time)
                 }
                )]

   :steps-fns [(fn [params]
                 {
                  ::test [:test "hello"]
                  ::timer [:timer 50]
                  ::t [:format-time ::timer]

                  ;; expand

                  ::expand-test [:sample-expand 10]

                  ;; todo: add example with expands

                  ;; :enriched-posts* [:wrap* ::posts*]

                  ;; ::ck [:identity [::timer ::t]]
                  ;; ::cv [:collect [::timer ::t]]
                  })]
   :opt-fns   [(partial _opts-fn__state *wf)]
   }
  )


;; map based wf runner
(defn run-wf! [*wf & wfs]
  (let [empty-cfg {
                   :init-fns []
                   :ctx-fns []
                   :steps-fns []
                   :opt-fns []
                   }
        resulting-cfg (reduce (fn [a cfg]
                                (merge-with into a cfg))
                              empty-cfg wfs)
        {init-fns :init-fns
         ctx-fns :ctx-fns
         steps-fns :steps-fns
         opt-fns :opt-fns
         } resulting-cfg
        ]
    (let [wf-params-fn identity  ;; transforms initial map to a wf params
          ; latest fn that returns params
          opt-params-fn (fn [params]
                          ;; maybe store params here
                          ;(.log js/console "params" params)
                          ;(swap! *wf assoc ::params params)
                          params
                          ) ;; transforms wf params to opt params

          wf (base/parametrized-wf!
               (base/combine-init-fns init-fns)
               wf-params-fn
               opt-params-fn

               (base/combine-fns opt-fns :merge-results base/merge-opts-maps)
               (base/combine-fns ctx-fns)
               (base/combine-fns steps-fns)
               (partial base/capturing-WF *wf)
               )]

      ; store wf
      (swap! *wf merge wf)
      (swap! *wf assoc ::status :running)
      (base/run-wf! wf identity)
      )
    ))


;; ------ ui --------------




(defn safe-dec [v]
  (if (> v 0)
    (dec v)
    0))

(defn safe-inc [max v]
  (if (= (inc v) max)
    0
    (inc v)))

(defn remove-indexed [v n]
  (into (subvec v 0 n) (subvec v (inc n))))



; a way of handling ui mode
(defn tabs-data-source [tabs]
  (let [fwd-fn (fn [*a]
                 (let [max (count (:data @*a))]
                   (swap! *a update-in [:ui :selected-idx] (partial safe-inc max))))

        back-fn (fn [*a]
                  (swap! *a update-in [:ui :selected-idx] safe-dec))

        new-tab (fn [*a new-v]
                  (swap! *a update-in [:data] conj new-v)
                  (swap! *a assoc-in [:ui :selected-idx] (dec (count (:data @*a)))))

        rm-tab (fn [*a]
                 (let [i (get-in @*a [:ui :selected-idx])]
                   (swap! *a update-in [:data] remove-indexed i)
                   (back-fn *a)
                   )
                 )

        select (fn [*a i]
                 (swap! *a assoc-in [:ui :selected-idx] i)
                 )
        ]

    {:data tabs
     :ui   {
            :selected-idx 0
            }
     :api  {
            :fwd     fwd-fn
            :back    back-fn
            :new-tab new-tab
            :rm-tab  rm-tab
            :select  select
            }
     }
    )
  )






(defn multi-select-data-source [items available-items]
  (let [v-map (reduce (fn [a [i v]]
                        (assoc a v i))
                      {} (map-indexed (fn [i a]
                                        [i (:v a)]) available-items))

        find-i (fn [a]
                 (get v-map a -1))]

    {
     :raw-data items
     ;; convert values vector into vector of select data sources
     :data     (vec (map
                      (fn [a]
                        (ui/select-data-source available-items
                                            (find-i a)))
                      items))
     :api      {
                :add-new (fn [*ds]
                           (swap! *ds update-in [:data]
                                  conj (ui/select-data-source available-items 0)))
                :remove (fn [*ds i]
                          (swap! *ds update-in [:data] remove-indexed i))

                :get-values (fn [*ds]
                              (let [d (:data @*ds)]
                                (vec (map (fn [a]
                                            ;; todo: use api
                                            (:v (get-in a [:data
                                                           (get-in a [:ui :selected-idx])]))
                                            ) d))
                                )
                              )
                }
     }
    )
  )





(rum/defc <tabs> < rum/reactive [*tab-ds <ui>]
  (let [ds (rum/react *tab-ds)

        {back-fn       :back
         fwd-fn        :fwd
         remove-tab-fn :rm-tab
         new-tab       :new-tab
         select        :select
         } (:api ds)

        tabs (:data ds)
        selected-idx (get-in ds [:ui :selected-idx])

        new-tab-fn (fn []
                     (new-tab  *tab-ds {
                               :header  "new"
                               :content "neeew"
                               })
                     )
        ]

    [:.tab-ui
     [:.tabs
      (let [tab-fn (fn [i tab]
                     (let [selected? (= i selected-idx)     ; (:selected? tab)
                           header (:header tab)]
                       [:li {:class    (if selected? "selected" nil)
                             :on-click (fn [e]
                                         (let [target (.-target e)]
                                           (if-not (= "A" (.-tagName target))
                                             (do
                                               (select *tab-ds i)
                                               )
                                             )))
                             }
                        [:header
                         (if selected? (ui/menu-item "<" (partial back-fn *tab-ds)))
                         (if selected? (ui/menu-item ">" (partial fwd-fn *tab-ds)))
                         (str " " header " ")
                         (if selected? (ui/menu-item "✖" (partial remove-tab-fn *tab-ds)))
                         ]
                        ]
                       )
                     )
            ]

        [:div
         ]
        (into
          [:ul]
          (conj (vec (map-indexed tab-fn tabs))
                [:li (ui/menu-item "+" new-tab-fn)]
                )
          )
        )

      ]
     [:.t-body
      (<ui> *tab-ds)
      ]

     ]
    )
  )



(rum/defc <multi-select> < rum/reactive
  [*multi-ds]
  (let [m-ds (rum/react *multi-ds)
        get-vals (partial (get-in m-ds [:api :get-values]) *multi-ds)
        add-new (partial (get-in m-ds [:api :add-new]) *multi-ds)
        remove (partial (get-in m-ds [:api :remove]) *multi-ds)

        ]
    (into [:div
           ]
          (conj (vec (map-indexed (fn [i a]
                                    [:div.select-row
                                     (ui/<select> (rum/cursor-in *multi-ds [:data i]))
                                     (ui/menu-item "-" (fn [] (remove i)))]
                                    )
                                  (:data m-ds)
                                  ))
                [:div.select-row

                 (ui/menu-item "+" (fn []
                                     (add-new)
                                     ;; add new
                                     ))
                 ;; for now select api is not used,
                 ;; no need to create an intermediary cursor
                 #_(ui/btn "dbg" (fn []
                                 (prn (get-vals))
                                 ))
                 ]
                ; [:pre (d/pretty m-ds)]
                )
          )
    )
  )
;;;


(rum/defc <wf-ui> < rum/reactive
  [*wf-state *tab-ds]

  (let [tab-ds (rum/react *tab-ds)
        selected-idx (get-in tab-ds [:ui :selected-idx])

        *init-fns-ds  (rum/cursor-in *tab-ds [:data selected-idx :ds :init-fns])
        *ctx-fns-ds  (rum/cursor-in *tab-ds [:data selected-idx :ds :ctx-fns])

        get-ids (fn [*ds]
                  (let [f (get-in @*ds [:api :get-values])]
                    (f *ds)))

        get-init-fn (fn[]
                      (let [init-fn-ids (get-ids *init-fns-ds)
                            init-fns-mapping (registry-mapping :init-fns)]
                        (map #(get init-fns-mapping %) init-fn-ids)))

        get-ctx-fn (fn[]
                      (let [ctx-fn-ids (get-ids *ctx-fns-ds)
                            ctx-fns-mapping (registry-mapping :ctx-fns)]
                        (map #(get ctx-fns-mapping %) ctx-fn-ids)))


        test-init! (fn []
                     ;; can ds have a keyword key?

                     (let [
                           init-fns (get-init-fn)
                           ctx-fns (get-ctx-fn)
                           ]

                       (let [wf-cfg {
                                      :init-fns init-fns
                                      :ctx-fns ctx-fns
                                      :steps-fns [(fn [params]
                                                    {
                                                     ::test [:test "hello"]
                                                     })]
                                      :opt-fns   [(partial _opts-fn__state *wf-state)]
                                 }]
                         (run-wf! *wf-state wf-cfg)
                         )

                       #_(let [init-fn (base/combine-init-fns init-fns)]

                         (let [wf-params (init-fn {:azaza :test})]
                           (.log js/console "INIT: " wf-params)

                           (let [ctx-fn (base/combine-fns ctx-fns)
                                 ctx (ctx-fn wf-params)
                                 ]

                             (.log js/console "CTX: " ctx)
                             )

                           ))
                       )
          )
        ]

    [:div
     ;; add state chooser, or hardcode for now

     #_(into [:select {:multiple ""}]
           (map (fn [a]
                  [:option (pr-str a)]
                  )
             init-fns-mapping)
           )

     [:div {:style {:margin-bottom ".5rem"}}
      (ui/menubar "build wf:" [["run init-fn" test-init!]])
      ]


     [:div.flex
      [:label "init-fn"]
      (<multi-select> *init-fns-ds)]

     [:div "___"]
     [:div.flex
      [:label "ctx-fn"]
      (<multi-select> *ctx-fns-ds)]

     [:pre [:strong "state:"] "\n"
      (d/pretty @*wf-state)
      ]


     [:pre [:strong "tab DS:"] "\n"
      (d/pretty @*tab-ds)
      ]

     ]
    )

  )



(rum/defc <ui> < rum/reactive
  [*STATE]
  (let [state-id :tabs
        *tabs-ds (rum/cursor-in *STATE [:tabs])
        *wf (rum/cursor-in *STATE [:wf])
        *multi-ds (rum/cursor-in *STATE [:multi-ddl])
        ]

    [:div.proto8


     [:p
      "prototype for combining WF from parts"]


     #_(<multi-select> *multi-ds)


     ;(ui/<select> (get @*STATE :ddl))
     ;(ui/<select> (get @*STATE :ddl))
     [:hr]

     (ui/menubar "compile workflow UI" [
                                        ["init" (fn[]
                                                  (let [all @*tabs-ds
                                                        wfs (:data all)
                                                        ]
                                                    (prn wfs)
                                                    ; (example-wf *wf)
                                                    )

                                                  )]
                                        ["run" (fn []
                                                 (run-wf! *wf
                                                          (example-wf *wf)
                                                          )
                                                 )]])
     (<tabs> *tabs-ds
             (partial <wf-ui> *wf))

     ; [:hr]
     ; [:pre (d/pretty (rum/react *select-ds))]

     [:hr]
     ;[:pre (d/pretty (rum/react *tabs-ds))]
     [:pre [:strong "WF:"] "\n"
      (d/pretty @*wf)]
     ]

    )
  )


; ----- state -------


(defonce *UI-STATE (atom
                     {
                      ;; wf state atom per each key
                      :tabs      (tabs-data-source
                                   [{
                                     :header    "test wf!"

                                     :ds        {
                                                 :init-fns (multi-select-data-source
                                                             []
                                                             (available-init-fns))

                                                 :ctx-fns (multi-select-data-source
                                                            []
                                                            (available-ctx-fns))
                                                 }

                                     :init-fns  []
                                     :ctx-fns   []
                                     :steps-fns []
                                     :opt-fns   []
                                     }
                                    ])




                      ;; resulting wf state
                      :wf        {}

                      ;; ui tests
                      :multi-ddl (multi-select-data-source
                                   ["a" "b"]
                                   [{:v "a" :text "a"}
                                    {:v "b" :text "b"}
                                    {:v "c" :text "c"}
                                    ])
                      ;;
                      :ddl       (atom [
                                        {:v "Tttt" :text "Tttt"}
                                        {:v "zzz" :text "azaza (zzz)"}

                                        ])

                      }))





;; how to stop running wf on reload??

;; root ui component for prototype
(def <app> #(<ui> *UI-STATE))
