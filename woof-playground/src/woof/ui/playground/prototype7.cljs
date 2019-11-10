(ns ^:figwheel-hooks woof.ui.playground.prototype7
  (:require
    [cljs.core.async :as async]
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



    [woof.ui.playground.common :as cmn]

    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;;



;; -------


(defonce *UI-STATE (atom
                     {
                      ;; wf state atom per each key
                      }))


(declare <ui>)                                              ;; (defc <ui> []) - exported ui component


(def init! (partial cmn/default-init! *UI-STATE))
(def reload! (partial cmn/default-reload! *UI-STATE))



(defn WF [nu-params context-map-fn steps-fn wf-params]
  (reify WoofWorkflow
    (get-params [this] nu-params) ;; is this really needed
    (get-context-map [this] (context-map-fn nu-params))
    (get-steps [this] (steps-fn nu-params)))
  )

(defn capturing-WF [*wf nu-params context-map-fn steps-fn wf-params]
  (reify WoofWorkflow
    (get-params [this] nu-params) ;; is this really needed
    (get-context-map [this] (let [ctx-map (context-map-fn nu-params)]
                              (swap! *wf assoc ::ctx ctx-map)
                              ctx-map))
    (get-steps [this] (let [steps (steps-fn nu-params)]
                        (swap! *wf assoc ::steps steps)
                        steps
                        )))
  )


(defn parametrized-wf!
  "parametrized wf implementation
  init-fn (fn []) => {..}
  "
  [init-fn        ;; returns initial maps
   wf-params-fn   ;; transforms initial map to a wf params
   opt-params-fn  ;; transforms wf params to opt params
   opts-fn        ;; provides opts map via opt params
   context-map-fn ;; provides context map from wf params
   steps-fn       ;; provides steps map from wf params
   workflow-fn
   ]
  (let [wf-fn (fn [params]
                (let [nu-params (wf-params-fn params)]
                  {
                   :params nu-params
                   :wf     (partial workflow-fn nu-params context-map-fn steps-fn)
                   }
                  )
                )
        opts-fn (fn [opts-params]
                  (let [nu-opt-params (opt-params-fn opts-params)]
                    {
                     :params nu-opt-params
                     :opts   (opts-fn nu-opt-params)
                     })
                  )
        ]
    {
     :init-fn init-fn
     :wf-fn   wf-fn
     :opts-fn opts-fn
     }
    )
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

          wf (parametrized-wf!
               (base/combine-init-fns init-fns)
               wf-params-fn
               opt-params-fn

               (base/combine-fns opt-fns :merge-results base/merge-opts-maps)
               (base/combine-fns ctx-fns)
               (base/combine-fns steps-fns)
               ;; todo: migrate base/capturing-WF
               ;; todo: how to use here your ::ctx and ::steps
               (partial capturing-WF *wf)
               )]

      ; store wf
      (swap! *wf merge wf)
      (swap! *wf assoc ::status :running)
      (base/run-wf! wf identity)
      )
    ))


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


(defn short-key [k]
  (clojure.string/replace-all (pr-str k) #":woof.ui.playground.prototype7/" "::"))


(defn short-value [v]
  (if (utils/channel? v)
    "<channel>"
    (d/pretty v)
    )
  )


(rum/defcs <kv> < rum/static (rum/local true ::show?)
  [local heading results]

  (let [show? @(::show? local)]
    (into [:div
           (ui/menubar heading [[(if show? "↑" "↓")
                                 (fn []
                                   (swap! (::show? local) not))]])
           ]
          (if show?
            (map (fn [[k v]]
                   [:div.kv
                    [:.k (short-key k)]

                    ; maybe pass steps and ctx here
                    ; [:.step "aaa"]

                    ; how to know how to show value
                    [:.v (short-value v)]]
                   ) results)
            []
            )
          )
    )
  )


(rum/defcs <results> < rum/static (rum/local true ::show?)
  [local heading
   ctx-map
   initial-steps
   results]

  (let [show? @(::show? local)]
    (into [:div
           (ui/menubar heading [[(if show? "↑" "↓")
                                 (fn []
                                   (swap! (::show? local) not))]])
           ]
          (if show?
            (let [tree (reduce (fn [a [k [step-id v]]]
                                 (let [ctx (get ctx-map step-id)
                                       expands? (get ctx :expands?)
                                       modifier (clojure.string/join " "
                                                  [(if (get ctx :infinite) "i" "")
                                                  (if (get ctx :collect) "c" "")
                                                  (if (get ctx :expands?) "e" "")]

                                                  )

                                       ]
                                   (assoc a k {
                                               :step [step-id v]
                                               :res (get results k)
                                               :ctx ctx
                                               :modifier (clojure.string/trim modifier)
                                               :expands? expands?
                                               }))
                            ) (sorted-map) initial-steps)]
              (map (fn [[k v]]
                     [:div {:style {:outline "1px solid red"}}
                      [:.kv
                       [:.k (clojure.string/trim
                              (str
                                (short-key k)))]

                       [:.v
                        (pr-str (:step v))
                        (if (= "" (:modifier v)) "" (str " — (" (:modifier v) ")"))
                        ]
                       ]


                      [:.kv

                       (if (:expands? v)
                         (into
                           [:.v]
                           (map (fn[a] [:div.kv {:style {:margin-left "1rem"}}
                                        [:.k (short-key a)]
                                        [:.v (d/pretty (get results a))]
                                        ] ) (:res v))
                           )
                         [:.v
                          (str
                            (short-value (:res v))
                            )
                          ]
                         )

                       ]

                      ]
                     ) tree)
                           )
            []
            )
          )
    )

  )



(defn example-wf [*wf]
  {:init-fns [(fn [params] (prn params))]
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
   :opt-fns   [(fn [params]
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
                 )]
   }
  )

(rum/defcs <ui> < rum/reactive (rum/local {} ::*wf)
  [local *STATE]
  (let [*wf (::*wf local)]
    [:div.proto7
     [:h1 "capture final workflow"]

     (let [wf @*wf
           status (get wf ::status)]
       [:div.capturing-wf

        (ui/btn "run"
                (fn []
                  (run-wf! *wf
                           (example-wf *wf)
                           )))
        (ui/btn "stop" (fn []
                         (base/end! (::xtor wf))

                         ;; <?> how to close infinite channels
                         ; (utils/close-channels! (vals (::result wf)))
                         ))

        [:div.status (pr-str (get wf ::status))]
        (if status
          [:div.flex
           (<kv> "context" (::ctx wf))
           (let [ctx-map (::ctx wf)
                 initial-steps (::steps wf)
                 results (::result wf)
                 ]
             (<results> "steps+results"
                        ctx-map
                        initial-steps
                        results
                        )
             )

           (<kv> "results" (::result wf))
           ]
          )

        ]
       )

     [:pre (d/pretty @*wf)]
     ]

    )
  )

;; example for workflows

;; data config -> running program







;; how to stop running wf on reload??

;; root ui component for prototype
(def <app> #(<ui> *UI-STATE))
