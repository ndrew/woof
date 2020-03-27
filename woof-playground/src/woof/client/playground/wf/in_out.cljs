(ns ^:figwheel-hooks woof.client.playground.wf.in-out
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]

    [woof.base :as base]
    [woof.client.playground.ui :as ui]
    [woof.client.playground.ui.wf :as wf-ui]
    [woof.client.stateful :as st-wf]
    [woof.wf :as wf]
    [woof.utils :as utils]
    [woof.data :as d]

    [woof.playground.v1.utils :refer [dstr kstr vstr]]
    [viz.core :as viz])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; in-out stuff

(defn merge-IN-OUT-maps [a b]
  (merge {} a b {:IN (clojure.set/union (get a :IN #{}) (get b :IN #{}))
              :OUT (clojure.set/union (get a :OUT #{}) (get b :OUT #{}))}))

(defn merge-with-IN-OUT-meta [a b]
  (let [m-1 (meta a)
        m-2 (meta b)]
    (with-meta
      (merge a b)
      (merge-IN-OUT-maps m-1 m-2)
      )
    )
  )



(defn in-out-merge [META r]
  (if-let [m (meta r)]
    (swap! META (fn [a b]
                  (assoc a
                         :IN (clojure.set/union (:IN a) (get b :IN #{}))
                         :OUT (clojure.set/union (:OUT a) (get b :OUT #{}))
                         )
                  m))
    (utils/throw! (str "No metadata attached to " (pr-str r)))))


(defn in-out-meta-atom []
  (atom {:IN  #{}
         :OUT #{}})
  )

(defn conj-IN! [*meta k]
  (swap! *meta update :IN conj k))


(defn conj-OUT! [*meta k]
  (swap! *meta update :OUT conj k)
  )


(defn init-fn-w-meta [params]
  ; inject a meta atom into a wf
  (merge params {:META (in-out-meta-atom)}))



(rum/defc <kv-w-meta> < rum/state
  [k v]

  [:div {:style {:outline "1px solid rgba(0,0,0,.1333)"}}
   [:span (pr-str k)]
   [:pre (d/pretty v)]
   (if-let [m (meta v)]
     [:pre "meta:\n\n" (d/pretty m)])

   ]
  )


(defn short-key [k]
  (clojure.string/replace (pr-str k) #"woof.client.playground.wf." "")
  )


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









(defn graph-to-svg [steps rfn]
  (let [graphviz-edges (reduce rfn "" steps)]
    (viz/image (str "digraph { " graphviz-edges " }")))
  )



(rum/defc <in-out-wf> < rum/reactive
  [wf]


   (let [result (:result wf)
         in-out-map (meta result)
         IN (:IN in-out-map)
         OUT (:OUT in-out-map)

         initial-data (get-in wf [:runtime :initial])

         params (:params initial-data)



         ]
     [:div ;; .proto7

      (try
        (ui/<results-ui> "RESULTs"
                         initial-data
                         result)
        (catch js/Error e
          [:pre (pr-str e)]
          )
        )



      [:hr]


      (<kv-w-meta> :IN (select-keys params IN))
      (<kv-w-meta> :OUT (select-keys result OUT))


      (<kv> "results" result)

      (ui/<debug> result)

      ;; how to keep the steps up to date
      #_[:div.graph
       {:dangerouslySetInnerHTML
        {:__html (graph-to-svg initial-steps (fn [gviz [k [action param]]]
                                       (if (wf/sid? param)
                                           (str gviz " "
                                                (clojure.string/replace (name param) #"-" "_")
                                                " -> "
                                                (clojure.string/replace (name k) #"-" "_")
                                                ";\n")
                                           gviz
                                           )
                                       ))}}]

      ;(<kv-w-meta> :full-params params)

      ;(<kv-w-meta> :context-map (get captured-wf :context-map))
      ;(<kv-w-meta> :steps (get captured-wf :steps))

      ;(wf-ui/<default-wf-body-ui> wf)
      ]
     )


  )



(defn initialize-in-out-wf [*wf]
    {
     :title      "IN OUT workflow"

     :explanation [:div
                   [:p "In this workflow we try maintaining meta data for workflow through full workflow lifecycle."]

                   [:p "We add the " [:code "init-fn"] " that creates a metadata atom via "
                    [:code "base/build-init-meta-fn"]  " (accessor " [:code "base/&*meta"] "), so consequent "

                    [:code "init-fn"] "s can pass some metadata further to " [:code "ctx-fn"]
                    " and " [:code "steps-fn"]]

                    [:p [:code "ctx-fn"] " can use metadata for providing prefixed "
                     "step handlers - in case of name collision, or several versions of the step handler"
                    ]

                    [:p [:code "steps-fn"] " can use metadata for specifying additional info regarding steps, "
                     "especially expanded"
                     ]

                    [:p [:b "<?>"] " How this is different from doing it via params? Maybe just use separate playground context "
                     "— for explicitly stating what is a result"]

                    [:p "Also we use this workflow for refining the UI that will show workflow progress"]
                    ]

     :init-fns         [(fn [params]
                          (let [params-map {:some-numbers [1 2 3 4 5]}]

                              ;; store in meta params IN metadata
                               (swap! (base/&*meta params) update :IN conj :some-numbers)

                               params-map
                               )
                          )
                        (base/build-init-meta-fn)
                        st-wf/chan-factory-init-fn
                        ]

     :ctx-fns          [(fn [params]
                          {
                           :test {:fn (fn [v] v)}

                           ;; math
                           :+<   {:fn       (fn [xs]
                                              (into (array-map)
                                                    (map-indexed (fn [i x]
                                                                   [(wf/rand-sid) [:v x]]) xs)))
                                  :expands? true
                                  }
                           :v    {:fn (fn [x] x)}
                           :+>   {
                                  :fn       (fn [xs] (reduce + xs))
                                  :collect? true
                                  }

                           }
                          )

                        ]

     :steps-fns        [;
                        ;; combine that will preserve meta
                        (fn [params]
                          (swap! (base/&*meta params) update :OUT conj ::step-1)
                            {
                             ::step-1        [:test "step-1"]
                             ::hidden-step-1 [:test "hidden-step"]
                             }
                            )

                        (fn [params]
                          (swap! (base/&*meta params) update :OUT conj ::step-2)

                          {
                           ::step-2 [:test "step-2"]
                           }
                          )

                        (fn [params]
                          (let [expander-sid (wf/rand-sid "add-p-")
                                sum-result-sid (wf/rand-sid "sum-")
                                ]

                               (swap! (base/&*meta params) update :OUT conj sum-result-sid)
                               {
                                expander-sid [:+< [1 2 3]]
                                sum-result-sid [:+> expander-sid]
                                }
                               )

                          )

                        ]

     :opt-fns          [(base/build-opt-on-done (fn [params result] (with-meta result @(base/&*meta params))))
                        st-wf/chan-factory-opts-fn]

     ;; how to provide a custom ui for actions - we need to pass state here
     :ui-fn            (partial wf-ui/<default-wf-ui> <in-out-wf>)

     :wf-actions       {
                        ; :not-started []
                        :running [

                                  #_["ui event" (fn []
                                                  (let [loop-chan (st-wf/&wf-init-param *wf ::evt-loop-chan)]
                                                       (async/put! loop-chan
                                                                   {(wf/rand-sid "ui-") [:test (u/now)]})
                                                       )
                                                  )]

                                  ]
                        ; :done        []
                        }

     }

  )
