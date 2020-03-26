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
    [woof.data :as d])
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



(rum/defc <kv> < rum/state
  [k v]

  [:div {:style {:outline "1px solid rgba(0,0,0,.1333)"}}
   [:span (pr-str k)]
   [:pre (d/pretty v)]
   (if-let [m (meta v)]
     [:pre "meta:\n\n" (d/pretty m)])

   ]
  )

(rum/defc <in-out-wf> < rum/reactive
  [wf]


   (let [result (:result wf)
         in-out-map (meta result)
         IN (:IN in-out-map)
         OUT (:OUT in-out-map)

         captured-wf (get-in wf [:runtime :initial])
         params (get captured-wf :params)
         ]
     [:div


      (<kv> :IN (select-keys params IN))
      (<kv> :OUT (select-keys result OUT))

      ;(<kv> :full-params params)

      ;(<kv> :context-map (get captured-wf :context-map))
      ;(<kv> :steps (get captured-wf :steps))

      ;[:pre (d/pretty result)]
      ;[:pre (d/pretty in-out-map)]

      ;(wf-ui/<default-wf-body-ui> wf)
      ]
     )


  )


;; todo: what if don't know which steps will be resulting
;; maybe via :op-handlers-map :done
;; but it doesn't change the data



(defn initialize-in-out-wf [*wf]
    {

     :init-fns         [; init-evt-loop
                        (fn [params]
                          (let [params-map {:some-numbers [1 2 3 4 5]}]

                              ;; store in meta params IN metadata
                               (swap! (:META params) update :IN conj :some-numbers)

                               params-map
                               )
                          )
                        (base/build-init-meta-fn)
                        st-wf/chan-factory-init-fn
                        ]

     :ctx-fns          [;ctx-evt-fn
                        ;; (in-out-merge META steps)
                        (fn [params]
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
                                  :fn       (fn [xs]

                                              (reduce + xs))
                                  :collect? true
                                  }

                           }
                          )

                        ]

     :steps-fns        [;
                        ;; combine that will preserve meta
                        (fn [params]
                          (swap! (:META params) update :OUT conj ::step-1)
                            {
                             ::step-1        [:test "step-1"]
                             ::hidden-step-1 [:test "hidden-step"]
                             }
                            )

                        (fn [params]
                          (swap! (:META params) update :OUT conj ::step-2)

                          {
                           ::step-2 [:test "step-2"]
                           }
                          )

                        (fn [params]
                          ;;                              ::addp   [:+< [1 2 3]]
                          ;                             ::add    [:+> ::addp]
                          (let [expander-sid (wf/rand-sid "add-p-")
                                sum-result-sid (wf/rand-sid "sum-")
                                ]

                               (swap! (:META params) update :OUT conj sum-result-sid)
                               {

                                expander-sid [:+< [1 2 3]]
                                sum-result-sid [:+> expander-sid]
                                }
                               )

                          )

                        ]

     :opt-fns          [;; ls/ls-opts-fn

                        (base/build-opt-on-done (fn [params result]
                                                  (with-meta result @(:META params))
                                                  ))

                        st-wf/chan-factory-opts-fn
                        ]


     ;; how to provide a custom ui for actions - we need to pass state here
     :ui-fn            (partial wf-ui/<default-wf-ui> <in-out-wf>)

     :title      "IN OUT workflow"

     ; :explanation [:div  "this is the explanation for the workflow"]

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
