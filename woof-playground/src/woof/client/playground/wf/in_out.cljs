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

  [:div

   (let [captured-wf (get-in wf [:runtime :initial])]
     [:div

      (<kv> :params (get captured-wf :params))
      (<kv> :context-map (get captured-wf :context-map))
      (<kv> :steps (get captured-wf :steps))
      ;(<kv> )
      ]
     )


   [:pre (d/pretty (:result wf))]
   [:pre (d/pretty (meta (:result wf)))]

   ;(wf-ui/<default-wf-body-ui> wf)
   ]
  )


;; todo: what if don't know which steps will be resulting
;; maybe via :op-handlers-map :done
;; but it doesn't change the data



(defn initialize-in-out-wf [*wf]
  (let [META (in-out-meta-atom)]
    {

     :init-fns  [; init-evt-loop
                 (fn [params]
                   (let [IN (with-meta
                              {:data "woof"}
                              ; how to distinguish between step and opt param??
                              {:IN #{:data}})]
                        (in-out-merge META IN)
                        ; or always use all in keys
                        ;(swap! META update :IN into (keys IN))
                        IN)
                   )
                 st-wf/chan-factory-init-fn
                 ]

     ;; this will merge the
     :merge-results-fn merge-with-IN-OUT-meta

     :ctx-fns   [;ctx-evt-fn

                 ;; (in-out-merge META steps)

                 (fn [params]
                   {
                    :test {:fn (fn [v] v)}

                    }
                   )]

     :steps-fns [;
                 ;; combine that will preserve meta
                 (fn [params]
                   (with-meta
                   {
                    ::step-1 [:test "step-1"]
                    ::hidden-step-1 [:test "hidden-step"]
                    }
                   {:OUT #{::step-1}}))

                 (fn [params]
                   (with-meta
                     {
                      ::step-2 [:test "step-2"]
                      }
                     {:OUT #{::step-2}}))

                 ]

     :opt-fns   [;; ls/ls-opts-fn

                 (base/build-opt-on-done (fn [result]
                                           (with-meta result @META)))

                 st-wf/chan-factory-opts-fn
                 ]


     ;; how to provide a custom ui for actions - we need to pass state here
     :ui-fn      (partial wf-ui/<default-wf-ui> <in-out-wf>)

     ;:title      "Workflow with Event Loop and custom UI"

     ; :explanation [:div  "this is the explanation for the workflow"]

     :wf-actions {
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

  )
