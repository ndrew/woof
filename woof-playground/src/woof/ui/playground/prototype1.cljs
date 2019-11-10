(ns ^:figwheel-hooks woof.ui.playground.prototype1
  (:require
    [cljs.reader]
    [cljs.core.async :as async]

    [rum.core :as rum]

    [woof.base :as base]
    [woof.data :as d]
    [woof.ui.playground.common :as cmn]
    [woof.utils :as utils]

    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; the idea with parametrized wf - is that we can combine inner wf functions from smaller (potentially re-usable) functions
;; an example wf that illustrates this is a wf that stores meta-data about IN-OUT parameters



;; wf state atom

(defonce
  *UI-STATE
  (atom
    {
     ;; workflow model atom
     :wf {
          :status :compile

          ;; store the result map, possibly with meta data
          :RESULT {}
          }
     }))


;; --- exports

(def init! (partial cmn/default-init! *UI-STATE))
(def reload! (partial cmn/default-reload! *UI-STATE))


;; in-out stuff

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




;; common

(defn run-workflow! [init-fn
                     opts-fn
                     ctx-fn
                     steps-fn
                     ]
  (let [wf (base/parametrized-wf!
             init-fn
             identity
             identity
             opts-fn
             ctx-fn
             steps-fn)]
    (base/run-wf! wf identity))
  )


;; parametrized opts wf
(defn default-opts-fn [RESULT-fn opts-params]

  {
   :before-process  (fn [wf-chan xtor]
                      ;;(swap! *wf assoc :status :running)
                      (.log js/console "wf started")

                      :ok
                      )
   :op-handlers-map {
                     :error   (fn [err]
                                (.error js/console err))

                     :process (fn [interm-result]
                                ;; how to find which are working
                                )
                     :done    (fn [result]
                                (RESULT-fn opts-params result)
                                )
                     }
   }
  )

(defn stateful-opts-fn [*wf params]
  {
   :before-process  (fn [wf-chan xtor]
                      ;; todo: store xtor
                      (swap! *wf assoc :status :running)
                      ;; (.log js/console "external opts start")
                      :ok)
   :op-handlers-map {
                     :done (fn [result]
                             (swap! *wf assoc
                                    :status :result
                                    :RESULT result)
                             (.log js/console "DONE:" result)

                             result
                             )
                     }
   }
  )

;; wfs


(defn run-wf-1 [*wf]
  (let [result-fn (fn [params result]
                    (let [result-w-meta (with-meta
                                          (merge
                                            (:IN params)
                                            result)
                                          @(:META params))]

                      (.log js/console "WF-1 is done" result-w-meta)

                      result-w-meta))

        init-1 (fn [params]
                 (merge params
                        {:STATE (atom {:hello "woof!"})}))

        init-data (fn [params]
                    (prn "init-1-data")
                    (conj-IN! (:META params) ::data)

                    (merge params
                           {:IN {::data (:hello @(:STATE params))}})
                    )
        ]

    (run-workflow!
      ;; TODO: which one to use? combined or init-fn-1???
      (base/combine-init-fns*
        init-fn-w-meta
        init-1
        init-data)
      (base/combine-fns
        [(partial default-opts-fn result-fn)
         (partial stateful-opts-fn *wf)]
        :merge-results base/merge-opts-maps)
      (fn [params]
        {
         :yo {:fn identity}
         })
      (base/arg-fn                                          ;; provides steps map from wf params (multi-arity)
        (fn [& {:keys [IN META]}]
          (let [steps {
                       ::0 [:yo (::data IN)]}]
            (swap! META update :OUT conj ::0)
            steps))
        )
      )
    )
  )


(defn run-wf-2 [*wf]
  (let [result-fn (fn [params result]
                    (let [*META (:META params)
                          IN (:IN params)

                          result-w-meta (with-meta
                                          (merge IN result)
                                          @*META)]

                      (.log js/console "WF-2 is done" result-w-meta)

                      result-w-meta
                      )
                    )
        ]


    (run-workflow! (base/combine-init-fns*
                     init-fn-w-meta
                     (fn [params]
                       ;; in
                       (conj-IN! (:META params) ::data)
                       (merge params
                              {:IN {::data "woof"}})))
                   (base/combine-fns
                     [(partial default-opts-fn result-fn)
                      (partial stateful-opts-fn *wf)]
                     :merge-results base/merge-opts-maps)
                   (fn [params]
                     {
                      :yo {
                           :fn (fn [a] a)
                           }
                      }
                     )
                   (fn [params]
                     (let [*META (:META params)
                           IN (:IN params)
                           steps {
                                  ::0 [:yo (::data IN)]
                                  }]
                       (conj-OUT! *META ::0)
                       steps)
                     )
                   )
    )
  )




(defn run-wf-3 [*wf]
  (let [META (in-out-meta-atom)

        ;; wf 3

        ctx-fn-3 (fn [& r]
          ;; meta stuff should be wrapped inside
          {
           :yo       {:fn (fn [a]
                            "yo!"
                            )}
           :yo-async {:fn (fn [a]
                            (let [c (async/chan)]
                              (go
                                (async/<! (utils/timeout 3000))
                                (async/put! c a))
                              c))
                      }
           }
          )

        steps-fn-3 (fn [& r]

          ;; todo: what if don't know which steps will be resulting
          ;; maybe via :op-handlers-map :done
          ;; but it doesn't change the data

          ;; todo: what if we return :IN here
          ;; how to merge it
          (with-meta
            {
             ::0 [:yo-async "!!!"]
             }
            {:OUT #{::0}})
          )

        wrapped-init-fn (fn []
                          (let [IN (with-meta
                                     {:data "woof"}
                                     ; how to distinguish between step and opt param??
                                     {:IN #{:data}})]
                            (in-out-merge META IN)
                            ; or always use all in keys
                            ;(swap! META update :IN into (keys IN))
                            IN))

        ]



    (run-workflow! wrapped-init-fn

                   ;; can result be enriched via
                   (base/combine-fns
                     [(partial default-opts-fn (fn [params result]
                                                 (.log js/console "WF-3 is done")
                                                 ;; enrich the result with meta
                                                 (with-meta (assoc
                                                              result :testo :pesto)
                                                            @META)
                                                 ))
                      (partial stateful-opts-fn *wf)
                      ]
                     :merge-results base/merge-opts-maps)

                   (base/arg-fn ctx-fn-3)
                   (fn [params]
                     (let [steps ((base/arg-fn steps-fn-3) params)]
                       (in-out-merge META steps)

                       steps
                       )
                     )
                   )

    )

  )



;; ui

(rum/defc <wf> < rum/reactive [*wf run-fn!]
  (let [wf @*wf
        status (:status wf)]
    [:div
     [:pre (pr-str wf)]

     (if (= :compile status)
       [:div [:button {:on-click (fn [e]
                                   (run-fn! *wf))} "run!"]])

     (if (= :running status)
       [:div "wf is working..."])

     (if (= :result status)
       (let [result (:RESULT wf)]
         [:div
          [:button {:on-click (fn [e]
                                (swap! *wf assoc :status :compile
                                       :RESULT nil)
                                )} "ok"]
          [:header "data"]
          [:pre (d/pretty result)]
          [:header "meta"]
          [:pre (d/pretty (meta result))]
          ]))
     ]))


(rum/defcs <ui> < rum/reactive
                  (rum/local 2 ::wf-n)
  [local *STATE]

  [:div
   [:pre (pr-str @*STATE)]
   [:hr]

   [:button {:on-click (fn [e] (reset! (::wf-n local) 1))} "1"]
   [:button {:on-click (fn [e] (reset! (::wf-n local) 2))} "2"]
   [:button {:on-click (fn [e] (reset! (::wf-n local) 3))} "3"]

   (condp = @(::wf-n local)
     1 (<wf> (rum/cursor-in *STATE [:wf]) run-wf-1)
     2 (<wf> (rum/cursor-in *STATE [:wf]) run-wf-2)
     3 (<wf> (rum/cursor-in *STATE [:wf]) run-wf-3)
     )
   ]

  )

(def <app> #(<ui> *UI-STATE))

