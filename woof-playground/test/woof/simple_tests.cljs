(ns woof.simple-tests
  (:require
    [cljs.core.async :as async]
    [cljs.test :refer-macros [use-fixtures deftest is testing async]]


    [woof.alpha.wf :as awf]
    [woof.v2.wf.stateful :as st-wf]
    [woof.playground.state :as state]

    [woof.utils :as u]

    [woof.base :as base]
    [woof.data :as d]
    [woof.wf :as wf])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]])
  )


;; way to run a wf as an async test
(defn test-wf [init-fns
               ctx-fns
               steps-fns
               opt-fns]

  (async ready
    (let [wf-impl
          (base/parametrized-wf!
            (base/combine-init-fns init-fns)
            identity ; wf-params-fn
            identity ; opt-params-fn
            (base/combine-fns
              (conj opt-fns
                    (fn [params]
                                 {
                                  ;; timeout
                                  :after-process (fn [exec-chann]
                                                   exec-chann)

                                  :op-handlers-map {
                                                    :done  (fn [result]
                                                      ;(.log js/console ::after-process)
                                                      (ready))

                                                    :error (fn [result]
                                                             (is (= "" (.-message result)))
                                                             (is (= "" (.-stack result)))

                                                             (.error js/console result)
                                                      ;(.log js/console ::after-process)
                                                      (ready))

                                                    }

                                  }))
              :merge-results base/merge-opts-maps)
            (base/combine-fns ctx-fns)
            (base/combine-fns steps-fns))
          ]
      (base/run-wf! wf-impl identity)
      )))


(defn run-simple-wf [ctx steps done-fn]

  (test-wf
    [] ;; init-fn
    [(fn [params] ctx)] ;; ctx-fn
    [(fn [params] steps)] ;; steps
    [(fn [params] ;; opts
       {:timeout 100
        :op-handlers-map {:done done-fn}})]
    )

  )

;;
;; --> start here
;;

(deftest wf-basics-01-simplest-wf

  ;; woof workflow is a way of defining and running computations.
  ;; let's start from simplest workflows to a more complex ones

  ;; workflow is defined by steps that need to be executed
  (let [steps-map {

                   ;; ::step-id [:step-handler <step-id>]
                   ;; so it's basically
                   ;; ::step-1 = step-handler("hello")

                   ;; step-id is always a qualified keyword
                   ::step-1 [
                             :step-handler "hello"
                             ]
                   ::step-2 [:step-handler "world"]
                   }

        ;; step handlers are defined in context map
        ctx-map {
                 ;; step-hanlder-id is a simple keyword
                 :step-handler {:fn (fn [v] v)}
                 }

        ;; if wf worked - then we'll receive the following result
        ;; note that wf is almost always asynchronous.
        expected-result {
                         ::step-1 "hello"
                         ::step-2 "world"
                         }
        ]
    (run-simple-wf ctx-map steps-map (fn [result] (is (= expected-result result))))

    ;; why bother to use wf?

    ;; * async
    ;; * order is not important if step are not linked
    ;; * steps and ctx are data - hence are composable
    ;; * same executor, different wfs

    )
  )

(deftest wf-basics-02-nesting-steps
  ; usually we want a wf to behave like pipeline, so there should be nesting steps, like f(g(x))

  (let [ctx-map {
                 :double {:fn (fn [i] (* 2 i))}
                 :n      {:fn (fn [i] i)}
                 }
        steps-map {
                   ::4   [:n 4]
                   ::2xn [:double ::4]  ;; FIXME: no error on unknown key
                   }

        expected-result {
                         ::4   4
                         ::2xn 8
                         }
        ]

    (run-simple-wf ctx-map steps-map (fn [result]
                                       (is (= expected-result result))
                                       )))

)


(deftest wf-basics-03-steps-w-multiple-params

  ;; "collect? steps - handling multi-arity"
  (let [ctx-map {
                 :add {
                       :fn       (fn [args]
                                   (apply + args))
                       :collect? true
                       }
                 :n   {:fn (fn [i] i)}
                 }
        steps-map {
                   ::4      [:n 4]
                   ::5      [:n 5]
                   ::10     [:n 10]

                   ;; pass a sid-list to a collect step
                   ::4+5+10 [:add [::4 ::5 ::10]]
                   }

        expected-result {

                         ::4      4
                         ::5      5
                         ::10     10
                         ::4+5+10 (+ 4 5 10)
                         }
        ]

    (run-simple-wf ctx-map steps-map (fn [result] (is (= expected-result result)))))
  )






(deftest wf-basics-04-expand_steps

  ; another option instead of returning collections - is to return 'new steps' into a wf
  (let [ctx-map {
                 :split-str {
                             :fn       (fn [s]
                                         (let [ss (clojure.string/split s #" ")
                                               curr-ns (namespace ::just-give-me-a-ns)]

                                              (reduce (fn [a v]
                                                        (assoc a (keyword (str curr-ns "/"  v)) [:v v])) {} ss)

                                              )
                                         )
                             :expands? true
                             }
                 :v         {:fn identity}
                 }
        steps-map {
                   ::split [:split-str "a b c d e f"]
                   }

        ;; if wf worked - then we'll receive the following result
        ;; note that wf is almost always asynchronous.
        expected-result {
                         ::split '(::a ::b ::c ::d ::e ::f)

                         ::a "a"
                         ::b "b"
                         ::c "c"
                         ::d "d"
                         ::e "e"
                         ::f "f"
                         }
        ]

    (run-simple-wf ctx-map steps-map (fn [result]
                                       (.log js/console result)
                                       (is (= expected-result result)))))
  )



(deftest wf-basics-05-expands-step-returning-multiple-values

  (let [ctx-map {
                 :split-str    {
                                :fn       (fn [s]
                                            (let [ss (clojure.string/split s #" ")
                                                  curr-ns (namespace ::just-give-me-a-ns)]

                                                 (reduce (fn [a v]
                                                           (assoc a (keyword (str curr-ns "/" v)) [:v v])) {} ss)

                                                 )
                                            )
                                :expands? true
                                }
                 :reverse-join {
                                :fn       (fn [args]
                                            (clojure.string/join (reverse args))
                                            )
                                :collect? true
                                }

                 :v            {:fn identity}
                 }
        steps-map {
                   ::split    [:split-str "a b c d e f"]
                   ::reversed [:reverse-join ::split]
                   }

        expected-result {
                         ::split    '(::a ::b ::c ::d ::e ::f)

                         ::a        "a"
                         ::b        "b"
                         ::c        "c"
                         ::d        "d"
                         ::e        "e"
                         ::f        "f"

                         ::reversed "fedcba"
                         }
        ]

    (run-simple-wf ctx-map steps-map (fn [result]
                                       (is (= expected-result result)))))
  )

;; todo: add async examples
;; todo: add kv tests
;; todo: add map-filter-reduce



#_(deftest simplest-wf-1

  (test-wf
    ;; init-fn
    [(fn [params] { :t (u/now) })]
    ;; ctx-fn
    [(fn [params]
       {
        :id {:fn (fn [v]
                   ;(prn v (mod v 2))
                   #_(if (= 1 (mod v 3))
                       (u/throw! "aaaaaaaa")
                       v
                       )
                   v

                   )}
        })
     ]
    ;; steps
    [(fn [params]
       {
        ::ping [:id (:t params)]
        }
       )]
    [(fn [params]
       {

        :op-handlers-map {
                          :done  (fn [result]
                                   (.log js/console ::done (d/pretty result))

                                   ;; (is (= :foo :bar))
                                   )
                          :error (fn [result]
                                   (.log js/console ::error (d/pretty result))

                                   )
                          }
        })]
    )
  )




(defn simplest-wf-initializer [*SWF]

  ;;
  (prn "initializer")
  {

   :init-fns   [(fn [params]

                  (prn "init-fns")

                  {:IN :some-wf-parameters}
                  )
                ]

   :ctx-fns    [(fn [params]
                  {
                   :test  {:fn (fn [v] v)}
                   :wait {
                          :fn       (fn [t]
                                      (let [ch (async/chan)]

                                           (go
                                             (async/<! (u/timeout t))
                                             (async/put! ch "DONE")
                                             )

                                           ch
                                           ))

                          }
                   }
                  )]

   :steps-fns  [(fn [params]
                  {
                   ::step [:test "Hello!"]
                   ::wait [:wait 300]
                   })

                ]
   :opt-fns    [
                (fn [params]
                    {
                     :op-handlers-map {
                                       :done (fn [result]
                                               (swap! *SWF assoc ::result result)
                                               (.log js/console "Test WF is DONE" result)
                                               )
                                       ;; :error close!
                                       }
                     })]


   }
  )


;; test runner based on async


#_(deftest alpha_wf_test__1

  (testing "stateful representation of the wf"
    ;; wrap wf to a state atom.
    ;; start with an empty (default) state map
    ;; then add needed aspects atop - for example ui

    ;; fixme: maybe there is some nicer way of wrapping state

    (let [initial-state (state/empty-swf :dummy)
          wf-init-fn simplest-wf-initializer


          t-fn (fn [*SWF]
                 (let [nu-wf-state (wf-init-fn *SWF)
                       updated-state (merge
                                       initial-state
                                       nu-wf-state)
                       ]

                      (swap! *SWF merge  updated-state)
                      (.log js/console @*SWF)
                      )
                 )
          ; produce a map with :woof.v2.wf.stateful/init-wf
          wf-map  (st-wf/wf :dummy t-fn)

          *wf (atom wf-map)
          ]

      ;; this one is ugly as we use state as source of the code

      ;; do init a wf
      (st-wf/wf-init! *wf)

      ;; run wf
      (state/swf-run! *wf)

      (async ready
        (do
          (prn "wait for wf..")
          (js/setTimeout (fn []
                           (.log js/console "FULL RESULT" @*wf)

                           (ready)
                           ) 3000))))))

