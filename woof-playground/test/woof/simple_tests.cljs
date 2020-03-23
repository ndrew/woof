(ns woof.simple-tests
  (:require
    [cljs.core.async :as async]
    [cljs.test :refer-macros [use-fixtures deftest is testing async]]

    [woof.base :as base])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


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
                                       ;(.log js/console result)
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




(deftest wf-01-sid-pipelines

  ; split the collection and process each item by separate step handler
  (let [ctx-map {
                 :identity { :fn identity }

                 :range { :fn (fn [N] (vec (range N))) } ;; produces N numbers

                 ;; wrap each collection element into a new step
                 :map-id*  (base/expand-into :identity)

                 ;;
                 :map-element-transform* (base/expand-into :element-transform)

                 ;
                 :element-transform { :fn (fn [n] {:n n })}

                 ;; gathers sids back to a list
                 :*collect            {
                                       :fn       (fn [xs] xs)
                                       :collect? true
                                       }
                 }
        steps-map {
                   ::numbers [:range 5]     ; [0 1 2 3 4]

                   ::*numbers [:map-id* ::numbers]  ; [0] [1] [2] [3] [4]

                   ::*transformed [:map-element-transform* ::*numbers]  ; [0] [1] [2] [3] [4]

                   ::result [:*collect ::*transformed]
                   }
        ]

    (run-simple-wf ctx-map steps-map (fn [result]
                                       (is (= '({:n 0} {:n 1} {:n 2} {:n 3} {:n 4}) (::result result)))
                                       )))

  )


(deftest wf-02-sid-pipelines

  ; split the collection and process each item by separate step handler
  (let [ctx-map {
                 :identity { :fn identity }

                 :range { :fn (fn [N] (vec (range N))) } ;; produces N numbers

                 ;; wrap each collection element into a new step
                 :map-id*  (base/expand-into :identity)

                 ;;
                 :map-as-kv* (base/expand-into :as-kv)
                 :as-kv { :fn (fn [n] {:n n })}

                 ;; gathers sids back to a list
                 :*collect            {
                                       :fn       (fn [xs] xs)
                                       :collect? true
                                       }

                 }
        steps-map {
                   ::numbers [:range 4]     ; [0 1 2]
                   ::*numbers [:map-id* ::numbers]  ; [0 1 2] -> [[0] [1] [2]]
                   ::*kvs [:map-as-kv* ::*numbers]  ; [0->{:n 0}] [1->{:n 1}] [2->{:n 2}]
                   ::result [:*collect ::*kvs]
                   }
        ]

    ;; map
    (run-simple-wf ctx-map steps-map (fn [result]
                                       (is (= '({:n 0} {:n 1} {:n 2} {:n 3}) (::result result)))
                                       )))

  )



(deftest wf-03-kv-zipping
  ; kv-zipping is retrieving the sid and step value
  ; example: map-filter-reduce
  ;  we have a collection [0 1 2 3 4]
  ;  process each number separately
  ; -> 0
  ; -> 1
  ; ...
  ; -> 4
  ;

  (let [ctx-map {

                 ;; collection
                 :range { :fn (fn [N] (vec (range N))) }                    ;; produces N numbers


                 ;; singe
                 :identity { :fn identity }
                 :as-kv { :fn (fn [n] {:n n })}

                 ;;
                 :map*  (base/expand-into :identity)  ;; splits each element in collection to a separate step
                 :map-as-kv* (base/expand-into :as-kv)



                 ;; a way of memoizing the sid for kv zipping
                 :mem-k* {
                          :fn (fn [o] { (base/rand-sid "reduce-k") [:identity {:k o}] })
                          :expands? true
                          }

                 ;; kv zipping - joins keys with values
                 :*kv-zip {
                           :fn       (fn [[[k] vs]]
                                       (let [ks (:k k)] ;; todo: check for k and vs length, also flatten (at least for single el)
                                            (apply assoc {}  (interleave ks vs))))
                           :collect? true
                           }

                 ;; expand filter
                 :filter-odd {
                          :fn (fn [kv]
                                (reduce (fn [a [k v]]
                                          (if (odd? (:n v))
                                              (assoc a (base/rand-sid "filter-") [:identity k])
                                              a
                                              )) {} kv)
                                )
                          :expands? true
                          }

                 :*collect            {
                                       :fn       (fn [xs] xs)
                                       :collect? true
                                       }
                 }
        steps-map {
                   ::numbers [:range 5]     ; [0 1 2 3 4]

                   ;; [0 1 2 3 4] -> [0] [1] [2] [3] [4]
                   ::expanded-numbers [:map* ::numbers]
                   ; [0->{:n 0}] [1->{:n 1}] [2->{:n 2}]
                   ::items [:map-as-kv* ::expanded-numbers]
                   ; kv-zip ::items
                   ::ks-1 [:mem-k* ::items]                     ; store sids
                   ::items-kvzipped [:*kv-zip [::ks-1 ::items]] ;

                   ::filter [:filter-odd ::items-kvzipped]

                   ::result [:*collect ::filter]
                   }
        ]

    (run-simple-wf ctx-map steps-map (fn [result]
                                       (is (= '({:n 1} {:n 3}) (::result result))))))

  )



;; todo: add async examples
;; todo: add kv tests
;; todo: add map-filter-reduce


#_(deftest wf-04-nested-kv-zipping
  ; kv-zipping is retrieving the sid and step value
  ; example: map-filter-reduce
  ;  we have a collection [0 1 2 3 4]
  ;  process each number separately
  ; -> 0
  ; -> 1
  ; ...
  ; -> 4
  ;

  (let [ctx-map {
                 :identity { :fn identity }
                 ;; produces N numbers
                 :range { :fn (fn [N] (vec (range N))) }

                 ;; splits each element in collection to a separate step
                 :map*  (base/expand-into :identity)

                 ;; a way of memoizing the sid for kv zipping
                 :mem-k* {
                          :fn (fn [o] { (base/rand-sid "reduce-k") [:identity {:k o}] })
                          :expands? true
                          }

                 ;; kv zipping - joins keys with values
                 :*kv-zip {
                           :fn       (fn [[[k] vs]]
                                       (let [ks (:k k)] ;; todo: check for k and vs length, also flatten (at least for single el)
                                            (apply assoc {}  (interleave ks vs))))
                           :collect? true
                           }
                 }
        steps-map {
                   ::numbers [:range 5]     ; [0 1 2 3 4]

                   ::map [:map* ::numbers]  ; [0] [1] [2] [3] [4]
                   ::ks [:mem-k* ::map]     ; store keys

                   ::kv [:*kv-zip [::ks ::map]] ; get map
                   }
        ]

    (run-simple-wf ctx-map steps-map (fn [result]
                                       (let [kv (::kv result)
                                             ks (keys kv)

                                             kv-from-results (reduce (fn [m k]
                                                                       (assoc m k (get kv k))) {} (keys kv))
                                             ]

                                            (is (= [0 1 2 3 4] (sort (vals kv))))
                                            (is (= kv-from-results kv))
                                            )
                                       )))

  )

#_{

   ;; a way of memoizing the sid for kv zipping
   :mem-k* {
            :fn (fn [o] { (base/rand-sid "reduce-k") [:identity {:k o}] })
            :expands? true
            }

   ;; kv zipping - joins keys with values
   :*kv-zip {
             :fn       (fn [[[k] vs]]
                         (let [ks (:k k)] ;; todo: check for k and vs length, also flatten (at least for single el)
                              (apply assoc {}  (interleave ks vs))))
             :collect? true
             }

 ;; another way of doing kv zipping
 :reduce-v             {
                        :fn       (fn [o]
                                    {
                                     (base/rand-sid "V-") [:identity {:v o}]
                                     }
                                    )

                        :expands? true
                        :collect? true
                        }

 :zip-implicit {
                :fn       (fn [[[k] [v]]]

                            (let [ks (:k k)
                                  vs (:v v)]
                                 (apply assoc {}  (interleave ks vs))
                                 )
                            )
                :collect? true
                }

 }

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




#_(defn simplest-wf-initializer [*SWF]

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


;; fixme add test for stateful wf

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

