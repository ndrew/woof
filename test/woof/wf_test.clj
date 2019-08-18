(ns woof.wf-test
  (:require
    [clojure.core.async :as async :refer [go go-loop]]
    [clojure.test :refer :all]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.wf-data :as wdata]

    [woof.utils :as u]
    [woof.test-data :as test-data]
    [woof.graph :as g]

    [criterium.core :as criterium]


))



; 1) context - contains step handlers available to a workflow

(defonce SAMPLE-CONTEXT {

    :hello {:fn (fn [a]                  ;; each step handler accepts 1 argument
                  (str "Hello " a))}

    :timeout200 {:fn (fn [a]
                       (Thread/sleep 200)
                       (str "Hello " a))}
})



;; runs the simple workflow

(deftest simplest-pipeline

  (let [; workflow is described by a flat map with finite number of steps
        steps {
                ;; each step has it's unique id and step definition
                ;; step definition is hiccup style vector where first el is step-id and second is step parameters
                ::0 [:hello "World!"]
                ::1 [:hello "Woof!"]}

         context (wf/make-context SAMPLE-CONTEXT)

         ; from context (for now mutable) and steps - we create an executor
         executor (wf/build-executor context steps)]

    ;; execute workflow synchronously (clojure only)
    (let [v @(wf/sync-execute! executor)]
      (is (not (nil? v)))

      ;; we can use exec/extract-results to get only step results we need
      (is (= (wdata/extract-results v [::0]) {::0 "Hello World!"}))
      (is (= (wdata/extract-results v [::1]) {::1 "Hello Woof!"}))))
)




;; runs the nested workflow

(deftest nesting-results-pipeline

  (let [context-map {
                  :producer {:fn (fn [a] "world!")}
                  :consumer {:fn (fn [a] (str "Hello " a)) }}

        steps (assoc (array-map)
                ;; if we specify the action parameter as a step-id the workflow will use result of the step passed as param
                ::0 [:consumer ::1]
                ::1 [:producer nil])

        ;; note that order of steps is unimportant

        context (wf/make-context context-map)

        ; from context (for now mutable) and steps - we create an executor
        executor (wf/build-executor context steps)]

    (let [v @(wf/sync-execute! executor)]
      (is (= (wdata/extract-results v [::0]) {::0 "Hello world!"})))
    )

  )




(deftest expand-pipeline

  (let [context-map {
                      :hello {:fn (fn [s]
                                    (str "Hello " s "!"))}

                      ;; expand actions return new actions that will be added into workflow
                      :expand {:fn (fn [vs]
                                     (into (array-map)
                                           (map-indexed
                                             (fn[i a]
                                               [(test-data/gen-ns-id (str "hello-" i)) [:hello a]])
                                             vs)))
                               :expands? true
                               }
                      }
        ; workflow is described by a finite number of steps, each of calls to an action
        steps (assoc (array-map)
                ::0 [:expand ["world" "universe"]])

        context (wf/make-context context-map)

        ; from context (for now mutable) and steps - we create an executor
        executor (wf/build-executor context steps)]

    (let [v @(wf/sync-execute! executor 2000)
          ;; move added results into resulting map
          results (wdata/inline-results v)]

      (is (= results {::0 '("Hello world!" "Hello universe!")}))

      ))
  )




(deftest simple-expand-test

  ;; test that expand step produces new steps, both sync and async

  (let [expand-steps {::1 [:id 1]
                      ::2 [:id 2]
                      ::3 [:id 3]
                      }

        async-expand-steps {
                             ::a1 [:id 1]
                             ::a2 [:id 2]
                             ::a3 [:id 3]
                             }
        context-map {
                      :id {:fn identity}
                      :expand* {:fn (fn [xs] expand-steps)
                                :expands? true}

                      :async-expand* {:fn (fn[x]
                                            (let [chan (async/chan)]
                                              (go
                                                (async/put! chan async-expand-steps))
                                              chan))
                                      :expands? true
                                      }
                      }
        context (wf/make-context context-map)]

    (let [steps {
                  ::e* [:expand* ""]
                  ::ae* [:async-expand* ""]
                  }

          result @(wf/sync-execute! (wf/build-executor context steps))]

      ;; ensure added steps had been executed
      (is (= (::1 result) 1))
      (is (= (::2 result) 2))
      (is (= (::3 result) 3))

      (is (= (::a1 result) 1))
      (is (= (::a2 result) 2))
      (is (= (::a3 result) 3))

      ;; ensure the the added sid list is returned
      (is (= (::e* result) '(::1 ::2 ::3)))
      (is (= (::ae* result) '(::a1 ::a2 ::a3)))

      ;; ensure the intermediary results are ommitted if the results are inlined
      (is (= {
               ::e* '(1 2 3)
               ::ae* '(1 2 3)} (wdata/inline-results result)))
      ;result
      ))
  )



;; runs the expanding workflow

(deftest map-reduce-test

  (let [context-map {
          :expand {:fn (fn [xs]
                         (into (array-map)
                               (map-indexed (fn [i x]
                                              [(keyword (str *ns* "/" i)) [:map x]]) xs)))
                   :expands? true
                   }

          :collect {
                     :fn (fn[xs] xs)
                     :collect? true
                     }

          :map {:fn (fn [x] x)}

          :async-map {:fn (fn[x]
                            (let [chan (async/chan)]
                              (go
                               (async/<! (u/timeout 500))
                               (async/put! chan x))
                              chan))}
        }
        context (wf/make-context context-map)]

    ;; collect step functions gather the results the sids provided to them
    (let [steps {
                  ::map-reduce [:collect [::1 ::2]]

                  ::1 [:map 1]
                  ::2 [:map 2]

                  ::async-map-reduce [:collect [::a1 ::a2 ::1 ::2]]

                  ::a1 [:async-map 1]
                  ::a2 [:async-map 2]


                  ::map-reduce-vals [:collect [1 2]]
                  }

          result @(wf/sync-execute! (wf/build-executor context steps))]
        (is (= (::map-reduce result) '(1 2)))

        (is (= (::map-reduce-vals result) '(1 2)))

        (is (= (::async-map-reduce result) '(1 2 1 2)))
      )

    ;; also results can be collected after workflow had been executed
    ;;
    (let [steps {
            ::map-reduce-after-wf [:expand [1 2 3 4]]
          }
          result @(wf/sync-execute! (wf/build-executor context steps))]
      (is (= (wdata/inline-results result) {::map-reduce-after-wf '(1 2 3 4)})))

    )


  )






;; todo: multi-expand test, like f(g(z(x)))


(deftest expand-wf-test
  (let [{
          context-map :context
                 steps :steps } (test-data/gen-expand-wf [:a :b :c])

        context (wf/make-context context-map)
        executor (wf/build-executor context steps)]

    (let [v @(wf/sync-execute! executor 2000)]
      ;(println (d/pretty v))

      (let [async-xpanded (get v :woof.test-data/async-xpand)
            sync-xpanded (get v :woof.test-data/sync-xpand)

            nested-sync (get v :woof.test-data/sync-nested-xpand)
            nested-async (get v :woof.test-data/async-nested-xpand)]


        ;; (println (d/pretty v))

        ; test if expand worked
        (is (= #{":a" ":b" ":c"} (into #{} (vals (select-keys v sync-xpanded)))))
        (is (= #{":a" ":b" ":c"} (into #{} (vals (select-keys v async-xpanded)))))

        ; test if sync nesting works
        (is (= #{":a" ":b" ":c"} (into #{} (vals (select-keys v nested-sync)))))
        (is (= #{":a" ":b" ":c"} (into #{} (vals (select-keys v nested-async)))))

        ; TODO: test async nesting

        )
      )
    )
  )



(deftest error-handling

  ;; in case of something workflow will throw an exception
  (let [timeout-steps (assoc (array-map)
                        ::0 [:timeout200 "World!"])

        no-such-step (assoc (array-map)
                        ::0 [:no-such-step ""])


        context (wf/make-context SAMPLE-CONTEXT)
        xctor (partial wf/build-executor context)]


    (with-out-str
      (with-open [out-writer (clojure.java.io/writer System/err)]
        ;; handle timeout
        (is (thrown? Exception @(wf/sync-execute! (xctor timeout-steps) 10)))

        ;; todo: handle no such step
        (is (thrown? Exception @(wf/sync-execute! (xctor no-such-step) 10)))
        )))
    )





(defn- async-wf [test-context-map test-steps result-fn]

  (let [context (wf/make-context test-context-map)
        executor (wf/build-executor context test-steps)

        *result (atom nil)
        *done (atom nil)

        c (async/chan)

        start-wf-with-transducers! (fn [executor]
                                     (let [exec-chann-0 (wf/execute! executor)
                                           exec-chann (async/pipe
                                                        exec-chann-0
                                                        (async/chan 1 (wf/time-update-xf 300)))]
                                       ;; (async/chan 1 (wf/chunk-update-xf 20))
                                       exec-chann))

        before-processing! (fn [exec-chann executor]
                             (go
                               (async/<! (u/timeout 3500)) ;; ?????
                               ;;(println "timeout" (d/pretty @*result))
                               (if-not (nil? @*done)
                                 (async/put! exec-chann [:error :timeout])))
                             :ok
                             )

        after-processing! (fn [exec-chann]
                            (let [v (async/<!! c)]
                              (result-fn v @*result)))


        op-handlers-map {
                          :done (fn [data]
                                 (reset! *result data)
                                 (reset! *done true)
                                 (go
                                   (async/>! c :ok)))
                          :error (fn [data]
                                  (println "ERROR" [:error data])
                                  ; (async/close! exec-chann)
                                  (go
                                    (async/>! c :timeout)))
                          }

        processor (wf/->ResultProcessor executor
                                     { :execute start-wf-with-transducers!
                                       :before-process before-processing!
                                       ;; :process-handler op-handler
                                       :op-handlers-map op-handlers-map
                                       :after-process after-processing!

                                       :timeout 3500 ;; remove later
                                       })]

        (wf/process-results! processor)
    )

   )



(deftest executor-test
  ;; hand written

  ;; TODO: play with backpressure
  (let [test-context {
                       :hello {:fn (fn [s]
                                     (str "Hello " s "!")
                                     )}
                       :expand {:fn (fn [vs]
                                      (into (array-map)
                                            (map-indexed (fn[i a] [(test-data/gen-ns-id (str "hello-" i))
                                                                   [:hello a]])
                                                         vs)))
                                :expands? true}

                       :wait {:fn (fn [s]
                                    ; (println "wait" s )
                                    (let [c (async/chan)
                                          t (int (rand 1500))]
                                      (go
                                        (async/<! (u/timeout t))
                                        ; (println s "DONE!")
                                        (async/put! c s))
                                      c)
                                    )}
                       }
        ; workflow is described by a finite number of steps, each of calls to an action
        test-steps (assoc (array-map)
                     ::0 [:expand ["world" "universe"]]
                     ::1 [:wait "1"]
                     ::2 [:wait "2"]
                     ::3 [:wait ::2]

                     )


        ]
    (async-wf test-context test-steps
              (fn [v result]
                ;(println v)
                (is (= :ok v))
                ;; todo: add assertion
                ;; (println result)
                ))
    )

  )





(defn wf-test-data [N]
  (let [{
          test-context :context
          test-steps :steps
          } (test-data/get-test-steps-and-context N)]

    (async-wf test-context test-steps
              (fn [v result]

                (is (= (into (sorted-set)
                                    (filter number? (vals   result)))
                       (into (sorted-set) (range N))))
                )
              )))



(deftest test-data-test
  ;; TODO: test profiling
  ;(prof/start {})
  (dotimes [i 1]
    (wf-test-data 80) ;; why more is failing?
    ;(println "Yo!")

    )
  ;(prof/stop {})
  )



(deftest cycle-detection-test

  (let [context-map {:f {:fn (fn [s] (str "Hello " s "!")) }}
        steps (assoc (array-map)
                ::0 [:f ::1]
                ::1 [:f ::2]
                ::2 [:wait ::0]
                )
        context (wf/make-context context-map)]
    (let [c (async/chan)
          executor (wf/build-executor context steps)]

      ;; TODO: move to a result processor

      (let [exec-chann (wf/execute! executor)]
        (go-loop [] ; can handle state via loop bindings
                 (let [r (async/<! exec-chann)
                       [status data] r]

                   ;; todo: can these be done via transducer?
                   (condp = status
                     :init (recur)
                     :error (do
                              ;; (println "ERROR" r)
                              (async/close! exec-chann)
                              (async/>! c :error)
                              )
                     :process (do
                                (recur))
                     :done (do
                             (async/>! c :ok))

                     (do ; other events like :wf-update
                       (recur)))))
        )




      (let [v (async/<!! c)]
        (is (= v :error))
        )
      )

    ))



;; (run-tests)


;;
;; multi-arity step handler

#_(let [*state (atom {})
       test-context
      {
        :state! {:fn (fn [v]
                       (swap! *state assoc :test v)
                       (str "Hello " v "!")
                      )}

        :v {:fn (fn[v]
                  v
                  )}

        :fn-2 {:fn (fn [[a b]]
                     (str "(fn " a " " b ")")
                     )
               :collect? true
               }

        ;; how to do steps sequentially without wrapping them to an intermediary step


        }
        ; workflow is described by a finite number of steps, each of calls to an action
        test-steps {
                     ::arg-1  [:state! "BAR"]
                     ::arg-2  [:v "param-pum-pum"]

                     ::arity-test [:fn-2 [::arg-1 ::arg-2]]

                     ;;::first [:state! "FOO"]
                     }


        ]
    (async-wf test-context test-steps
              (fn [v result]
                (println v result)

                (println "STATE" @*state)

                ;; todo: add assertion
                ;; (println result)
                ))
    )




#_(let [*state (atom {})
       test-context
      {
        :state! {:fn (fn [[k v]]
                       (swap! *state assoc k v)
                       (str "Hello " v "!")
                      )}

        :state {:fn (fn[v]
                      (get @*state v)
                      )}

        :v {:fn (fn[v]
                  v
                  )}

        :c {:fn (fn[v]
                  v
                  )
            :collect? true
            }


        :wait-last {:fn (fn[v]
                          (last v))
                    :collect? true
                    :expands? true
                    }

        :fn-2 {:fn (fn [[a b]]
                     ;(str "(fn " a " " b ")")
                     [a b]
                     )
               :collect? true
               }




        :e-fn {:fn (fn[[a b]]
                  (println [a b])

                  (assoc (array-map)
                    ;; should wait for ::a ::a

                    ::a [:state! [:a a]]
                    ::b [:state! [:b b]]


                    )
                  )
            :collect? true
            :expands? true
            }



        ;; how to do steps sequentially without wrapping them to an intermediary step


        }
        ; workflow is described by a finite number of steps, each of calls to an action
        test-steps {


                     ::arg-1  [:state! [:test "BAR"]]
                     ::arg-2  [:v "param-pum-pum"]

                     ;; ::v [:fn-2 [::arg-1 ::arg-2]]


                     ;; multi-arity step handler
                     ::expand-wait [:e-fn [::arg-1 ::arg-2]]


                     ;; collect keys/values
                     ::expand-k [:v ::expand-wait]
                     ::expand-v [:c ::expand-wait]


                     ;; waiting for step

                     ;; add a meta handler with steps to be executed after data are ready
                     ::step [:v {::at-last [:state :a]}]

                     ;; wait for all values, but treat last as steps
                     ::wait [:wait-last [::expand-v ::expand-k ::step]]
                     ;; have the result
                     ::wait-result [:c ::wait]

                     ;; ::v [:v ::www]


                     ;;::first [:state! "FOO"]
                     }


        ]
    (async-wf test-context test-steps
              (fn [v result]
                (println v (d/pretty result))

                (println "STATE" @*state)

                ;; todo: add assertion
                ;; (println result)
                ))
    )




;; multi-expand

(let [test-context
      {
        :e1 {:fn (fn[[a b]]
                  (assoc (array-map)
                    ;; should wait for ::a ::a

                    ::a1 [:e2 [:a a]]
                    ::b1 [:e2 [:b b]]
                    )
                  )
            :expands? true
            }

        :e2 {:fn (fn[a]
                  (assoc (array-map)
                    ;; should wait for ::a ::a
                    (u/seq-sid "e2-") [:e3 (str "--" a)]

                    )
                  )
            :expands? true
            }


        :e3 {:fn (fn[a]
                  (assoc (array-map)
                    ;; should wait for ::a ::a
                    (u/seq-sid "e3-") [:v (str "--" a)]

                    )
                  )
            :expands? true
            }



        :v {:fn (fn[v]
                  v
                  )}



        :c {:fn (fn[v]

                  (if (every? wf/sid-list? v)
                    (let [s (u/seq-sid "c-")]
                      {
                        s [:c (apply concat v)]
                        ;;(wf/rand-sid) [:c s]
                        })
                    v
                    ; { (u/seq-sid "result-") [:v v] }
                  )

                )
            :expands? true
            :collect? true
            }


        :c1 {:fn (fn[v]
                  v)
             :collect? true
             }



        ;; how to do steps sequentially without wrapping them to an intermediary step


        }
        ; workflow is described by a finite number of steps, each of calls to an action
        test-steps {

                     ;; generate multi expand
                     ::e1 [:e1 ["1" "2"]]

                     ;; test injecting the result as step - callback?
                     ::c  [:c [::e1]]

                     ;; ::c1 [:c1 ::c]

                     }


        ]


  (async-wf test-context test-steps
            (fn [v result]
              (println v (d/pretty result))

              ))

  )





; (every? wf/sid-list? [[::a] [::b]])



;;(every? wf/sid-list? [:a])
