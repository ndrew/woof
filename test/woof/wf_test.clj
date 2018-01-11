(ns woof.wf-test
  (:require
    [clojure.core.async :as async :refer [go go-loop]]
    [clojure.test :refer :all]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.utils :as u]
    [woof.test-data :as test-data]
    [woof.graph :as g]

    [criterium.core :as criterium]


))

;; TODO: test async expaning

(defn- handle-result [wait-chan result-chan]
  "shorthand for processing workflow results from result-chan and pass the final result via wait-chan"
  (go-loop []
           (let [r (async/<! result-chan)
                 [status data] r]
             (condp = status
               :error (async/>! wait-chan :error)
               :done (async/>! wait-chan data)
               (do ; skip :init :process and other steps
                 (recur)))))

  ;; todo: add a timeout to close channel
  wait-chan
)



(deftest simplest-pipeline
  ;; runs the simple workflow
  (let [context { ; context holds info about actions workflow can execute
                  :hello {:fn (fn [a]
                                  "Hello!"
                                )}}
        ; workflow is described by a finite number of steps, each of calls to an action
        steps (assoc (array-map)
                ::0 [:hello {}])
        executor (wf/executor (atom context) steps)]

    (let [exec-chann (wf/execute! executor)]
      (let [v (async/<!! (handle-result (async/chan) exec-chann))]
        (is (= {::0 "Hello!"}))
        ;; we can use exec/extract-results to get only step result we need
        (is (= (wf/extract-results v [::0]) {::0 "Hello!"}))))))



(deftest nesting-results-pipeline
  ;; test nesting steps
  (let [context { :hello {:fn (fn [a]
                                  (if (nil? a)
                                    "Hello!"
                                    a)
                                )}}

        steps (assoc (array-map)
                ;; if we specify the action parameter as a step-id the workflow will use result of the step passed as param
                ::0 [:hello ::1]
                ::1 [:hello nil]
                )
        executor (wf/executor (atom context) steps)]

    (let [exec-chann (wf/execute! executor)]
      (let [v (async/<!! (handle-result (async/chan) exec-chann))]
        (is (= {::1 "Hello!"}))
        ;; we can use exec/extract-results to get only step result we need
        (is (= (wf/extract-results v [::0]) {::0 "Hello!"}))))
    ))



(deftest expand-pipeline
  (let [context { ; context holds info about actions workflow can execute
                  :hello {:fn (fn [s]
                                  (str "Hello " s "!")
                                )}
                  :expand {:fn (fn [vs]
                                 (into (array-map)
                                    (map-indexed (fn[i a] [(test-data/gen-ns-id (str "hello-" i))
                                                           [:hello a]])
                                         vs)))
                           :expands? true}
                  }
        ; workflow is described by a finite number of steps, each of calls to an action
        steps (assoc (array-map)
                ::0 [:expand ["world" "universe"]])
        executor (wf/executor (atom context) steps)]

    (let [exec-chann (wf/execute! executor)]
      (let [v (async/<!! (handle-result (async/chan) exec-chann))]
        (let [tk1 (test-data/gen-ns-id "hello-0")
              tk2 (test-data/gen-ns-id "hello-1")
              data (wf/extract-results v [tk1 tk2])]

          data
           ;(is (= data {tk1 "Hello world!" tk2 "Hello universe!"}))
           ;(= (::0 v) '(tk1 tk2))
          )))))



(deftest expand-wf-test
  (let [{context :context
         steps   :steps} (test-data/gen-expand-wf [:a :b :c])
        executor (wf/executor (atom context) steps)
        ]
    (let [exec-chann (wf/execute! executor)]
      (let [v (async/<!! (handle-result (async/chan) exec-chann))]
        ;(println (d/pretty v))

        (let [async-xpanded (get v :woof.test-data/async-xpand)
              sync-xpanded (get v :woof.test-data/sync-xpand)

              nested-sync (get v :woof.test-data/sync-nested-xpand)
              nested-async (get v :woof.test-data/async-nested-xpand)
              ]
          ; test if expand worked
          (is (= #{":a" ":b" ":c"} (into #{} (vals (select-keys v sync-xpanded)))))
          (is (= #{":a" ":b" ":c"} (into #{} (vals (select-keys v async-xpanded)))))

          ; test if sync nesting works
          (is (= #{":a" ":b" ":c"} (into #{} (vals (select-keys v nested-sync)))))
          (is (= #{":a" ":b" ":c"} (into #{} (vals (select-keys v nested-async)))))

          ; TODO: test async nesting

          )


        ))

    )
)




(defn- async-wf [test-context test-steps]
  (println "async-wf!")

  (let [c (async/chan)
        ;executor (wf/cached-executor (atom test-context) test-steps)
        executor (wf/executor (atom test-context) test-steps)
        ]

    (let [*result (atom nil)
          *done (atom nil)
          ;exec-chann (wf/execute! executor)
          ;; wrap
          exec-chann-0 (wf/execute! executor)
          ;z (async/chan 1 (wf/chunk-update-xf 20))
          z (async/chan 1 (wf/time-update-xf 300))
          exec-chann (async/pipe exec-chann-0 z)

          ]

       (go
         (async/<! (u/timeout 3500))
         ;;(println "timeout" (d/pretty @*result))
         (if-not (nil? @*done)
           (async/put! exec-chann [:error :timeout])))

       (go
        (loop [] ; can handle state via loop bindings
          (let [r (async/<! exec-chann)
                [status data] r]

          ;; todo: can these be done via transducer?
            (condp = status
              :init (recur)
              :error (do
                        (println "ERROR" r)
                        (async/close! exec-chann)
                        (async/>! c :timeout)
                       )
              :process (do
                         ;(println "PROCESS:")
                         ;(println (d/pretty data))
                         (recur))
              :done (do
                      (reset! *result data)
                      (reset! *done true)
                      (async/>! c :ok))

              (do ; other events like :wf-update

                (recur))))))

      (let [v (async/<!! c)
            results (into (sorted-set)
                       (filter number? (vals @*result)))]

        ;; TODO: add assertion here
        (println "TEST RESULT\n" results)
        (println "\n\n\n *** total steps " (count @*result) (d/pretty (keys @*result)) "***\n\n\n")
          )
      )
    )
  )


(defn executor-test []

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
                             (println "wait" s )
                             (let [c (async/chan)
                                       t (int (rand 1500))]
                                   (go
                                     (async/<! (u/timeout t))
                                     (println s "DONE!")
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
    (async-wf test-context test-steps)
    )

)



(defn wf-test-data []
  (let [N 20;;120 - fails
        {
          test-context :context
           test-steps :steps
        } (test-data/get-test-steps-and-context N)]

    (async-wf test-context test-steps)))



(deftest test-data-test
  ;; TODO: test profiling
  ;(prof/start {})
  (dotimes [i 2]
    (wf-test-data)
    ;(println "Yo!")

    )
  ;(prof/stop {})
  )


(deftest cycle-detection-test

  (let [context {:f {:fn (fn [s] (str "Hello " s "!")) }}
        steps (assoc (array-map)
                ::0 [:f ::1]
                ::1 [:f ::2]
                ::2 [:wait ::0]
                )]
    (let [c (async/chan)
          executor (wf/executor (atom context) steps)]


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




(deftest infite-actions-test

  (let [context (atom {:f {:fn (fn [s]
                                 ; (str "Hello " s "!")
                                 (let [chan (async/chan)]

                                   (go
                                     (async/<! (u/timeout 1000))
                                     (async/>! chan "1")
                                     (println "1")

                                     (async/<! (u/timeout 3000))
                                     (async/>! chan "2")

                                     (println "2")
                                     )

                                   chan))
                           :infinite true
                           }

                       ;;:infinite true
                       })
        steps (assoc (array-map) ::0 [:f "hello"])

        ;; result-chan
        process-chan (async/chan)

        MODEL (wf/make-state! context (wf/make-state-cfg steps process-chan))]


    (let [c (async/chan)
          executor
          (wf/executor context
                       MODEL
                       (async/chan)
                       process-chan)]


      (let [exec-chann (wf/execute! executor)]

        (go
          (async/<! (u/timeout 5000))
          (wf/end! executor))

        (go-loop [] ; can handle state via loop bindings
                 (let [r (async/<! exec-chann)
                       [status data] r]

                   ;; todo: can these be done via transducer?
                   (condp = status
                     :init (recur)
                     :error (do
                              ;; (println "ERROR" r)
                              (async/close! exec-chann)
                              (async/>! c [:error data] )
                              )
                     :process (do
                                (recur))
                     :done (do
                             (async/>! c [:done data]))

                     (do ; other events like :wf-update
                       (recur))))))




      (let [[status data] (async/<!! c)]
        (is (= :done status))))))





(deftest infite-actions-update-test

  (let [context (atom {:f {:fn (fn [s]
                                 ; (str "Hello " s "!")
                                 (let [chan (async/chan)]

                                   (go
                                     (async/<! (u/timeout (int (rand 1000))))
                                     (async/>! chan (str "1__" s))
                                     ; (println "1" s )

                                     (async/<! (u/timeout (int (rand 3000))))
                                     (async/>! chan (str "2__" s))

                                     ; (println "2" s)
                                     )

                                   chan))
                           :infinite true
                           }


                         :f1 {:fn (fn [s]
                                 (println "Hello " s "!")

                                    s
                                    )
                           :infinite true
                           }

                       })
        steps (assoc (array-map)
                ::0 [:f "_1"]
                ::1 [:f1 ::0]
                ::2 [:f "_2"]
                )

        ;; result-chan
        process-chan (async/chan)

        MODEL (wf/make-state! context (wf/make-state-cfg steps process-chan))]

    (let [c (async/chan)
          executor
          (wf/executor context
                       MODEL
                       (async/chan)
                       process-chan)]


      (let [exec-chann (wf/execute! executor)]

        (go
          (async/<! (u/timeout 7000))
          (wf/end! executor))

        (go-loop [] ; can handle state via loop bindings
                 (let [r (async/<! exec-chann)
                       [status data] r]

                   ;; todo: can these be done via transducer?
                   (condp = status
                     :init (recur)
                     :error (do
                              ;; (println "ERROR" r)
                              (async/close! exec-chann)
                              (async/>! c [:error data] )
                              )
                     :process (do
                                ;(println (d/pretty r))

                                (recur))
                     :done (do
                             (async/>! c [:done data]))

                     (do ; other events like :wf-update
                       (recur))))))




      (let [[status data] (async/<!! c)]
        (println [status data])
        (is (= :done status))

        )))
)




