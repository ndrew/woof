(ns woof.wf-test
  (:require
    [clojure.core.async :as async :refer [go go-loop]]
    [clojure.test :refer :all]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.utils :as u]
    [woof.test-data :as test-data]
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
           (is (= data {tk1 "Hello world!" tk2 "Hello universe!"}))
           (= (::0 v) '(tk1 tk2))
          )))))


(defn chunk-update-xf [buf-size]
  (fn [rf]
    (let [ctr (volatile! 0)]
      (fn
        ([] (rf)) ; init (arity 0) - should call the init arity on the nested transform xf, which will eventually call out to the transducing process
        ([result] ; completion (arity 1)
         (println "COMPLETE" result)
         (rf result))
        ; Step (arity 2) - this is a standard reduction function but it is expected to call the xf step arity 0 or more times as appropriate in the transducer.
        ; For example, filter will choose (based on the predicate) whether to call xf or not. map will always call it exactly once. cat may call it many times depending on the inputs.
        ([result v]                         ; we ignore the input as
         (let [[status data] v]
           ;;(println "~~~" @ctr "~" status "~~~")
           (if-not (= :process status)
             (rf result v)
             (do
               (vswap! ctr inc)
               (when (= buf-size @ctr)
                 (vreset! ctr 0)
                 (rf result v)
               ))
             )
           ))))))


(deftest executor-test
  ;; TODO: play with backpressure
  (let [c (async/chan)
        N 90
        {
          test-context :context
           test-steps :steps
        } (test-data/get-test-steps-and-context N)

        executor (wf/cached-executor (atom test-context) test-steps)
        ]

    (let [*result (atom nil)
          exec-chann-0 (wf/execute! executor)

          ;; wrap
          z (async/chan 1 (chunk-update-xf 20))
          exec-chann (async/pipe exec-chann-0 z)]

       (go
        (loop [] ; can handle state via loop bindings
          (let [r (async/<! exec-chann)
                [status data] r]

          ;; todo: can these be done via transducer?
            (condp = status
              :init (recur)
              :error (println "ERROR" r)
              :process (do
                         ;(println "PROCESS:")
                         ;(println (d/pretty data))
                         (recur))
              :done (do
                      (reset! *result data)
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


;; (run-tests)



