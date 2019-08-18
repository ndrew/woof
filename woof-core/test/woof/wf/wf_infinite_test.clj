(ns woof.wf.wf-infinite-test
  (:require
    [clojure.core.async :as async :refer [go go-loop]]
    [clojure.test :refer :all]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.core.protocols :as proto]


    [woof.utils :as u]
    [woof.test-data :as test-data]
    [woof.graph :as g]

    ))


(defn run-infinite-wf
  "runs workflow for t milliseconds, then sends results to done-channel."
  [t context-map steps done-channel]

  (let [context (wf/make-context context-map)
        xtor (wf/build-executor context steps)
        c (proto/execute! xtor)]

      (go-loop []
        (let [[status data] (async/<! c)]
          (when (= :done status)
            (async/put! done-channel data))

          (if-not (= :done status)
            (recur))
        ))

      (go
        (async/<! (u/timeout t))
        (proto/end! xtor))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; test for 'infinite' workflows
;;



;; how to implement locking



;
; test infinite expand:
;
;   * expand works multiple times
;   * expanded steps are executed
;     * expanded steps are executed once
;
(deftest infinite-expand-test

  (let [done-channel (async/chan)

        cnt (atom [])
        start-t (System/currentTimeMillis)


        context-map {
          :id {:fn (fn [x]
                     (swap! cnt conj x) ;; track number of step-fn runs

                     ;; (println x)

                     x)}

          :ui-loop {:fn (fn[x]
                            (let [chan (async/chan)]
                              (go
                                ;; test sending with pauses
                               (async/<! (u/timeout 10))
                               (async/>! chan { (u/sid "_" "1") [:id [1 (- (System/currentTimeMillis) start-t)]] })
                               ;(async/<! (u/timeout 70))
                               ;; try sending simultaneously
                               (async/>! chan { (u/sid "_" "2") [:id [2 (- (System/currentTimeMillis) start-t)]] })
                               (async/>! chan { (u/sid "_" "3") [:id [3 (- (System/currentTimeMillis) start-t)]] })
                               (async/>! chan { (u/sid "_" "4") [:id [4 (- (System/currentTimeMillis) start-t)]] })
                               (async/>! chan { (u/sid "_" "5") [:id [5 (- (System/currentTimeMillis) start-t)]] })

                               (async/>! chan { (u/sid "_" "6") [:id [6 (- (System/currentTimeMillis) start-t)]] })
                               (async/>! chan { (u/sid "_" "7") [:id [7 (- (System/currentTimeMillis) start-t)]] })
                               (async/>! chan { (u/sid "_" "8") [:id [8 (- (System/currentTimeMillis) start-t)]] })
                               (async/>! chan { (u/sid "_" "9") [:id [9 (- (System/currentTimeMillis) start-t)]] })


                                )

                              chan))

                      :expands? true
                      :infinite true
                      }
        }
        steps { ::ui [:ui-loop ""] }]


      (run-infinite-wf 300 context-map steps done-channel)

      (let [d (async/<!! done-channel)]
        (println  "WF DONE:\n"

                  (d/pretty d)
                  ;(d/pretty (::ui d))

                  "id called times= " @cnt
                  )
        (println (count @cnt))
        ; (println d)
;; FIXME: add more assertions
        ;; (is (<= (count @cnt) 5)) ;; FIXME: why the values are dropped?
        (is (= (count (::ui d)) 9))
        ;;

        ))

)



(deftest infinite-expand-collect-test
  ;;
  ;; test if values from infinite collect are passed to a linked step

  (let [done-channel (async/chan)

        collect-history (volatile! [])
        n-history (volatile! [])


        context-map {
                      ;; this is fired twice

          :n {:fn (fn [x]
                     ;; to test that these are executed once
                    (vswap! n-history conj x)
                     x)}

          :id {:fn (fn [x]
                     ;; to test that these are executed once
                    (println "ID: " x)
                     x)}

          :ui-loop {:fn (fn[x]
                            (let [chan (async/chan)]
                              (go
                               (async/<! (u/timeout 10))
                               (async/>! chan { (wf/rand-sid) [:id :1] })

                               (async/<! (u/timeout 70))
                               (async/>! chan { (wf/rand-sid) [:id :2] }))

                              chan))

                      :expands? true
                      :infinite true
                      }

            :collect {:fn (fn [x]
                            ; (println "collect: " x )
                            (vswap! collect-history conj x)
                            x)
                      :infinite true :collect? true
                      }
        }
        steps {
                  ::linked-step [:n ::c]
                  ::ui [:ui-loop ""]
                  ::c  [:collect ::ui]


                  }

        ]


      (run-infinite-wf 300 context-map steps done-channel)


      (let [d (async/<!! done-channel)]
        ;; (println  "WF DONE:" (d/pretty d))

        ; check if collect is executed twice
        (is (= @collect-history ['(:1) '(:1 :2)]))
        (is (= @collect-history @n-history))

        (is (= (::c d) '(:1 :2) ))
        (is (= (::c d) (::linked-step d) ))

        )))




(deftest infite-actions-test

  (let [context-map {:f {:fn (fn [s]
                                 (let [chan (async/chan)]
                                   (go
                                     (async/<! (u/timeout 100))
                                     (async/>! chan "1")

                                     (async/<! (u/timeout 150))
                                     (async/>! chan "2"))
                                   chan))
                           :infinite true
                           }
                     }

        steps (assoc (array-map) ::0 [:f "hello"])

        ;; result-chan
        process-chan (async/chan)
        context (wf/make-context context-map {:process-channel process-chan})]


    (let [c (async/chan)
          executor (wf/build-executor context steps)]


      (let [exec-chann (proto/execute! executor)]

        (go
          (async/<! (u/timeout 5000))
          (proto/end! executor))

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

  (let [context-map {:f {:fn (fn [s]
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
                                  ; (println "Hello " s "!")

                                  s
                                  )
                            :infinite true
                            }

                       }
        steps (assoc (array-map)
                ::0 [:f "_1"]
                ::1 [:f1 ::0]
                ::2 [:f "_2"]
                )

        ;; result-chan
        process-chan (async/chan)

        context (wf/make-context context-map {:process-channel process-chan})]

    (let [c (async/chan)
          executor (wf/build-executor context steps)]


      (let [exec-chann (proto/execute! executor)]

        ;; todo: use result procesor

        (go
          (async/<! (u/timeout 7000))
          (proto/end! executor))

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
        ;; todo: add assertion here
        ;(println [status data])

        (is (= :done status))

        )))
  )





; (run-tests)

