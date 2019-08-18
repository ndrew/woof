(ns woof.wf.wf-shandlers-test
  (:require
    [clojure.core.async :as async :refer [go go-loop]]
    [clojure.test :refer :all]

    [woof.core.processors :as p]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.xform :as x]
    [woof.wf-data :as wdata]

    [woof.utils :as u :refer [inline--fn inline--fn1]]
    [woof.test-data :as test-data]

))



;; test for generating step handler function via transducer

(deftest step-handler-from-transducer-local


  (let [*log (atom [])
        dbg (fn [& x]
              (swap! *log concat x)
              ; (locking *out* (apply println x))
              )

        xform (fn
                ([]
                 ;; init will be logged every time shandler runs
                 (dbg "init"))
                ([input]
                 ;; transform value - add !!!
                 (str  input "!!!"))
                ([input output]
                 ;; log initial value and transformed value
                 (dbg (str input " -> " output))
                 output)
                )

        context (wf/make-context
                  {
             ;; define the handler by xform
                    :handler {
                               :fn (x/local-shandler xform)
             ;; note that we generate only step handler fn
             ;; as we can use different types of step handler
             ;; like expand or collect
                               }
                    })

        steps {
                ::0 [:handler "hello"]
                ::1 [:handler "world"]
                }

        executor (wf/build-executor context steps)]

    (let [v @(p/sync-execute! executor)]

      (is (= "hello!!!" (::0 v)))
      (is (= "world!!!" (::1 v)))
      ;; ensure init is being called twice
      (is (= ["init" "hello -> hello!!!"
              "init" "world -> world!!!"
              ] (vec @*log)))
      ))

)



(deftest step-handler-from-transducer-global

  (let [*log (atom [])
        dbg (fn [& x]
              (swap! *log concat x)
              ; (locking *out* (apply println x))
              )

        xform (fn
                ([]
                 ;; init will be logged once per workflow run
                 ;; even if shandler won't be executed
                 (dbg "init"))
                ([input]
                 ;; transform value - add !!!
                 (str  input "!!!"))
                ([input output]
                 ;; log initial value and transformed value
                 (dbg (str input " -> " output))
                 output)
                )

        context (wf/make-context
                  {
             ;; define the handler by xform
                    :handler {
                               :fn (x/global-shandler xform)
             ;; note that we generate only step handler fn
             ;; as we can use different types of step handler
             ;; like expand or collect
                               }
                    })

        steps {
                ::0 [:handler "hello"]
                ::1 [:handler "world"]
                }


        executor (wf/build-executor context steps)]

    ;; execute workflow synchronously (clojure only)
    (let [v @(p/sync-execute! executor)]

      (is (= "hello!!!" (::0 v)))
      (is (= "world!!!" (::1 v)))
      ;; ensure init is being called once
      (is (= ["init"
              "hello -> hello!!!" "world -> world!!!"
              ] (vec @*log)))
      ))

)





;; (deftest rpc-via-xform
  ;; example of using stateful transducers to return data from the workflow
  ;;

  #_(let [*log (atom [])
        dbg (fn [x]
              (swap! *log conj x)
              ;; (locking *out* (println x))
              )

        global-out-c (async/chan 100)

        xform (fn
                ([]
                 ;; init the output loop
                 (go-loop []
                          (when-let [[sid result] (async/<! global-out-c)]
                            ;;
                            ;; (inline--fn1 (partial dbg v) )
                            (dbg {:sid sid
                                  :v result})
                            (recur)
                            )))
                ([v]
                 ;; prepare output for sending to a wire
                 ; (println "v=" (pr-str v))
                 (str (last v) "!!!"))

                ([[sid input] result]
                 ;; send sid and the result to output channel
                 ;; note that we prepared the steps, so there will be a sid
                 ;; it can be useful as we can reference it later
                 (go
                   (async/>! global-out-c [sid result]))

                 ; (dbg [sid result])

                 result))


        base-context {
                      :v   {:fn identity}
                      ;; return sid as a value
                      :&v  {:fn (fn[a] {(wf/rand-sid) [:v a]})
                            :expands? true}
                      ;; return value as sid-list
                      :v*  {:fn identity
                            :collect? true
                            }
                      ;; group
                      :zip-1 {:fn (fn [vs]
                                    (let [[a b] vs]
                                      ;(println (pr-str (concat a b)))
                                      (concat a b)
                                      )

                                    ;(first (partition (count vs) (apply interleave vs)))
                                    )
                              :collect? true
                              }

                     }

        context-map  (merge base-context
                            {

                              :in {:fn (fn[x]
                                         (u/subsitute-with-rand-sids base-context {
                                           ::v   [:v x]
                                           ::v-ref  [:&v ::v]
                                           ::v-val  [:v* ::v-ref]

                                           ::z  [:zip-1 [::v-ref ::v-val]]

                                           ;::out [:out ::z ]
                                           })
                                         )
                                   :expands? true
                                   }
                              :out { :fn (x/global-shandler xform) }

                              })

        steps {
                ::hello [:in "hello"]

                ;;::woof [:in "woof"]
                }


        executor (wf/build-executor (wf/make-context context-map) steps)]

    (let [v @(wf/sync-execute! executor)]
      ;;
      #_(async/thread
        (Thread/sleep 500)

        (let [log @*log]
          (println log)


          (is (= 2 (count log)))

          #_(doseq [{v :v
                   sid :sid
                   } log]
            (is (not (nil? (#{"hello!!!" "woof!!!"} v))))
            (is (u/sid? sid))
            )
          )
        )


      (println (d/pretty v))
      (println (d/pretty (wdata/inline-results v)))

      #_(let [result (wdata/inline-results v)]
        ;(println (d/pretty result))
        (is (= "hello!!!" (last (::hello result))))
        (is (= "woof!!!" (last (::woof result))))
        )

      ))
;;)
