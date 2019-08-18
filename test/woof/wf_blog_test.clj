(ns woof.wf-blog-test
  (:require
    [clojure.core.async :as async :refer [go go-loop]]
    [clojure.test :refer :all]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.wf-data :as wdata]

    [woof.utils :as u]
    [woof.test-data :as test-data]
   ; [woof.graph :as g]

    ;[compact-uuids.core :as uuid]

    ; [me.raynes.fs :as fs]
    ; [markdown.core :as md]

    ; [hawk.core :as hawk]
    )
  (:import [java.io File ]))


;; take with timeout


#_(let [c (async/chan)]
  (go ; produce

    (async/<! (async/timeout 900))
    (async/>! c :hello)

    )

  (let [[v _] (async/alts!! [c (async/timeout 1000)] :priority true)]
    (if (nil? v)
      (println "timeout!")
      (println v)
      )

    )

)




;; start several go-loops (producers)
;; start single consumer


#_(let [consume-chan (async/chan)
      end-chan (async/chan)]

  ;; consume-chan
  (go

      (loop [n 0]
              (when-let [x (async/<! consume-chan)]
                  (println x)
                  (recur (inc n))))

      (println "done:" "iterations" )
      )

  ;; producers

  (go

    (loop []
      (async/<! (u/timeout 50))
      (async/put! consume-chan (System/currentTimeMillis))

      (recur))


    (println "done:" "PRODUCER 1: ")
  )

  (go

    (loop []
      (async/<! (u/timeout 33))
      (async/put! consume-chan (System/currentTimeMillis))

      (recur))


    (println "done:" "PRODUCER 2: ")
  )



  (go ;; close channels

      (async/<! (u/timeout 4000))
      (async/close! consume-chan)
      )

  )









(defn debug [x] (pr-str x))


(defn run-wf
  "workflow runner function"
  [context in-fn]

  (let [done> (async/chan)
        in-chan> (async/chan)
        steps {
                ::IN [:in in-chan>]
                ::OUT [:out ::IN]
                ;;
                }

        context (wf/make-context context)
        executor (wf/build-executor context steps)

        *result (atom {})

        before-processing! (fn[exec-chan executor]
                             (-> (Thread. (fn[]
                                            (let [d (async/<!! done>)]
                                              (let [r @*result]

                                                (locking *out*
                                                  (println "D O N E:\n\nIN:\n"
                                                           (debug (::IN r))
                                                           "OUT:\n" (debug (::OUT r))

                                                           "other keys " (debug (filter #(not (#{::IN ::OUT} %)) (keys r)))

                                                           "\n\nRESULTS:\n\n" (debug (wdata/inline-results r))
                                                           ))

                                                )
                                              )
                                            )) .start)
                             :ok
                             )


        processor (wf/->ResultProcessor executor
                                        { ;:execute start-wf-with-transducers!
                                          :before-process before-processing!
                                          ;; :process-handler op-handler
                                          :op-handlers-map {
                                                             :process (fn[data]
                                                                        (swap! *result merge data)
                                                                        )
                                                             :done #(async/put! done> %)
                                                             :error #(async/put! done> %)

                                                             }
                                          :timeout 1000
                                          })
        ]


    ;; start processing loop
    (wf/process-results! processor)

    ;; put data into ::IN
    (in-fn in-chan>)))




(def base-context {:debug
                   (wf/step-handler (fn[x]
                              (println "\tDBG: " (d/pretty x))
                              x))})


;; infinite expands steps produces new values, each  linked to :out

;; infinite scenarios

;; <?> can normal step handlers be replaced via infinite ones?

;; inf :IN, inf :OUT
;; a -> :IN -> :OUT
;; b -> :IN -> :OUT
;;
;; expected result {:IN a :OUT a} => {:IN b :OUT b}



;; todo: why this


#_(let [input-ch (async/chan)
      output-ch (async/chan)]


  #_(go-loop [n 0]
        (when-let [x (<! input-ch)]
                  (>! output-ch x)
                  (recur (inc n))
        n))

  )



(run-wf
  (merge base-context
         { :in { :fn (fn[in>]
                       (let [chan> (async/chan)]
                         (go-loop []
                                  (let [v (async/<! in>)]
                                    (async/put! chan> v))
                                  (recur))
                         chan>))
                 :infinite true}
           :out {
                  :fn (fn[xs]
                        ;; todo: why these are called many times
                        (locking *out* (println "--> OUT(" (rand-int 100) ")\t" xs))
                        xs)
                  :infinite true}
           })

  (fn[in-chan>]
    (go
      (async/put! in-chan> "1")
;;      (async/<! (u/timeout 150))
      (async/put! in-chan> "2")
;;            (async/<! (u/timeout 1))
      (async/put! in-chan> "3")
      (async/put! in-chan> "4")
      (async/put! in-chan> "5")
      (async/put! in-chan> "6")
      (async/put! in-chan> "7")
      (async/put! in-chan> "8")
      (async/put! in-chan> "9")
      (async/put! in-chan> "10")
      ;(async/<! (u/timeout 100))
      ;(async/put! in-chan> "3")
      )
    ))





;; inf/exp :IN, inf/collect :OUT
;; [:id a] -> :IN -> :OUT
;; [:id b] -> :IN -> :OUT

;; expected result {:IN (:a) :a a :OUT (a)}
;;              => {:IN (:a, :b) :a a :b b  :OUT (a b)}




#_(run-wf
    (merge base-context
         { :in { :infinite true :expands? true

                 :fn (fn[in>]

                       (let [chan> (async/chan)]
                         (go-loop []
                                  (let [[sid v] (async/<! in>)
                                        ;; sid (wf/rand-sid)
                                        ]

                                    ; put the expanded value
                                    (async/put! chan> (with-meta
                                                 {sid v}
                                                        {:expand-key sid}))


                                    )
                                  (recur))
                         chan>))
                 }

           :out { :infinite true :collect? true

                  :fn (fn[xs]
                        (locking *out* (println "--> OUT \t" xs))
                        xs)
                  }
           })


        (fn[in-chan>]
          (go
            (async/put! in-chan> [::a1 [:debug "1"]])
;            (async/<! (u/timeout 0))
            (async/put! in-chan> [::a2 [:debug "2"]])
            (async/put! in-chan> [::a3 [:debug "3"]])
;            (async/<! (u/timeout 100))
;            (async/put! in-chan> "3")
            )
          ))









