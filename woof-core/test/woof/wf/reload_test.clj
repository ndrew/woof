(ns woof.wf.reload-test
  (:require
    [clojure.core.async :as async :refer [go go-loop]]
    [clojure.test :refer :all]

    [clojure.java.io :as io]
    [clojure.string :as str]

    [woof.base :as base]
    [woof.data :as d]
    [woof.wfs.evt-loop :as evt-loop]
    [woof.utils :as u]
    ))

;; test case:
;; * reloading workflow - stopping previous instance of workflow, waiting for it to finish and starting new workflow instance

;; solution:
;; * base/stateful-wf and base/auto-run-wf!

;; problem:
;; * workflow does not stop instantly, so additional wait mechanisms are needed to ensure that workflow had actually ended

;; note: :after-process handler in opts IS NOT ABOUT ending workflow


;; SOLUTION: base/auto-run-wf!


(defn dbg
  "debug println, with thread name"
  [& args]
  ;; uncomment to see logs of the workflow execution
  #_(locking *out*
    (apply println (concat [(.getName (Thread/currentThread)) "\t"] args)))
  )



;;
;; test workflow
(defn reload-test-wf [*STATE id]
    (dbg "starting workflow " id)

  ;; note: here we use closures to access state, but consider using init accessors

    (let [;; note that chan factory is not used here, as this signal channel should survive after workflow ending
          STOP-CHAN (async/chan 1)

          CHAN-FACTORY (base/chan-factory (atom {}))
          EVT-LOOP (base/make-chan CHAN-FACTORY (base/rand-sid "server-loop"))

          init-fn (fn [params]
                    (dbg "[WF][INIT]") ;; indicate that wf had 'started'
                    ;; provide id of the workflow, so it can be used after workflow is finished
                    {::wf-id id})


          WF (base/wf! :init [init-fn
                              (base/build-init-chan-factory-fn CHAN-FACTORY)
                              (evt-loop/build-evt-loop-init-fn EVT-LOOP)
                              ;; OPTIONAL: provide *state for workflow init/ctx/steps via base/&state
                              (base/build-init-state-fn *STATE)
                              ]

                       :ctx [
                             evt-loop/evt-loop-ctx-fn
                             (fn [params]
                               {
                                ;; identity function, that logs it's value
                                :v {:fn (fn [v]
                                          (dbg "[WF]" v "\t" (System/currentTimeMillis))
                                          v)}

                                ;; infinite step to indicate that workflow is running. emits new value every 100 ms, until max-num times
                                :8 {:fn       (fn [max-num]
                                                (let [chan-factory (base/&chan-factory params)

                                                      chan (base/make-chan chan-factory (base/rand-sid))
                                                      t (volatile! (u/now))]

                                                  (go-loop [i 0]
                                                    (vreset! t (u/now))

                                                    (async/>! chan (str i ": " (int (rand 100))))
                                                    (async/<! (u/timeout 100))

                                                    (if (< i max-num)
                                                      (recur (inc i))))

                                                  chan))
                                    :infinite true
                                    }
                                })]
                       :steps [
                               (fn [params]                   ;; evt-loop-steps-fn + ws-steps-fn
                                 {
                                  ::evt-loop [:evt-loop (evt-loop/&evt-loop params)]

                                  ::v        [:v "Workflow is running"]

                                  ::counter [:8 30]

                                  ::progress [:v ::counter]

                                  })
                               ]
                       :opts [

                              (base/build-opt-state-fn *STATE)

                              ;; provide a stop signal channel in the workflow state atom
                              (fn [params]
                                {:before-process  (fn [wf-chan xtor]
                                                    (swap! *STATE assoc :STOP-CHAN STOP-CHAN)

                                                    :ok)})

                              ;; close channel factory
                              (base/build-opts-chan-factory-fn CHAN-FACTORY)

                              ;; store results in state map and mark workflow as ended by sending a msg to a signal channel
                              (fn [params]
                                {
                                 ;; log errors
                                 :op-handlers-map {
                                                   :error (fn [err] (println err))
                                                   ;:process (fn [result] (info ::process "\n" (d/pretty result)))
                                                   :done  (fn [result-map]
                                                            (dbg "[WF][DONE]" "\n" result-map)

                                                            ;; save wf-results on done with workflow id
                                                            (swap! *STATE assoc ::WF-RESULTS {(get params ::wf-id) result-map} )

                                                            ;; todo: get from params
                                                            (async/put! STOP-CHAN :wf-done)

                                                            result-map
                                                            )
                                                   }
                                 })

                              ])
          ]

      (base/stateful-wf *STATE WF
                        ;; should we expose the state here?
                        :api {:STOP-CHAN STOP-CHAN}
                        )
      )
    )




;;
;; test running infinite wf, and stopping it
;;
(deftest stateful-wf-run-and-stop-test
  (let [*wf-instance (atom nil)
        thread-blocker (async/chan 1)
        t (async/timeout 5000)]

    ;; thread to launch the workflow
    (async/thread
      (reset! *wf-instance (reload-test-wf *wf-instance :TEST))
      ((:start-wf! @*wf-instance))
      )

    ;; thread that will stop the workflow
    (async/thread
      (Thread/sleep 1000)

      (if-let [old-instance @*wf-instance]
        ;; re-start wf if it's already running
        (let [stop-wf-fn! (:stop-wf! old-instance)]
          (dbg "triggering stop wf")
          (stop-wf-fn!)

          (async/put! thread-blocker :wf-ended)
          ))

      )

    ;; blocking get to get result from thread-blocker or timeout-chan
    (let [[A B] (async/alts!! [thread-blocker t])]
      (dbg  "[main] DONE " )
      ;; ensure that wf was terminated via stop-wf!, not via timeout
      (assert (= :wf-ended A))
      )
    )
)


;;
;; test reloading workflow several times
(deftest stateful-wf-reload-test
  (let [*wf-instance (atom nil)
        thread-blocker (async/chan 1)
        t (async/timeout 5000)

        on-stop (fn [old-state]
                  ;; get stop channel from the current state
                  ;; on last on-done this channel should be filled
                  (let [stop-chan (:STOP-CHAN old-state)]
                    (dbg "ON-STOP" stop-chan)

                    stop-chan
                    )
                  )
        ]

    ;; start A ...        <sleep 1.5s>     stop B->start C.....
    ;;           stop A->start B ...

    (async/thread ;; thread to launch the workflow A, and then C

      (base/auto-run-wf! *wf-instance
                         (fn [old-state]
                           (let [prev-wf-results (::WF-RESULTS @*wf-instance)]
                             (dbg "starting wf :A...\n" (d/pretty! prev-wf-results))
                             (is (nil? prev-wf-results)))

                           (reload-test-wf *wf-instance :A ))
                         :on-stop on-stop
                         )

      (Thread/sleep 1500)

      (base/auto-run-wf! *wf-instance
                         (fn [old-state]
                           (let [prev-wf-results (get-in old-state [::WF-RESULTS :B])]
                             (dbg "starting wf :C...\n" (d/pretty! prev-wf-results))
                             (is (nil? prev-wf-results)))

                           (reload-test-wf *wf-instance :C )
                           )
                         :on-stop on-stop)
      )


    (async/thread ;; thread to run workflow B
      (Thread/sleep 1000)
      (base/auto-run-wf! *wf-instance
                         (fn [old-state]
                           (let [prev-wf-results (get-in old-state [::WF-RESULTS :A])]
                             (is (not (nil? prev-wf-results)) )
                             (dbg "starting wf :B...\n"
                                  (pr-str prev-wf-results))
                             )
                           (reload-test-wf *wf-instance :B))
                         :on-stop on-stop
                         )
      )



    (async/thread ;; thread that will stop the workflow
      (Thread/sleep 3000)

      (if-let [old-instance @*wf-instance]
        ;; re-start wf if it's already running
        (let [stop-wf-fn! (:stop-wf! old-instance)]
          (dbg "triggering stop wf - " (keys old-instance))
          (stop-wf-fn!)

          (let [stop-chan (get old-instance :STOP-CHAN)]
            (go
              (async/<! stop-chan)
              (async/put! thread-blocker :wf-ended)

              )
            )
          ))

      )

    ;; blocking get
    (let [[A B] (async/alts!! [thread-blocker t])
          last-results (get-in @*wf-instance [::WF-RESULTS :C])
          ]
      ;; ensure that wf was terminated via stop-wf!, not via timeout
      (is (= :wf-ended A))

      (dbg  "[main] DONE " last-results )
      ;; ensure that workflow C had started and ended
      (is (not (nil? last-results)))
      )
    )
  )

