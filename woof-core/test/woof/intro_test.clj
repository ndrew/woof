(ns woof.intro-test
  (:require
    [clojure.test :refer :all]

    [clojure.core.async :as async :refer [go go-loop]]

    [woof.data :as d]
    [woof.wf :as wf]

    [woof.core.processors :as p]

    [woof.wfc :as wfc]
    [woof.utils :as u]

    ;; internal protocols
    [woof.core.protocols :as protocols
     :refer [WoofDebug dbg!
             WoofContext get-step-fn get-step-config
             WoofWorkflowMsg send-message
             WoofSteps
               initial-steps steps get-steps* get-steps2add* get-steps-left*
               do-update-steps! get-infinite-steps get-infinite-step set-infinite-step
               steps-ready? add-steps! add-pending-steps! step-ready! step-working! is-step-working? has-infinite?
             WoofState
               get-initial-steps get-steps get-steps2add get-steps-left get-results do-commit! do-update!
               do-update-sync! do-expand! commit! expand! update-steps! ready? sync? get! get!* get-all!
             WoofStepProcessor
               process-step! produce-steps! consume-steps!
             WoofBackPressure
               start-producing? stop-producing?
             WoofExecutor
               execute! execute-step! end!

             ]]

    [woof.base :as base]
    ))



;; intro to woof workflows:

; wf is way of representing an async computation
; basically it's a way of doing work and sending messages about computation progress
;
; on the bottom level wf is an executor (producer) that produces messages and a wf processor
; that handles these messages
;
; msg is a tuple [status data], where status is in #{:done :error}
;

;; ultra-low-level: executor example

(deftest executor-test

  ;; WoofDebug is a simple debugging protocol for tracking stuff happening in wf
  (let [dbg-impl (reify WoofDebug
                   (dbg! [this msg] (print msg)))]

    (let [test-msg "boo!"
          dbg-output (with-out-str (dbg! dbg-impl test-msg))]
      (is (= dbg-output test-msg))))

  ;; WoofExecutor

  ;; test implementation of the executor that just returns the wf result
  (let [wf-ready-msg "azaza"
        result-chan (async/chan 1)

        sample-timeout 10
        executor (reify WoofExecutor
                   ;; start execution
                   (execute! [this]
                     (go
                       (async/<! (u/timeout sample-timeout))
                       (async/put! result-chan [:done wf-ready-msg]))

                     result-chan)

                   ;; executes specific step
                   (execute-step! [this id step-id params]
                     ;; for now - do nothing
                     )

                   ;; this is called to halt workflow abruptly
                   (end! [this]
                     ;; do nothing
                     )
                   )]

    ;; as we know that this workflow will end, so we can just wait for the result
    ;; but there are infinite workflows that can run until some event will halt them
    (let [result @(p/sync-execute! executor)]
      (is (= result wf-ready-msg)))
    )
  )


;; wf with context-map and steps example

;; the idea is to represent the computation as data that can be executed by wf
;; executor knows how to execute steps - pieces of computation.
;; each step has it's unique step-id (or sid), which is a qualified keyword and a step body
;; step body is pair of step-handler-id and step input, like
;;   ::step-id [:step-handler-id {:input :parameter}]
;;
;; step handlers are defined via context map - where step handlers are defined
;;
;; step handler is defined as a map with :fn that holds an actual step handler
;;
;; it's an executor job to trigger the step handler with inputs from step body
;; if all steps had been executed then executors sends :done message with a result
;;
;; result is a map of the same structure as steps (called flat-map),
;; but with step result instead of step body

(deftest simplest-pipeline
  (let [
        context-map {
                     :hello {:fn (fn [a]                  ;; each step handler accepts 1 argument
                                   (str "Hello " a))}
                     }

        ; workflow is described by a flat map with finite number of steps
        steps {
               ;; each step has it's unique id and step definition
               ;; step definition is hiccup style vector where first el is step-id and second is step parameters
               ::0 [:hello "World!"]
               ::1 [:hello "Woof!"]}

      ; create an executor instance for specified context-map and steps
        executor (wf/default-executor context-map steps)]

    ;; execute workflow synchronously (clojure only)
    (let [v @(p/sync-execute! executor)]
      (is (not (nil? v)))

      ;; we can use exec/extract-results to get only step results we need
      (is (= (base/extract-results v [::0]) {::0 "Hello World!"}))
      (is (= (base/extract-results v [::1]) {::1 "Hello Woof!"}))))
  )

;; todo: add test regarding steps linking to other steps and step handler types

;; the idea behind steps and context maps is that they can be combined
;; via simple map merge - if they don't have overlapping ids

;; also context map and steps can be returned via functions
;; called context-fn and step-fn

;; this allows to parametrize the step handlers and step with some
;; initial params map, which can also be combined as maps and returned via function
;; called init-fn

;; this is called parametrized workflow

;; for parametrized workflows there is a separate workflow runner
;; which builds more concepts atop of executor

;; usage is
;; (woof.core.runner/run-wf
;;    init-fn ; (fn [] => defaults )
;;    wf-fn   ; (fn [params] -> {:wf <wf>, :params {}})
;;    opts-fn ; (fn [params] -> {:opts <>, :params {}}
;;    run-fn ; (fn [wf_impl opts]))
;
;; init-fn — returns initial parameters map
;; that is later passed into opts-fn
;;
;; opts-fn — returns initial properties further (maybe modifying it)
;; along with opts map, where opts map describes how executor should
;; handle its messages, like wf is completed, etc.
;;   TODO: maybe it should be executor-opts?


(deftest parametrized-wf-test
  (let [;; store wf run progress in atom
        *progress (atom [])
        track-progress! (fn [progress]
                          (swap! *progress conj progress))

        params {:debug-prefix "DBG:"}
        ;; init-fn returns params map for further usage in context-fn and steps-fn
        init-fn (fn []
                  (track-progress! :init-fn)
                  ;;(*progress)
                  params)
        ;;
        context-fn (fn [& initial-params]
                     (track-progress! [:context-fn initial-params])
                     ;; todo: use :debug-prefix
                     {
                      :log  {:fn (fn [a]
                                   (track-progress! [:log a])
                                   (identity a))}

                      :log* {:fn       (fn [a]
                                         (track-progress! [:log* a])

                                         (identity a))
                             :collect? true
                             }
                      })

        steps-fn (fn [& initial-params]         ;; & {:keys []}
                   (track-progress! [:steps-fn initial-params])
                   {
                    ::hello [:log "Hello!"]
                    ::1     [:log* ::hello]
                    })

        opts-fn (fn [params]
                  (track-progress! [:opts-fn params])
                  {
                   :params params
                   :opts   {
                            :before-process  (fn [wf-chan xtor]
                                               ;; todo: use channel here
                                               (track-progress! :before-wf)
                                               ;; note, that here we return channel, if we
                                               ;; need to wait for something before wf starts
                                               :ok
                                               )
                            :op-handlers-map {
                                              :process (fn [interm-result]
                                                         (track-progress! [:process interm-result]))
                                              :error   (fn [error]
                                                         (track-progress! [:error error]))
                                              :done    (fn [result]
                                                         (track-progress! [:done result])

                                                         ; (println (d/pretty @*progress))
                                                         (is (=
                                                               [:init-fn
                                                                [:wf-fn {:debug-prefix "DBG:"}]
                                                                [:opts-fn {:debug-prefix "DBG:", :added-from-opts :yeah!}]
                                                                [:wf-xtor-fn {:debug-prefix "DBG:", :added-from-opts :yeah!}]
                                                                [:context-fn '(:debug-prefix "DBG:" :added-from-opts :yeah!)]
                                                                [:steps-fn '(:debug-prefix "DBG:" :added-from-opts :yeah!)]
                                                                :before-wf
                                                                [:log "Hello!"]
                                                                [:log* "Hello!"]
                                                                [:process {::hello "Hello!", ::1 "Hello!"}]
                                                                [:done {::hello "Hello!", ::1 "Hello!"}]]
                                                               @*progress
                                                               ))

                                                         )
                                              }
                            }
                   })

        wf-fn (fn [initial-params]
                (track-progress! [:wf-fn initial-params])
                {
                 :wf     (fn [params]
                           (track-progress! [:wf-xtor-fn params])

                           (wfc/params-wf params
                                          context-fn ;; context-fn [& params]
                                          steps-fn)   ;; steps-fn [& params]
                           )
                 :params (merge initial-params {:added-from-opts :yeah!})
                 }
                )
        ]

    ;; note that here we don't return a workflow result,
    ;; we just start the wf, so we need the test to wait for the wf results

    ;; so we'll run a wf in a separate thead and wait some time for wf to be executed

    ;; TODO: use channel, instead of thread
    (async/thread

      (base/run-wf-internal!
        :init-fn init-fn
        :wf-fn wf-fn
        :opts-fn opts-fn)
      )

    (Thread/sleep 100)


    )
  )


(defn sync-IN-OUT [IN
                   context-fn
                   steps-fn
                   ;
                   ]
  (let [META (atom {:IN  #{}
                    :OUT #{}
                    })
        init-fn (fn []
                  (swap! META update :IN into (keys IN))
                  IN)


        wf-fn (fn [initial-params]
                {
                 ;; wf constructor
                 :wf     (fn [params]
                           (wfc/params-wf params context-fn
                                          (fn [& r]
                                            (let [res (apply steps-fn r)]
                                              (if-let [m (meta res)]
                                                (swap! META merge m)
                                                (u/throw! "No metadata attached to workflow steps")
                                                )
                                              res
                                              )
                                            )

                                          ))
                 :params (merge initial-params {})
                 }
                )

        opts-fn (fn [params]
                  {
                   :params params
                   :opts   {
                            :before-process  (fn [wf-chan xtor]
                                               ; (locking *out* (println ":before-process"))
                                               :ok
                                               )
                            :op-handlers-map {
                                              :__done  (fn [data]

                                                       )

                                              :__error (fn [data]
                                                       (println "ERROR!\n" (d/pretty data))
                                                       )
                                              }
                            }
                   })
        ]

    (let [z (base/run-wf-internal! :processor-fn (partial p/TimeoutFutureWF_ 1000)
                                   :init-fn init-fn
                                   :wf-fn wf-fn
                                   :opts-fn opts-fn)

          result @z
          ]

      (with-meta
              (merge IN result)
              @META)

      )

    )
  )

(defn prn-in-out [result]
  (println "IN: " (select-keys result (:IN (meta result))))
  (println "OUT: " (select-keys result (:IN (meta result))))

  (println "\nfull map:\n" result)
  )




