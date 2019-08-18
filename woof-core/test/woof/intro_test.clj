(ns woof.intro-test
  (:require
    [clojure.test :refer :all]

    [clojure.core.async :as async :refer [go go-loop]]

    [woof.core.runner :as runner]
    [woof.data :as d]
    [woof.wf :as wf]

    [woof.core.processors :as p]

    [woof.wf-data :as wdata]
    [woof.wfc :as wfc]
    [woof.utils :as u]

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
             ]]
    ))



;; intro to woof workflows

; wf is way of representing an async computation
; basically it's a way of doing work and sending messages about computation progress

; on the bottom level wf is an executor (producer) that produces messages and a wf processor
; that handles these messages

;; msg is a tuple [status data], where status is in #{:done :error}

;; executor example

(deftest executor-test

  ;; check that internal protocols are available////////////;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let [z (reify WoofDebug
            (dbg! [this k]
              (prn k)))]
    (dbg! z "azaza")
    )
  #_(let [t 10

          result-chan (async/chan 1)
          executor (reify wf/WoofExecutor
                     ;; start execution
                     (execute! [this]
                       (go
                         (async/<! (u/timeout t))
                         (async/put! result-chan [:done "azaza"]))

                       result-chan)

                     ;; executes specific step
                     (execute-step! [this id step-id params])

                     ;; halts workflow
                     (end! [this]

                       )
                     )]

      (let [v @(p/sync-execute! executor)]
        (prn v)
        )

      )


  )




(deftest simplest-pipeline
  (let [
        ;;
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

      ; create an executor instance for steps
        executor (wf/default-executor context-map steps)
        ]

    ;; execute workflow synchronously (clojure only)
    (let [v @(p/sync-execute! executor)]
      (is (not (nil? v)))

      ;; we can use exec/extract-results to get only step results we need
      (is (= (wdata/extract-results v [::0]) {::0 "Hello World!"}))
      (is (= (wdata/extract-results v [::1]) {::1 "Hello Woof!"}))))
  )







;; core.processors


;; how to get workflow results

;; java
; 1) sync processor - with timeout
; 2) separate thread

;;


(defn context-fn [& r] ;; & {:keys []}
  (println "->CTX: " r)
  {
   :log  {:fn (fn[a]
                (locking *out* (println "DBG:" a))
                (identity a))}

   :log*  {:fn (fn[a]
                 (locking *out* (println "DBG:" a))
                 (identity a))
           :collect? true
           }

   })


(defn steps-fn  [& r]  ;; & {:keys []}
  (println "->STEPS: " r)
  {
   ::hello [:log "Hello!"]

   ::1 [:log* ::hello]

   })


(defn wf! [initial-params]
  (println "wf-fn: " initial-params)

  {
   :wf (fn [params]
         (wfc/params-wf params context-fn steps-fn)
         ) ;; (partial wwf in-chan> out-chan< *local) ;; wf constructor
   :params (merge initial-params {})
   }
  )

#_(deftest ^:intro zzz-test
  (let [params {}]
    (runner/run-wf
      ;; return the params from 'compile' stage
      (fn [] params)

      ;; (fn [params] -> {:wf <wf>, :params {}})
      wf!

      ;;
      (fn [params]
        (println "PUI" params)
        #_(pui/ui-opts *STATE params
                     :done (fn [data]
                             (println "GOT DATA" data)
                             data
                             (swap! *STATE merge {:app {:result data}} )
                             ))
        {}
        )
      ;;
      #_(ui-fn *STATE
             (fn [*STATE]
               [:div
                "finite workflow"
                (default-ui/<wf-ui> "SIMPLE WORKFLOW:" *STATE)]
               )
             :auto-start true
             )
      runner/default-run-fn
      )
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

    (let [z (runner/run-wf
              init-fn ;; defaults
              wf-fn  ;; (fn [params] -> {:wf <wf>, :params {}})
              opts-fn
              (fn [wf-impl opts]

                (prn wf-impl)

                (let [xtor (wfc/wf-xtor wf-impl)
                      processor (assoc
                                  (p/->FutureWF xtor opts)
                                  :woof.core.processors/timeout 1000)]
                  (wf/process-results! processor)
                  )
                )
              )
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


;; TODO: write IN-OUT workflow using current workflow machinery
(deftest ^:intro sync-IN-OUT-wf-test


  (let [;; define which step handlers are available to a wf
        context-fn (fn [& r] ;; & {:keys []}
                     (println "->CTX: " r)
                     {
                      :log  {:fn (fn [a]
                                   ;(/ 100 0)
                                   (locking *out* (println "DBG:" a))
                                   (identity a))}

                      :log* {:fn       (fn [a]
                                         (locking *out* (println "DBG:" a))
                                         (identity a))
                             :collect? true
                             }

                      })]

    (let [r (sync-IN-OUT
              {:data "SAMPLE DATA"}
              context-fn
              (fn [& {:keys [data] :as r}]  ;;
                (println "->STEPS: " (into {} r))

                (with-meta
                  {
                   ;;::data data
                   ::hello [:log "HELLO"] ;;[:log ::data]
                   }
                  ;; todo: what if don't know which steps will be resulting
                  ;; maybe via :op-handlers-map :done
                    ;; but it doesn't change the data

                  {:OUT #{::hello}}
                  )
                )
              )]
      (prn (meta r))
      (prn-in-out r)
      ))


  #_(let [IN {:data "SAMPLE DATA"}
        OUT (atom {})

        META (atom {:IN #{}
                    :OUT #{}
                    })

        init-fn (fn []
          (println "INIT FN" IN)

          (swap! META update :IN into (keys IN))
          IN
          )

        ;; define which step handlers are available to a wf
        context-fn (fn [& r] ;; & {:keys []}
                     (println "->CTX: " r)
                     {
                      :log  {:fn (fn [a]
                                   ;(/ 100 0)
                                   (locking *out* (println "DBG:" a))
                                   (identity a))}

                      :log* {:fn       (fn [a]
                                         (locking *out* (println "DBG:" a))
                                         (identity a))
                             :collect? true
                             }

                      })

        ;; generate steps from the initial data
        IN->steps (fn [& {:keys [data] :as r}]  ;;
                    (println "->STEPS: " (into {} r))

                    ;; mark which keys are OUT
                    (swap! META update :OUT into #{::hello})

                    {
                     ::hello [:log data]
                     })
        ]

    ;; do smth -> OUT

    (let [
          steps-fn IN->steps

          ;;
          wf-fn (fn [initial-params]
                  {
                   :wf     (fn [params]
                             (wfc/params-wf params context-fn steps-fn)
                             ) ;; (partial wwf in-chan> out-chan< *local) ;; wf constructor
                   :params (merge initial-params {})
                   }
                  )


          opts-fn (fn [params]
                    (println "opts-fn: " params)
                    {
                     :params params
                     :opts   {
                              :before-process  (fn [wf-chan xtor]
                                                 (locking *out* (println ":before-process"))
                                                 ; (println "Hello World")

                                                 :ok
                                                 )
                              :op-handlers-map {
                                                :done  (fn [data]
                                                         (println "DONE!\n" (d/pretty data))
                                                         (reset! OUT data)
                                                         )

                                                :error (fn [data]
                                                         (println "ERROR!\n" (d/pretty data))
                                                         )
                                                }
                              }
                     })
          ]


      ; take wf-impl + opts and process them with default processor

      (let [z (runner/run-wf
                init-fn ;; defaults
                wf-fn  ;; (fn [params] -> {:wf <wf>, :params {}})
                opts-fn
                ;; runner/default-run-fn
                (fn [wf-impl opts]
                  (let [xtor (wfc/wf-xtor wf-impl)
                        processor (assoc
                                    (p/->FutureWF xtor opts)
                                    :woof.core.processors/timeout 1000)

                        ]
                    (wf/process-results! processor)
                    )
                  )
                )]

        (let [result (with-meta
                       (merge IN @z)
                       @META)
              ]
          (println "IN: " (select-keys result (:IN @META)))
          (println "OUT: " (select-keys result (:OUT @META)))

          (locking *out* (println "\nfull map:\n" result))



          )

        ;;(locking *out* (println "META\n" @META))




        )

      ;;(/ 100 0)
      )
    )


  )



;; TODO: workflow over atom

