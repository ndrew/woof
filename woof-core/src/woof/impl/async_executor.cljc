(ns woof.impl.async-executor
  "AsyncExecutor impl"
  (:require [woof.data :as d]
            [woof.cache :as cache]
            [woof.graph :as g]

            #?(:clj [woof.utils :as u :refer [put!? debug! inline--fn inline--fn1]])
            #?(:cljs [woof.utils :as u])


            #?(:clj [clojure.core.async :as async :refer [go go-loop]])
            #?(:cljs [cljs.core.async :as async])

    ;; for now refer all the protocols and all their methods
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
    ;; impls
            [woof.impl.state :refer [make-state-cfg make-state!]]
            [woof.impl.backpressure :refer [make-backpressure!]]
            )

  #?(:cljs
     (:require-macros
       [cljs.core.async.macros :refer [go go-loop]]
       [woof.utils-macros :refer [put!? debug! inline--fn inline--fn1]]
       )))

;; shorthands

(def sid u/sid)
(def sid? u/sid?)  ;; predicates that check parameter in workflow is a link to other action
(def sid-list? u/sid-list?)
(def rand-sid u/rand-sid)


;; kinda debugger - TODO: maybe a context step of more generic way?
(def ^:dynamic *consumer-debugger* nil)
(def ^:dynamic *producer-debugger* nil)


(defn- freq-map!
  "builds map with number of working and pending steps"
  [steps-left]
  (merge {:working 0} (frequencies steps-left)))

(def freq-map (memoize freq-map!))



(defn do-async-consume! [STATE force-stop r]

  (let [[op v] r]

    (condp = op

      :save (let [[id step-id params result] v]     ; step is done, store the result
              (do-commit! STATE id step-id result)

              ; (do-update-sync! STATE v)

              (do-update! STATE v)
              )
      :expand (let [[id step-id params result] v]   ; step expanded

                (do-expand! STATE id result)

                ;; fixme: do the update here synchronously
                ; (g    do-update! STATE v)
                (do-update-sync! STATE v)

                )

      :stop (do ;; todo: gracefully shutdown
              (vreset! force-stop true))

      ;; else
      (do
        (locking *out* (println "infinite upd" r))
        )
      )

    )
  )



(defn- do-async-process-steps!
  "workflow running algorithm"
  ([context R ready-channel process-channel inf-process-chan processor executor ]


   (let [produce-chan (async/chan)
         async-id (rand-int 1000)
         ]


     (consume-steps! processor
                     ready-channel
                     process-channel
                     inf-process-chan
                     produce-chan)


     (go
       (let [had-sync? (volatile! false)]

         (loop []
           (let [[op steps] (async/<! produce-chan)
                 f (partial produce-steps! processor
                            ready-channel
                            process-channel
                            steps)
                 s? (sync? R)]

             (if (or @had-sync? s?) ;; TODO: is sync? working correct?
               (do
                 (vreset! had-sync? true)

                 #?(:clj (dosync (inline--fn f)))
                 #?(:cljs (inline--fn f))
                 )
               ;; do we need to
               (go
                 (produce-steps! processor
                                 ready-channel
                                 process-channel
                                 steps))
               )


             (when-not (= :done op)
               (recur))))))



     ;;
     ;; producer goroutine
     (go
       (async/>! ready-channel [:init @(get-results R)])    ;; notify that processing started. Is this needed? maybe use send-message
       (async/>! produce-chan [:steps (get-initial-steps R)])
       )

     (if (satisfies? WoofWorkflowMsg context) (send-message context :start executor))

     ; return channel, first there will be list of pending actions, then the completed ones be added, then channel will be close on the last
     ready-channel

     )
    ))



(defn- do-handle-commit! [executor context wf-state id step-id params]
  (let [step-cfg (get-step-config context step-id)  ; (get @(:*context executor) step-id) ;; FIXME:
        infinite? (:infinite step-cfg)
        f (get-step-fn executor step-id)
        store-result! (fn[result]
                        (if (:expands? step-cfg)
                          (expand! wf-state id step-id params result)
                          (commit! wf-state id step-id params result)))

        ]

    ;; fixme: why f is called several times

    (if (and (:collect? step-cfg)
             (sid-list? params))
      ;; collect
      (do

        (let [all-collected (get-all! wf-state (get!* wf-state params))]

          (when (not-any? #(or (nil? %1)
                               (u/channel? %1)
                               ) (flatten all-collected))
            (store-result! (f all-collected))

            )

          )
        )
      ;; process sid or value
      (do ;; (nil? (get! wf-state id))

        #_(if infinite? (dosync (store-result! (f params)))
                        (store-result! (f params)))
        ;; do we need sync
        (store-result! (f params))

        )



      )


    [id [step-id params]]
    )
  )



(defn- handle-commit!
  "runs step implementation "
  [executor context wf-state id step-id params]
  ;; TODO: should expand work with different handler or one will do?
  ;; TODO: should the step return some typed responce?
  ;; TODO: what if step handler is not found?

  ;; TODO: catch exception in (f ...)


  (do-handle-commit! executor context wf-state id step-id params)

  )



(defrecord WFStepProcessor [STATE XTOR CTX options]
  WoofStepProcessor



  ;; processes 'step' [sid <sbody>]
  (process-step!
    [this [id [step-id params]]]



    (let [existing-result (get! STATE id)

          run-step! (partial execute-step! XTOR id step-id)]


      (cond
        (nil? existing-result)               ; no step result - run the step
        (do
          (if (sid? params)
            (let [new-params (get! STATE params)]

              (cond
                (or (nil? new-params)
                    (u/channel? new-params)) [id [step-id params]]
                :else
                (run-step! new-params)))
            (run-step! params)))

        (u/channel? existing-result)          ; waiting for channel to process result
        [id [step-id params]]

        :else                                 ; step seems to be processed
        [id [step-id params]]))

    )



  (consume-steps!
    [this ready-channel
     process-channel
     inf-process-chan
     produce-chan
     ]

    ;; consumer. processes results from processing channel and pipes them to resulting channel

    (let [ *steps       (get-steps STATE)
          *steps-left  (get-steps-left STATE)
          *results     (get-results STATE)

          messenger    (get options :messenger)
          notify-ui!   (fn [msg-key msg]
                         (if messenger (send-message messenger msg-key msg)))
          ]


      ;;
      ;; normal consuming
      (go

        ;; wait if needed, before producing any results
        (debug! *consumer-debugger* { :process-loop-start { :i 0                     ;; TODO: do not evaluate debug output - be lazy
                                                           :old-results @*results
                                                           :steps (get-initial-steps STATE)
                                                           :steps-left @*steps-left}})

        (let [force-update (volatile! false)
              force-stop   (volatile! false)]
          ;;
          ;; start consume loop
          (loop [i 0

                 old-results @*results

                 steps (get-initial-steps STATE)
                 steps-left @*steps-left]


            (let [r (async/<! process-channel) ;; TODO: inject processing logic via transducer?
                  [op v] r]


              #_(locking *out* (println "=> " op
                                        (cond (= op :expand) (str (pr-str (first v)) "\n")
                                              (= op :steps) (str (pr-str (first v))  "\n")
                                              :else "")
                                        ))


              ;; consume the 'message'.        <?>: what if unknown op is sent?
              (condp = op

                :save (let [[id step-id params result] v]     ; step is done, store the result
                        (do-commit! STATE id step-id result))

                :expand (let [[id step-id params result] v]   ; step expanded
                          (do-expand! STATE id result)
                          (vreset! force-update true))

                ;; <?> when this happens?
                :update (do
                          ;(locking *out* (println "do update " v))
                          (do-update! STATE v))


                :steps (let [[steps-op _] v]                   ; loop completed, merge steps and add new ones (if any)

                         (update-steps! STATE v)

                         (if (= :update steps-op)              ; if update is received - force loop one more time
                           (vreset! force-update true))

                         (async/>! ready-channel [:wf-update [@*steps @*results]]))

                :back-pressure (do                             ; producer notified us of backpressure
                                 ;; todo: send backpressure via msg channel
                                 (async/>! ready-channel [:back-pressure :what-to-put-here])
                                 (notify-ui! :back-pressure :what-to-put-here))
                :stop (do
                        ;; todo: gracefully shutdown
                        ;;   close the channels in :infinite
                        (vreset! force-stop true)))


              (let [processed-steps @*steps
                    new-results @*results
                    same-results? (= old-results new-results)

                    new-steps-left @*steps-left
                    same-steps-left? (= steps-left new-steps-left)]

                (when (or (not same-results?)
                          @force-update
                          (not (empty? @*steps-left)))
                  ; <?> will force-update cover not empty @*steps2add

                  (debug! *consumer-debugger* {:process-loop-end {:i i :new-results new-results :steps processed-steps :steps-left new-steps-left}})

                  (if-not same-results?                                ; send the processed results
                    (async/>! ready-channel [:process new-results]))


                  (when (and                                           ; restart-producing
                          (not-empty new-steps-left)
                          (or @force-update (not same-steps-left?)))

                    ;; FIXME: queue producing restart
                    ; (produce-steps! this ready-channel process-channel @*steps)

                    (put!? produce-chan [:steps @*steps] 1000)

                    ))
                )

              ;; todo: stop not immediately when force stop

              (if (or @force-stop (ready? STATE))                        ; send done, or keep processing the result
                (let [last-results @*results]
                  (async/>! ready-channel [:done last-results])
                  (async/>! produce-chan [:done @*steps])

                  (notify-ui! :stop last-results))
                (recur (inc i) @*results @*steps @*steps-left))   ;; todo: add timeout/termination if i is growing
              )
            )

          ;;
          )
        )

      ;; infinite actions consuming
      (let [has-infinite-steps (nil? inf-process-chan)]

        ;;        (println "INF-LOOP")
        (go-loop []
          ;;          (println "->START")

          (let [force-stop   (volatile! false)]
            ;; FIXME:
            (let [r (async/<! inf-process-chan) ;; TODO: inject processing logic via transducer?
                  ]

              ;; make main consume thread wait?

              #?(:clj (dosync
                        (do-async-consume! STATE force-stop r)))

              #?(:cljs (do-async-consume! STATE force-stop r)))

            (if-not @force-stop
              (recur)
              )

            )
          )


        )

      )

    )



  (produce-steps!
    [this ready-channel process-channel steps]


    (let [{
           *prev-added-steps ::prev-added-steps
           *prev-results ::prev-results

           WORKING-MAX :working-max
           PARALLEL-MAX :parallel-max
           PENDING-MAX :pending-max
           *tmp-results ::intermed-results
           } options

          initial-freqs (freq-map (vals @(get-steps-left STATE)))

          start-produce? (fn[] (if-let [BP (get options :bp)] (start-producing? BP) true))
          stop-produce? (fn[i] (if-let [BP (get options :bp)] (stop-producing? BP i) false))

          PUT_RETRY_T 1000

          dbg-map #(reduce (fn[r [k v]]
                             (assoc r k
                                      (cond (u/channel? v) :chan
                                            (and (vector? v) (keyword? (first v)))
                                            [(first v) (if (u/channel? (second v)) :chan (second v))] ;; :x ;(str v) ;[(name v) (if (u/channel? (second v)) :chan (second v))]
                                            :else v))
                             ) {} %)


          produce-id (rand-int 1000)


          ]


      ;; two ways of limiting the producing:
      ;; 1) do not start the go block
      ;; 2) wait in go block

      ;; can-produce?





      (when (start-produce?) ; there is a capacity for producing more

        ;; (locking *out* (println "START PRODUCING!" produce-id))

        ;; how to know that we have to sync producing (for infinite actions)

        ;;        (go

        (debug! *producer-debugger* {:producer {:steps steps}}) ;; pause the producer if needed


        (try
          (if-let [cycles (g/has-cycles steps)]
            (u/throw! (str "cycle detected " (d/pretty cycles))))


          ; handle next iteration

          ; #?(:cljs (.log js/console (u/now))) ;; todo: right now it's a marker that producing started


          (let [ *steps (get-steps STATE)
                *steps-left (get-steps-left STATE)
                *results (get-results STATE)

                prev-steps @*prev-added-steps
                *new-steps (volatile! (array-map))


                ]


            (loop [steps steps
                   i 0]


              (when-not (empty? steps)

                (let [step (first steps)
                      [id [step-id _]] step
                      step-cfg (get-step-config CTX step-id)
                      infinite? (:infinite step-cfg)]

                  ;; ?

                  (let [[k v]
                        (if infinite? (do
                                        (if-let [r (get @*tmp-results id)]
                                          r
                                          (let [r (process-step! this step)]
                                            (vswap! *tmp-results assoc id r)
                                            r
                                            ))
                                        )
                                      (process-step! this step))]
                    (vswap! *new-steps assoc k v))

                  ;; FIXME: stop producing if there are new pending produce blocks

                  ;; should we exit here, or resume with new steps?
                  (if-not (stop-produce? i)
                    (recur (rest steps) (inc i))))
                ))

            ;;;              (locking *out* (println "SAVING STEPS!" produce-id))



            (let [new-steps @*new-steps


                  ;; todo: check difference
                  steps-added? (and
                                 (not (= prev-steps new-steps))
                                 (not (= (keys prev-steps) (keys new-steps)))
                                 )

                  results-changed? (not (= @*prev-results @*results))
                  ]

              (when (or steps-added?
                        results-changed?)

                (when results-changed? ;; results changed
                  (vreset! *prev-results @*results)

                  (if-not steps-added?
                    (put!? process-channel [:steps [:update new-steps]] PUT_RETRY_T)))

                ;; if only added
                (when steps-added? ;(and steps-added? (not results-changed?))
                  ; (println "PRODUCING CONTUNUES: steps added")

                  (vreset! *prev-added-steps new-steps)
                  (put!? process-channel [:steps [:add new-steps]] PUT_RETRY_T))
                )


              (vreset! *tmp-results {})

              )
            ;;;                (locking *out* (println "PRODUCE DONE:" produce-id))

            )

          (catch
            #?(:cljs js/Error) #?(:clj  Exception) e
                                                   (go (async/>! ready-channel [:error e]))
                                                   )))

      )

    )

  ;;    )

  )





(defn step-processor-impl
  "constructor for step processor"
  [STATE XTOR CTX] ;; context only for send-message

  (->WFStepProcessor
    STATE XTOR CTX
    {
     ;; volatile vars for statefull processing
     ::prev-added-steps (volatile! (array-map))     ;;  *prev-added-steps
     ::prev-results (volatile! @(get-results STATE)) ;; *prev-results

     ;; back-pressure configs
     :working-max 50 ;; WORKING-MAX
     :parallel-max 20 ;; PARALLEL-MAX
     :pending-max 100

     ;;
     :messenger (if (satisfies? WoofWorkflowMsg CTX) CTX nil)

     :bp (make-backpressure! STATE CTX)
     ::intermed-results (volatile! (array-map))

     ;;
     })
  )


;;
;; def executor
(defrecord AsyncExecutor [context model ready-channel] ;; todo: migrate to separate ns
  WoofExecutor

  ;; todo: wiretap the process-channel for debug

  (execute! [this]
    (do-async-process-steps! context model ready-channel
                             (get-in model [:cfg :process-channel])
                             (get-in model [:cfg :infinite-channel])
                             (step-processor-impl model this context) this))

  ;; todo: move to the processor
  (execute-step! [this id step-id params]
    (handle-commit! this context model id step-id params))


  (end! [this]
    (go
      (async/>! (get-in model [:cfg :process-channel]) [:stop this])

      (if-let [inf-process-chan (get-in model [:cfg :infinite-channel])]
        (do

          (async/>! inf-process-chan [:stop this])

          )

        )

      ))

  WoofContext

  (get-step-fn [this step-id]
    (get-step-fn context step-id))

  (get-step-config [this step-id]
    (get-step-config context step-id)))



;;

;;
;; TODO: to test cached executor, for infinite steps


(defrecord
  CachedAsyncExecutor [cache context model ready-channel process-channel]
  WoofExecutor

  (execute! [this]
    (do-async-process-steps! context model ready-channel process-channel nil (context model this context) this ))

  (execute-step! [this id step-id params]
    (handle-commit! this context model id step-id params))

  (end! [this]
    (go
      (async/>! process-channel [:stop this])))

  WoofContext

  (get-step-fn [this step-id]
    (get-step-fn context step-id))

  (get-step-config [this step-id]
    (get-step-config context step-id)))


(defn cached-executor
  "workflow constuctor, step function results are memoized"
  ([context steps]
   (let [process-chan (u/make-channel)
         inf-process-chan (async/chan 10)]
     (cached-executor context
                      (make-state! context (make-state-cfg steps process-chan inf-process-chan))
                      (u/make-channel)
                      process-chan)))
  ([context model ready-channel process-channel]
   (->CachedAsyncExecutor (cache/->Cache (atom {}))
                          context model
                          ready-channel
                          process-channel)))

;;;

