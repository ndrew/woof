(ns woof.wf
  "woof workflows"
  (:require [woof.data :as d]
            [woof.cache :as cache]
            [woof.graph :as g]


            #?(:clj [woof.utils :as u :refer [put!? debug!]])
            #?(:cljs [woof.utils :as u])


            #?(:clj [clojure.core.async :as async :refer [go go-loop]])
            #?(:cljs [cljs.core.async :as async])
            )

  #?(:cljs
      (:require-macros
          [cljs.core.async.macros :refer [go go-loop]]
          [woof.utils-macros :refer [put!? debug!]]
        )))



(comment
  "TODO: write introduction to workflows")


;;
;; context is an interface for map type thingy that holds step functions
;;
;; also context is a starting point for workflow - the impl knows how to create an executor
(defprotocol WoofContext
  ;; gets step function by step-id
  (get-step-fn [this step-id])

  ;; factory method for creating executor
  (build-executor [this steps]))




;;
;; executor runs specific workflow
;;
(defprotocol WoofExecutor ; - producer
  "protocol for running workflows"

  ;; starts the workflow, return a channel
  (execute! [this]) ;; TODO: do we need to pass the steps here or in executor constructor?

  ;; halts workflow
  (end! [this]))




;;
;; protocol for processing workflow results
(defprotocol WoofResultProcessor
  (process-results! [this])
  )



;;
;; WIP: IState



(defprotocol WoofSteps

  (initial-steps [this])

  (steps [this])

  (get-steps* [this])
  (get-steps2add* [this])
  (get-steps-left* [this])


  (do-update-steps! [this added-steps])

  (get-infinite-step [this id])

  (set-infinite-step [this id result])

  (steps-ready? [this])

  (add-steps! [this steps])
  (add-pending-steps! [this steps])

  (step-ready! [this id])
  (step-working! [this id])

  (is-step-working? [this id])
)



(defn make-steps-model! [steps]
  (let [*state (atom {:infinite {}})
        *results (atom (array-map))

        *steps (atom steps)
        *steps2add (atom (array-map))
        *steps-left (atom (reduce-kv (fn [a k v] (assoc a k :pending)) {} steps))]

    (reify WoofSteps
      (initial-steps [this] steps)

      (steps [this] @*steps)
      (get-steps* [this] *steps)
      (get-steps2add* [this] *steps2add)
      (get-steps-left* [this] *steps-left)


      (do-update-steps! [this [op nu-steps]]
                 (cond
                   (= op :add)
                   (do
                     (let [new-steps @*steps2add]
                           ; (println "new steps" (d/pretty added-steps) "\nvs\n" (d/pretty new-steps))
                           (when-not (empty? new-steps)
                             (swap! *steps merge new-steps)

                             #_(println "PROCESS ADDED STEPS:\n"
                                      "new-steps —" (d/pretty new-steps)
                                      "steps2add —" (d/pretty @*steps2add)
                                      )
                             (reset! *steps2add (array-map)))))

                   (= op :update)
                   (do
                     ;; <?> should we do ?
                     ;; (swap! *steps merge new-steps)

                     ;; #?(:cljs (.error js/console (pr-str [op nu-steps "local " (empty? @*steps2add)])))
                     ))
                 )

        (get-infinite-step [this id]
            (get-in @*state [:infinite id]))

        (set-infinite-step [this id result]
            (swap! *state update-in [:infinite] assoc id result))


        (steps-ready? [this]
                      (and
                        (empty? (get @*state :infinite {}))
                        (every? #(= % :ok) (vals @*steps-left)))
                      )

        (add-steps! [this actions]
                    (swap! *steps2add into actions)
                    )

        (add-pending-steps! [this actions]

                            ;; fixme:
                            #?(:clj
                            (if (satisfies? clojure.core.protocols/IKVReduce actions)
                              (swap! *steps-left merge (reduce-kv (fn [a k v] (assoc a k :pending)) {} actions))
                              (swap! *steps-left merge (reduce #(assoc %1 %2 :pending) {} actions))
                              ))

                            #?(:cljs
                              (if (satisfies? cljs.core/IKVReduce actions)
                                (swap! *steps-left merge (reduce-kv (fn [a k v] (assoc a k :pending)) {} actions))
                                (swap! *steps-left merge (reduce #(assoc %1 %2 :pending) {} actions))
                              ))

                            )

        (step-ready! [this id]
                     (swap! *steps-left dissoc id))

        (step-working! [this id]
                      (swap! *steps-left assoc id :working))

        (is-step-working? [this id]
                         (= :working (get @*steps-left id)))
      )
    )
  )




;;
;; todo: refactor IState into model and behaviour
(defprotocol IState

  (get-initial-steps [this])
  (get-steps [this])
  (get-steps2add [this])
  (get-steps-left [this])
  (get-results [this])

  (do-commit! [this id result])
  (do-update! [this msg])

  (do-expand! [this id actions])

  (commit! [this id step-id params result])
  (expand! [this id step-id params result])

  (update-steps! [this added-steps])

  (ready? [this])

  (get! [this id])

  (get-context* [this])
  )







; move the params to a :keys
(defrecord WFState [cfg STEPS]
  IState
  ;;
  (get-initial-steps [this] (initial-steps STEPS))

  (get-steps [this] (get-steps* STEPS))
  (get-steps2add [this] (get-steps2add* STEPS))
  (get-steps-left [this] (get-steps-left* STEPS))


  (get-context* [this] (get this :CTX))
  (get-results [this] (get this :RESULTS))


  (do-commit! [this id result]
      (let [*results (get-results this)]

    #?(:clj  (dosync
                (swap! *results assoc id result)

                 (step-ready! STEPS id) ;; todo: dissoc only if not infinite mode
               ))
    #?(:cljs (do
                (swap! *results assoc id result)
                (step-ready! STEPS id) ;; todo: dissoc only if not infinite mode
             ))
    ))


  (do-update! [this msg]
              (let [[id step-id params result] msg]
                ;(println "UPDATE: " (d/pretty msg))

                (let [d-steps (rest (g/get-dependant-steps (steps STEPS) id))]
                  ;;

                  (swap! (get-results this) (partial apply dissoc) d-steps)

                  (add-pending-steps! STEPS d-steps)

                  (do-commit! this id result)
                  ;; put update only if things had changed
                  (go
                    (put!? (:process-channel cfg) [:steps [:update d-steps]] 1000))

                  )))


  (do-expand! [this id actions]
              (let [*results (get-results this)]

                #?(:clj (dosync
                    ;; does order here matters?
                    (swap! *results merge {id (keys actions)}) ;; todo: how to save that action had been expanded

                    (add-steps! STEPS actions)
                    (add-pending-steps! STEPS actions)
                    (step-ready! STEPS id)
                          ))
                #?(:cljs (do
                    ;; does order here matters?
                    (swap! *results merge {id (keys actions)}) ;; todo: how to save that action had been expanded

                    (add-steps! STEPS actions)
                    (add-pending-steps! STEPS actions)
                    (step-ready! STEPS id)
                           ))

                )
              )


  (update-steps! [this params]
                 (do-update-steps! STEPS params))



  (commit! [this id step-id params result]
           ;;(println "commit!" id step-id params result)

           (if (u/channel? result)

             (let [*steps-left (get-steps-left* STEPS)

                   *context (get this :CTX)
                   infinite? (:infinite (get @*context step-id))]

               (if-not (is-step-working? STEPS id)
                 (do

                   (swap! (get-results this) assoc id result)
                   (step-working! STEPS id)

                   (if infinite?
                     (do

                       (when-not (get-infinite-step STEPS id)
                         (set-infinite-step STEPS id result) ;; store channel
                         (go-loop []
                           (let [v (async/<! result)] ;; u/<?
                             ;; TODO: do we need to close the channel
                             ;; #?(:cljs (.warn js/console "infinite!" (pr-str [:update [id step-id params v]]) ))


                             (put!? (:process-channel cfg) [:update [id step-id params v]] 1000))
                             (recur))
                         )

                       )
                    (go
                     (let [v (async/<! result)] ;; u/<?
                       ;; TODO: do we need to close the channel
                       (put!? (:process-channel cfg) [:save [id step-id params v]] 1000))) ;;
                     )
                   )
                 )
               )
             (do-commit! this id result)))


  (expand! [this id step-id params actions]
           (if (u/channel? actions)
             (let [*steps-left (get-steps-left this)
                    status (get @*steps-left id)]
               (when-not (= :working status)
                 (swap! (get-results this) assoc id actions) ;; store channel
                 (swap! *steps-left assoc id :working)
                 (go
                   (let [v (async/<! actions)]
                     ;; TODO: handle if > 1000 actions are added
                     ;; TODO: handle if cycle is being added

                     ;(println (d/pretty [:expand [id step-id params v]]))

                     (put!? (:process-channel cfg) [:expand [id step-id params v]] 1000)))
                 ))
             (do
               (do-expand! this id actions)
               )))

  (ready? [this]
          (steps-ready? STEPS))


  (get! [this id]  ; get!
        (u/nil-get @(get-results this) id))
)





(defn make-state-cfg [steps process-channel] ; bs
  {:steps steps
   :process-channel process-channel}
  )



(defn make-state! [*context state-config]
  (let [*results (atom (array-map))

        steps-model (make-steps-model! (:steps state-config))
        ]

    (assoc
      (->WFState state-config steps-model)

      ; pass the optional params
        :CTX *context
        :RESULTS *results
      )
    )
)








;; kinda debugger - TODO: maybe a context step of more generic way?
(def ^:dynamic *consumer-debugger* nil)
(def ^:dynamic *producer-debugger* nil)

;; backpressure
(def WORKING-MAX 40)







(defn- handle-commit!
  "saves or expands the step"
  [executor save-fn! expand-fn! id step-id params]
  ;; TODO: should expand work with different handler or one will do?
  ;; TODO: should the step return some typed responce?
  ;; TODO: what if step handler is not found?

  (let [step-cfg (get @(:*context executor) step-id)
        f (get-step-fn executor step-id)
        result (f params)]
    (if (:expands? step-cfg)
      (expand-fn! id step-id params result)
      (save-fn! id step-id params result))

    [id [step-id params]]))



;; step processing function. parametrized by get! and commit! fns.
(defn- do-process-step! [get! commit! [id [step-id params]]]
  (let [existing-result (get! id)]
   ;;(println "PROCESSING " [id [step-id params]] existing-result )

    ;;#?(:cljs (.warn js/console "PROCESSING: " (pr-str [id params "---" (get! params)]) ) )

    (cond
      (nil? existing-result) ;; no result -> run step
      (if (u/action-id? params)
        (let [new-params (get! params)]
          (cond
            (nil? new-params) [id [step-id params]]
            (u/channel? new-params) [id [step-id params]]
            :else (commit! id step-id new-params)))

        (commit! id step-id params))

      (u/channel? existing-result) ;; waiting for channel to process result
      [id [step-id params]]

      :else ;; already processed
        [id [step-id params]])))


(defn- put! [c v] ;; TODO: does this even work?
  (let [err (volatile! false)]
    (try
      (async/put! c v)
      (catch
        #?(:clj Throwable)
        #?(:cljs js/Error)
        e
        (println "Error while putting into channel!")
        (vreset! err true)))
    (not @err)))



(defn- freq-map! [steps-left]
  (merge {:working 0} (frequencies steps-left)))

(def freq-map (memoize freq-map!))




(defprotocol WoofStepProcessor ;; "produces" messages while processing wf

  (process-step! [this step])

  (process-steps! [this process-channel steps])
)



(defrecord WFStepProcessor [R commit! get! *prev-added-steps *prev-results]
  WoofStepProcessor

  (process-step!
    [this [id [step-id params]]]


    (let [existing-result (get! id)]
      ;; (println "PROCESSING " [id [step-id params]] existing-result )
      ;; #?(:cljs (.warn js/console "PROCESSING: " (pr-str [id params "---" (get! params)]) ) )

      (cond
        (nil? existing-result) ;; no result -> run step
        (if (u/action-id? params)
          (let [new-params (get! params)]
            (cond
              (nil? new-params) [id [step-id params]]
              (u/channel? new-params) [id [step-id params]]
              :else (commit! id step-id new-params)))

          (commit! id step-id params))

        (u/channel? existing-result) ;; waiting for channel to process result
        [id [step-id params]]

        :else ;; already processed
        [id [step-id params]]))
    )


  (process-steps!
    [this process-channel steps]

    (if-let [cycles (g/has-cycles steps)]
      (u/throw! (str "cycle detected " (d/pretty cycles))))

    ; (println "PRODUCING:")

    (let [ PUT_RETRY_T 1000
         *steps (get-steps R)
         *steps-left (get-steps-left R)
         *results (get-results R)

          prev-steps @*prev-added-steps

          get-freqs (fn [] (freq-map (vals @*steps-left)))

          *new-steps (volatile! (array-map))

          i (volatile! 0)
          backpressure-update (volatile! 0)

          ;; TODO: this should be part of backpressure logic
          start-freqs (get-freqs)
          prev-freqs (volatile! start-freqs)]

      ;; iterate through each
      ;; TODO: randomize steps?
      (doseq [step steps]

        (let [freqs (get-freqs)] ;; TODO: do we need to change freqs on each step?
          (vreset! prev-freqs freqs)

;                                #_(if (= @prev-freqs freqs) ;; debug output if wf is stuck
;                                    (let [pending-steps (keys (into {} (filter #(-> % val u/channel?)) @*results))]
;                                      (println  "waiting for "
;                                                  (if (< 5 (count pending-steps))
;                                                    (count pending-steps)
;                                                    (str (d/pretty @*results) "\n" (d/pretty @*steps-left))))))


          (if (> WORKING-MAX (:working freqs))
            (let [[k v] (process-step! this step)]
              (vswap! *new-steps assoc k v))
            ;; send process on first backpressure
            (when-not (> 10 (- @i @backpressure-update))
              ;(println "backpressure!")
              (put! process-channel [:back-pressure freqs]) ;; FIXME: does this really help
              (vreset! backpressure-update @i))))

        (vswap! i inc))


      (let [new-steps @*new-steps
            results @*results

            steps-added? (not (= prev-steps new-steps))
            results-changed? (not (= @*prev-results results))
            ]


        (when (or steps-added?
                  results-changed? )



          ; #?(:cljs (.warn js/console (d/pretty["STEPS:" "added?" steps-added? "results-changed?" results-changed?])))


          (when results-changed?
;            (println "PRODUCING CONTUNUES: results changed")

            (vreset! *prev-results results)
            (put!? process-channel [:steps [:update new-steps]] PUT_RETRY_T))

          ;; if only added
          (when steps-added? ;(and steps-added? (not results-changed?))
;            (println "PRODUCING CONTUNUES: steps added")

            (vreset! *prev-added-steps new-steps)
            (put!? process-channel [:steps [:add new-steps]] PUT_RETRY_T))

          )

        )

      )
    )
  )


(defn make-processor [R commit! get!]
  ; *prev-results
  ; commit!
  (->WFStepProcessor
    R commit! get!
    (volatile! (array-map))
    (volatile! @(get-results R))
    )
  )




(defn async-exec
  "workflow running algorithm"
  ([executor R ready-channel process-channel]


   ;; todo: add notification channel for ui updates
   ;; todo: rename R to something better
  (let [ PUT_RETRY_T 1000
         steps (get-initial-steps R)

         *steps (get-steps R)
         *steps-left (get-steps-left R)
         *results (get-results R)

         commit! (partial handle-commit! executor (partial commit! R) (partial expand! R))

         processor (make-processor R commit! (partial get! R))]


    ;;
    ;; consumer. processes results from processing channel and pipes them to resulting channel
    (go

      ;; wait if needed, before producing any results
      (debug! *consumer-debugger* { :process-loop-start {
                                                            :i 0
                                                            :old-results @*results
                                                            :steps steps
                                                            :steps-left @*steps-left}})

      (let [first-update (volatile! false)
            force-update (volatile! false)
            force-stop   (volatile! false)]
        ;; wait for results via process-channel
        (loop [i 0
               old-results @*results
               steps steps
               steps-left @*steps-left]

          ;; TODO: add termination if i is growing
          ;; TODO: inject processing logic via transducer
          (let [r (async/<! process-channel)
                [op v] r]

; #?(:cljs (.warn js/console (d/pretty ["consumer" op v])))

            ;; process the 'message'
            (condp = op
              :save (let [[id step-id params result] v]
                      ; got single items via async channel
                      (do-commit! R id result))

              :update (do
                        (do-update! R v)
                        )

              :steps (let [[steps-op _] v]
                       ;; loop completed, merge steps and add new ones (if any)
                       (update-steps! R v)

                       ;; if update is received - force loop one more time
                       ;; todo: move it to process-steps in some
                       (if (= :update steps-op)
                         (vreset! force-update true)
                         ;(async/>! ready-channel [:expand [id result]])
                         )

                       (async/>! ready-channel [:wf-update [@*steps @*results]]))


              :expand (let [[id step-id params result] v]
                        ; (println "GOT EXPAND: " (d/pretty v))
                        (do-expand! R id result)

                        ;; send result
                        ;(async/>! ready-channel [:expand [id result]])
                        )

              :back-pressure (do
                               (when-not @first-update ;; TODO: is this actually needed?
                                 (vreset! first-update true)
                                 ;(async/>! ready-channel [:wf-update @*results])
                                 )

                               )
              :stop (do
                      ;; todo: close the channels in :infinite

                      (vreset! force-stop true)
                      )


              ;; TODO: handle IllegalArgumentException - if unknown op is being sent
              )


            ;; process the result
            (let [processed-steps @*steps
                  new-results @*results
                  same-results? (= old-results new-results)]

; #?(:cljs (.warn js/console (d/pretty ["same-results?" same-results?
;                                      "force-update" @force-update
;                                      "steps-left before" steps-left
;                                      "steps left after" @*steps-left
;                                      ])))


              (when (or (not same-results?)
                        @force-update
                        (not (empty? @*steps-left)) ;; - <?> will this cause different results
                        )
                #_(println "CONSUMING:\n\n"
                         (d/pretty new-results)
                         "steps left: " (d/pretty @*steps-left)
                         )


                (debug! *consumer-debugger* {:process-loop-end {:i i :new-results new-results :steps processed-steps :steps-left @*steps-left}})

                (if-not same-results?                               ;; send the processed results
                  (async/>! ready-channel [:process new-results])
                  )

                (when (not-empty @*steps-left) ;; restart produce
                  (go
                    (debug! *producer-debugger* {:producer {:steps @*steps}}) ;; pause the producer if needed

                    (try                             ;; handle next loop iteration
                      (process-steps! processor process-channel @*steps)
                      (catch
                        #?(:cljs js/Error) #?(:clj  Exception) e
                        (async/>! ready-channel [:error e])))

                    )))
              )

          ;; todo: stop not immediately when force stop
          ;; send done, or keep processing the result
          (if (or @force-stop (ready? R))
              (async/>! ready-channel [:done @*results])
              (recur (inc i) @*results @*steps @*steps-left))))))


    ;;
    ;; producer goroutine
    (go

      (async/>! ready-channel [:init @*results]) ;; is this needed?

      ;; wait before producing
      (debug! *producer-debugger* {:producer {:steps steps}})

      ;; <?> send action that wf had started ?

      (try
        (process-steps! processor process-channel steps)
        (catch
          #?(:cljs js/Error) #?(:clj  Exception) e
          (async/>! ready-channel [:error e])))

      )

    ; return channel, first there will be list of pending actions, then the completed ones be added, then channel will be close on the last
    ready-channel)))





;; WoofContext impl


(defn- get-step-impl [*context] ;; context as map in atom
  (fn [step-id]
    (let [step-cfg (get @*context step-id)]
        (if-let [f (:fn step-cfg)]
          f
          (fn [v]
            (throw
              #?(:clj (Exception. (str "No step handler " step-id " in context" )))
              #?(:cljs (js/Error. (str "No step handler " step-id " in context" )))
            ))))))



(defn- get-step-cached-impl [*context cache]
  ;; cache is ICache
  (fn [step-id]
    (let [step-cfg (get @*context step-id)]
      (if (:expands? step-cfg) ;; todo: add cache flag?
        (:fn step-cfg)
        (cache/memoize! cache (:fn step-cfg) step-id)))
    )
  )






;;
;;


(defrecord AsyncExecutor [*context model ready-channel process-channel]
  WoofExecutor

  (execute! [this]
            (async-exec this model ready-channel process-channel))

  (end! [this]
        (go
          (async/>! process-channel [:stop this])))

  WoofContext

  (get-step-fn [this step-id]
    ((get-step-impl *context) step-id))

  (build-executor [this steps]
                 ;; TODO:
                 )

  )


(defrecord CachedAsyncExecutor [cache *context model ready-channel process-channel]
  WoofExecutor

  (execute! [this]
            (async-exec this model ready-channel process-channel))

  (end! [this]
        (go
          (async/>! process-channel [:stop this])))

  WoofContext

  (get-step-fn [this step-id]
    ((get-step-cached-impl *context cache) step-id))

  (build-executor [this steps]
                 ;; TODO:
                 )
  )




;; API


(defn executor
  "workflow constuctor"
  ([*context steps]
   (let [process-chan (u/make-channel)]
    (executor *context
                    (make-state! *context (make-state-cfg steps process-chan))
                    (u/make-channel)
                    process-chan)))

  ([*context model ready-channel process-channel]

   (let [xctor (->AsyncExecutor *context
                     model
                     ready-channel
                     process-channel)]

     ;; spliting current wf implementation into smaller 'mixin' style protocols
     (reify
        WoofExecutor

        (execute! [this]
                  (execute! xctor))

        (end! [this]
              (end! xctor))

        WoofContext

        (get-step-fn [this step-id]
          ((get-step-impl *context) step-id)) ;; todo: get from context impl

        (build-executor [this steps]
                 ;; TODO:
                 )

        ))

   ))


(defn cached-executor
  "workflow constuctor, step function results are memoized"
  ([*context steps]
   (let [process-chan (u/make-channel)]
    (cached-executor *context
                    (make-state! *context (make-state-cfg steps process-chan))
                    (u/make-channel)
                    process-chan)))
  ([*context model ready-channel process-channel]
    (->CachedAsyncExecutor (cache/->Cache (atom {}))
                     *context model
                     ready-channel
                     process-channel)))



;; how the executor should be instantiated?



;; transducers
;;

(defn chunk-update-xf
  "passes one :process items  "
  [buf-size]
  (fn [rf]
    (let [ctr (volatile! 0)]
      (fn
        ([] (rf)) ; init (arity 0) - should call the init arity on the nested transform xf, which will eventually call out to the transducing process
        ([result] ; completion (arity 1)
         ; why this is never called?
         ; (println "COMPLETE" result)
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



(defn time-update-xf [interval]
  (fn [rf]
    (let [ctr (volatile! 0)]
      (fn
        ([] (rf))              ; init (arity 0)
        ([result] (rf result)) ; completion (arity 1)
        ; Step (arity 2) - this is a standard reduction function but it is expected to call the xf step arity 0 or more times as appropriate in the transducer.
        ; For example, filter will choose (based on the predicate) whether to call xf or not. map will always call it exactly once. cat may call it many times depending on the inputs.
        ([result v]                         ; we ignore the input as
         (let [[status data] v]
           ;;(println "~~~" @ctr "~" status "~~~")
           (if-not (= :process status)
             (rf result v)
             (when (-> (u/now) (- @ctr) (> interval))
               (vreset! ctr (u/now))
               (rf result v)))))))))



(defn time-updated-chan [exec-chan-0 tick-interval]
  (let [exec-chan (async/pipe exec-chan-0
                              (async/chan 1 (time-update-xf tick-interval)))]

    exec-chan))



;; convenience functions
;; ========================================================

(defn- process-wf-result [v]

  ;;(println "process-wf-result" (d/pretty v))

  (if (u/exception? v)
    (u/throw! v)
    (if (nil? v)
      (u/throw! "workflow stopped due to timeout!")
      v)))



;; go loop for processing messages from exec-chan by
(defn process-wf-loop
  ([exec-chan op-handler]
   (go-loop []
            (let [r (async/<! exec-chan)] ;; [status data] r
              (if (op-handler r)
                (recur)))))

  ([exec-chan op-handler t end-fn]
    (process-wf-loop exec-chan op-handler)
    (go
       (async/<! (u/timeout t))
      ;; todo: check if wf had ended
       (end-fn)
    ))
)



(comment
            ;; use destructuring like these
               { init-handler :init
                process-handler :process
                wf-handler :wf-update
               ; :or { process-handler (fn [data] (println (d/pretty data))) }
               } options
          )



(defrecord AsyncWFProcessor [executor options]
  WoofResultProcessor

  (process-results! [this]
    (let [exec-chan (get options :channel (u/make-channel))

          ;; todo: pass here the tick interval or other channel piping options

          t (get this ::timeout)
          op-handler (get options :op-handler
                          (fn [[status data]]
                            (condp = status
                               :error (u/throw! data) ;; re-throw it, so wf will stop
                               :done true

                                false
                              )
                            ))]
      (if t
        (process-wf-loop exec-chan op-handler t (partial end! executor))
        (process-wf-loop exec-chan op-handler))

      exec-chan
    )
  )
)




(defn async-execute!
  ([xctor]
    (process-results! (->AsyncWFProcessor xctor {})))

  ([xctor channel handler]
   (process-results! (->AsyncWFProcessor xctor {
                                         :channel channel
                                         :op-handler handler
                                         })))
  )





(defrecord FutureWF [executor options]
  WoofResultProcessor

  (process-results! [this]
    #?(:clj

        (let [t (get this ::timeout 5000)]
          (future
            (let [timeout-chan (async/timeout t)
                  result-chan (execute! executor)]

              (go-loop []
                       (let [[status data] (async/<! result-chan)]

                         (condp = status
                           :error (do
                                    ;; TODO: error handling strategies8

                                    (async/>! timeout-chan data) ;; send the exception as a result
                                    (u/throw! data)              ;; re-throw it, so wf will stop
                                    )
                           :done (async/>! timeout-chan data)

                           (let [custom-handler (get options status)]
                             (if custom-handler
                                (custom-handler data))
                             (recur))
                           )))

              (process-wf-result (async/<!! timeout-chan)) ;; todo: close channel?
              ))
          )))
)


#?(:clj

  ;; can this be done as tranducer?
  (defn sync-execute!
    ([executor]
     (process-results! (->FutureWF executor {}))

     )
    ([executor t]
      (process-results!
        (assoc (->FutureWF executor {}) ; pass the optional params
                  ::timeout t))
     )
    ([executor t options]
      (process-results!
        (assoc (->FutureWF executor options)
                  ::timeout t))
     )
    )
)



;; TODO: add workflow merging



(defn make-context
  "workflow constuctor"
  ([*context]
   (make-context *context {}))

  ([*context options]

   (let [{} options]
     ;; todo: add cached executor and other options
     (reify
          WoofContext

          (get-step-fn [this step-id]
            ((get-step-impl *context) step-id)) ;; todo: get from context impl

          (build-executor [this steps]
              (executor *context steps))

          ))))



;; TODO: handle stuck workflows after certain timeout
;; TODO: parametrize backpressure handling
;;          Assert failed: No more than 1024 pending puts are allowed on a single channel. Consider using a windowed buffer.

;; TODO: collect expanded steps results

;; TODO: counting puts

