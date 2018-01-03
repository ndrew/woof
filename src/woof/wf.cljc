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



;; TODO: handle stuck workflows after certain timeout
;; TODO: parametrize backpressure handling
;;          Assert failed: No more than 1024 pending puts are allowed on a single channel. Consider using a windowed buffer.


(comment
  "TODO: write introduction to workflows")



(defprotocol IExecutor
  "protocol for running workflows"
  ;; the steps are passed via constructor
  (execute! [this])
  (get-step-fn [this step-id])

  (end! [this])
  )



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


(defn wf-do-save!
  [*results *steps-left id result]
  (swap! *results assoc id result)
;; todo: dissoc only if not infinite mode
  (swap! *steps-left dissoc id)
  )

(defn wf-do-expand! [*results *steps-left *steps2add id actions]
  ;(println "DO EXPAND: " id actions)
  (swap! *steps2add into actions)
  (swap! *results merge {id (keys actions)}) ;; todo: how to save that action had been expanded
  (swap! *steps-left merge (reduce-kv (fn [a k v] (assoc a k :pending)) {} actions))
  (swap! *steps-left dissoc id))



(defrecord WFState [*state *context cfg *steps *steps2add *steps-left *results ]
  IState
  ;;
  (get-initial-steps [this] (:steps cfg))
  (get-context* [this] *context)

  (get-steps [this] *steps)
  (get-steps2add [this] *steps2add)
  (get-steps-left [this] *steps-left)
  (get-results [this] *results)

  (do-commit! [this id result]
    #?(:clj  (dosync (wf-do-save! *results *steps-left id result)))
    #?(:cljs (wf-do-save! *results *steps-left  id result)))


  (do-update! [this msg]
              (let [[id step-id params result] msg]
                ;(println "UPDATE: " (d/pretty msg))


                (let [d-steps (rest (g/get-dependant-steps @*steps id))]

                  (swap! *results (partial apply dissoc) d-steps)
                  (swap! *steps-left merge (reduce #(assoc %1 %2 :pending) {} d-steps))



                  (do-commit! this id result)

                  (go
                    (put!? (:process-channel cfg) [:steps [:update d-steps]] 1000))

                  )))


  (do-expand! [this id actions]
              #?(:clj (dosync (wf-do-expand! *results *steps-left *steps2add id actions)))
              #?(:cljs (wf-do-expand! *results *steps-left *steps2add id actions))
              )

  (update-steps! [this [op nu-steps]]
                (cond
                  (= op :add)
                 (let [new-steps @*steps2add]
                   ; (println "new steps" (d/pretty added-steps) "\nvs\n" (d/pretty new-steps))
                   (when-not (empty? new-steps)
                     (swap! *steps merge new-steps)
                     (reset! *steps2add (array-map))))

                  (= op :update)

                  (do
                    ;;(swap! *steps-left merge (reduce #(assoc %1 %2 :pending) {} nu-steps))
                    ;;(swap! *results (partial apply dissoc) nu-steps)

                    ;;(println (d/pretty @*steps-left))
                    ;;(println (d/pretty @*results))
                    ;;(println "===========")
                    )))



  (commit! [this id step-id params result]
           ;;(println "commit!" id step-id params result)

           (if (u/channel? result)
             (let [status (get @*steps-left id)
                   infinite? (:infinite (get @*context step-id))]
               (if-not (= :working status)
                 (do
                   (swap! *results assoc id result)
                   (swap! *steps-left assoc id :working)
                   (if infinite?
                     (do
                       (swap! *state update-in [:infinite] assoc id result) ;; ;; store channel
                       (go
                         (loop []
                           (let [v (async/<! result)] ;; u/<?
                             ;; TODO: do we need to close the channel
                             (put!? (:process-channel cfg) [:update [id step-id params v]] 1000))
                             (recur)))

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
             (let [status (get @*steps-left id)]
               (when-not (= :working status)
                 (swap! *results assoc id actions) ;; store channel
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
                 (go
                   (put!? (:process-channel cfg) [:expand [id step-id params actions]] 1000))
               )))

  (ready? [this]
          (and
            (empty? (get @*state :infinite {}))
            (every? #(= % :ok) (vals @*steps-left))))

  (get! [this id]  ; get!
        (u/nil-get @*results id))
)




(defn make-state-cfg [steps process-channel]
  {:steps steps
   :process-channel process-channel}
  )

(defn make-state! [*context state-config]
  (let [*state (atom {:infinite {}})
        steps (:steps state-config)
        *steps (atom steps)
        *steps2add (atom (array-map))
        *steps-left (atom (reduce-kv (fn [a k v] (assoc a k :pending)) {} steps))
        *results (atom (array-map))]
    (->WFState *state *context state-config
               *steps *steps2add *steps-left *results )))






;; kinda debugger
(def ^:dynamic *consumer-debugger* nil)
(def ^:dynamic *producer-debugger* nil)

;; backpressure
(def WORKING-MAX 40)




;;
;; workflow transducers

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



(defn async-exec
  "workflow running algorithm"
  ([executor R ready-channel process-channel]
  (let [ PUT_RETRY_T 1000
         steps (get-initial-steps R)
         ;R (make-state! steps process-channel)

         *steps (get-steps R)
         *steps-left (get-steps-left R)
         *results (get-results R)

         get-freqs (fn []
                     (merge {:working 0} (frequencies (vals @*steps-left))))


         commit! (partial handle-commit! executor (partial commit! R) (partial expand! R))
         process-step! (partial do-process-step! (partial get! R) commit!)

         process-steps! (fn [steps]
                          (if-let [cycles (g/has-cycles steps)]
                            (u/throw! (str "cycle detected " (d/pretty cycles))))

                          ;; TODO: should this be a stateful transducer
                          (let [*new-steps (volatile! (array-map))
                                i (volatile! 0)
                                backpressure-update (volatile! 0)
                                start-freqs (get-freqs)
                                prev-freqs (volatile! start-freqs)]

                            ;; iterate through each
                            ;; TODO: randomize steps?
                            (doseq [step steps]

                              (let [freqs (get-freqs)] ;; TODO: do we need to change freqs on each step?
                                (vreset! prev-freqs freqs)

                                #_(if (= @prev-freqs freqs) ;; debug output if wf is stuck
                                    (let [pending-steps (keys (into {} (filter #(-> % val u/channel?)) @*results))]
                                      (println  "waiting for "
                                                  (if (< 5 (count pending-steps))
                                                    (count pending-steps)
                                                    (str (d/pretty @*results)
                                                         "\n" (d/pretty @*steps-left))
                                                    ))))

                                ;; TODO: extract back pressure logic
                                (if (> WORKING-MAX (:working freqs))
                                  (let [[k v] (process-step! step)]
                                      (vswap! *new-steps assoc k v))
                                  (do
                                    ;; send process on first backpressure
                                    (when-not (> 10 (- @i @backpressure-update))
                                      ;(println "backpressure!")
                                      (put! process-channel [:back-pressure freqs]) ;; FIXME: does this really help
                                      (vreset! backpressure-update @i)
                                      ))))

                              (vswap! i inc))


                            #_(when (= start-freqs (get-freqs))
                              ; (println "----------------")
                              #_(println  "waiting for "
                                          (keys (into {} (filter #(-> % val u/channel?)) @*results)))

                              )

                            (put!? process-channel [:steps [:add @*new-steps]] PUT_RETRY_T)

                            ))]




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
            force-stop   (volatile! false)
            force-update (volatile! false)

            ]
        ;; wait for results via process-channel
        (loop [i 0
               old-results @*results
               steps steps
               steps-left @*steps-left]

          ;; TODO: add termination if i is growing
          ;; TODO: inject processing logic via transducer
          (let [r (async/<! process-channel)
                [op v] r]

            ;; process the 'message'
            (condp = op
              :save (let [[id step-id params result] v]
                      ; got single items via async channel
                      (do-commit! R id result))

              :update (do-update! R v)

              :steps (let [[steps-op _] v]
                       (update-steps! R v) ;; loop completed, merge steps and add new ones (if any)

                       (if (= :update steps-op)
                         (vreset! force-update true)
                         )
                       )


              :expand (let [[id step-id params result] v]
                        ;; got steps after sequential processing
                        (do-expand! R id result)
                        (println "EXPAND!!!!!")

                        ;; send result
                        (async/>! ready-channel [:expand [id result]])
                        )

              :back-pressure (do
                               (when-not @first-update ;; TODO: is this actually needed?
                                 (vreset! first-update true)
                                 (async/>! ready-channel [:wf-update @*results])))
              :stop (do
                      ;; todo: close the channels in :infinite

                      (vreset! force-stop true)
                      )


              ;; TODO: handle IllegalArgumentException - if unknown op is being sent
              )


            ;; process the result
            (let [processed-steps @*steps
                  new-results @*results
                  same-results? (= old-results new-results)
                  ]


              (when (or (not same-results?) @force-update)

                ;; TODO: debugging via macro/transducer
                (when (u/channel? *consumer-debugger*)
                  (async/<! *consumer-debugger*)
                  (async/put! *consumer-debugger* {:process-loop-end {
                                                                       :i i
                                                                       :new-results new-results
                                                                       :steps processed-steps
                                                                       :steps-left @*steps-left}}))

                (if-not same-results?
                  ;; send the processed results
                  (async/>! ready-channel [:process new-results]))

                (when (not-empty steps-left) ;; restart produce
                  (go
                    ;; pause the producer if needed
                    (debug! *producer-debugger* {:producer {:steps @*steps}})


                    ;; handle next loop iteration
                    (try
                      (process-steps! @*steps)
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

      (try
        (process-steps! steps)
        (catch
          #?(:cljs js/Error) #?(:clj  Exception) e
          (async/>! ready-channel [:error e])))

      )

    ; return channel, first there will be list of pending actions, then the completed ones be added, then channel will be close on the last
    ready-channel)))




(defrecord AsyncExecutor [*context model ready-channel process-channel]
  IExecutor

  (get-step-fn [this step-id]
    (let [step-cfg (get @*context step-id)]
        (if-let [f (:fn step-cfg)]
          f
          (fn [v]
            (throw
              #?(:clj (Exception. (str "No step handler " step-id " in context" )))
              #?(:cljs (js/Error. (str "No step handler " step-id " in context" )))
            )
          )
      )))

  (execute! [this]
            (async-exec this model ready-channel process-channel))

  (end! [this]
        (println "END!!!")
        (go
          (async/>! process-channel [:stop this])))
  )


(defrecord CachedAsyncExecutor [cache *context model ready-channel process-channel]
  IExecutor

  (get-step-fn [this step-id]
    (let [step-cfg (get @*context step-id)]
      (if (:expands? step-cfg) ;; todo: add cache flag?
        (:fn step-cfg)
        (cache/memoize! cache (:fn step-cfg) step-id))))

  (execute! [this]
            (async-exec this model ready-channel process-channel))

  (end! [this]
        (go
          (async/>! process-channel [:stop this])))

  )



(defn executor
  "workflow constuctor"
  ([*context steps]
   (let [process-chan (u/make-channel)]
    (executor *context
                    (make-state! *context (make-state-cfg steps process-chan))
                    (u/make-channel)
                    process-chan)))

  ([*context model ready-channel process-channel]
    (->AsyncExecutor *context
                     model
                     ready-channel
                     process-channel)))


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





(defn- extract-result [result k]
  (let [r (get result k)]
      (if (u/action-id-list? r)
        (mapcat (fn [a]
                 (let [u (extract-result result a)]
                   (if (seq? u) u [u]))) r)
        r)))


(defn extract-results
  "get workflow results, but for certain keys"
  [result keyz]
  (into (array-map)
        (map (fn[k]
               [k (extract-result result k)])
             keyz)))

;; TODO: add workflow merging






