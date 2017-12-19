(ns woof.wf
  "woof workflows"
  (:require [woof.data :as d]
            [woof.cache :as cache]
            [woof.utils :as u]

            #?(:clj [clojure.core.async :as async :refer [go]])
            #?(:cljs [cljs.core.async :as async]))

  #?(:cljs
      (:require-macros [cljs.core.async.macros :refer (go)])))


;; TODO: handle infinite work
;; TODO: handle stuck workflows after certain timeout
;; TODO: parametrize backpressure handling

(comment
  "TODO: write introduction to workflows")



(defprotocol IExecutor
  "protocol for running workflows"
  ;; the steps are passed via constructor
  (execute! [this])
  (get-step-fn [this step-id]))



(defprotocol IState
  (get-initial-steps [this])
  (get-steps [this])
  (get-steps2add [this])
  (get-steps-left [this])
  (get-results [this])

  (do-commit! [this id result])
  (do-expand! [this id actions])

  (commit! [this id step-id params result])
  (expand! [this id step-id params result])

  (ready? [this])

  (get! [this id])
  )


(defn wf-do-save!
  [*results *steps-left id result]
  (swap! *results assoc id result)
  (swap! *steps-left dissoc id)
  )

(defn wf-do-expand! [*results *steps-left *steps2add id actions]
  ;;(println "DO EXPAND: " id actions)
  (swap! *steps2add into actions)
  (swap! *results merge {id (keys actions)}) ;; todo: how to save that action had been expanded
  (swap! *steps-left merge (reduce-kv (fn [a k v] (assoc a k :pending)) {} actions))
  (swap! *steps-left dissoc id))



(defrecord WFState [steps process-channel *steps *steps2add *steps-left *results]
  IState
  ;;
  (get-initial-steps [this] steps)
  (get-steps [this] *steps)
  (get-steps2add [this] *steps2add)
  (get-steps-left [this] *steps-left)
  (get-results [this] *results)

  (do-commit! [this id result]
    #?(:clj  (dosync (wf-do-save! *results *steps-left id result)))
    #?(:cljs (wf-do-save! *results *steps-left  id result)))

  (do-expand! [this id actions]
    #?(:clj (dosync (wf-do-expand! *results *steps-left *steps2add id actions)))
    #?(:cljs (wf-do-expand! *results *steps-left *steps2add id actions)))

  (commit! [this id step-id params result]
           (if (u/channel? result)
             (let [status (get @*steps-left id)]
               (when-not (= :working status)
                 (swap! *results assoc id result) ;; store channel
                 (swap! *steps-left assoc id :working)
                 (go
                   (let [v (async/<! result)] ;; u/<?
                     ;; TODO: do we need to close the channel
                     (u/put!? process-channel [:save [id step-id params v]] 1000)))
                 ))
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
                     (u/put!? process-channel [:expand [id step-id params v]] 1000)))
                 ))
             (do-expand! this id actions)))

  (ready? [this]
          ;; TODO: handle infinite workflow
          (every? #(= % :ok) (vals @*steps-left)))

  (get! [this id]  ; get!
        (let [rr @*results
              r (get rr id)]
          (if (nil? r)
            (if (contains? rr id) ::nil nil)
            r)))

)




(defn make-state! [steps process-channel]
  (let [*steps (atom steps)
        *steps2add (atom (array-map))
        *steps-left (atom (reduce-kv (fn [a k v] (assoc a k :pending)) {} steps))
        *results (atom (array-map))]
    (->WFState steps process-channel
               *steps *steps2add *steps-left *results)))






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
   ;; (println "PROCESSING " [id [step-id params]] existing-result )

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
  [executor steps ready-channel process-channel]
  (let [ PUT_RETRY_T 1000
         R (make-state! steps process-channel)

         *steps (get-steps R)
         *steps2add (get-steps2add R)
         *steps-left (get-steps-left R)
         *results (get-results R)


         cycle-detector (volatile! (u/now))

         get-freqs (fn []
                     (merge {:working 0} (frequencies (vals @*steps-left))))


         commit! (partial handle-commit! executor (partial commit! R) (partial expand! R))
         process-step! (partial do-process-step! (partial get! R) commit!)

         process-steps! (fn [steps]
                          ;; TODO: should this be a stateful transducer
                          (let [*new-steps (atom (array-map))
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
                                      (swap! *new-steps assoc k v))
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

                            (u/put!? process-channel [:steps @*new-steps] PUT_RETRY_T)


                            ))]




    ;;
    ;; consumer. processes results from processing channel and pipes them to resulting channel
    (go

      ;; wait if needed, before producing any results
      (u/debug! *consumer-debugger* { :process-loop-start {
                                                            :i 0
                                                            :old-results @*results
                                                            :steps steps
                                                            :steps-left @*steps-left}})

      (let [first-update (volatile! false)]
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


              :steps (do
                       ;; loop completed, merge steps and add new ones (if any)
                       (let [new-steps @*steps2add]
                         (when-not (empty? new-steps)
                           (swap! *steps merge new-steps) ;; TODO: check for cycles
                           (reset! *steps2add (array-map)))))


              :expand (let [[id step-id params result] v]
                        ;; got steps after sequential processing
                        (do-expand! R id result))

              :back-pressure (do
                        ;; TODO: is this actually needed?
                               (when-not @first-update
                                 (vreset! first-update true)
                                 (async/>! ready-channel [:wf-update @*results])))

              ;; TODO: handle IllegalArgumentException - if unknown op is being sent
              )


            ;; process the result
            (let [processed-steps @*steps
                  new-results @*results]

              (when-not (= old-results new-results)

                ;; TODO: debugging via macro/transducer
                (when (u/channel? *consumer-debugger*)
                  (async/<! *consumer-debugger*)
                  (async/put! *consumer-debugger* {:process-loop-end {
                                                                       :i i
                                                                       :new-results new-results
                                                                       :steps processed-steps
                                                                       :steps-left @*steps-left}}))

                ;; send the processed results
                (async/>! ready-channel [:process new-results])

                (when (not-empty steps-left)
                  (go

                    ;; pause the producer if needed
                    (u/debug! *producer-debugger* {:producer {:steps @*steps}})

                    ;; handle next loop iteration
                    (process-steps! @*steps)))))

          ;; send done, or keep processing the result
          (if (ready? R)
              (async/>! ready-channel [:done @*results])
              (recur (inc i) @*results @*steps @*steps-left))))))


    ;;
    ;; producer goroutine
    (go

      ;; wait before producing
      (u/debug! *producer-debugger* {:producer {:steps steps}})

      (async/>! ready-channel [:init @*results]) ;; is this needed?

      (process-steps! steps))

    ; return channel, first there will be list of pending actions, then the completed ones be added, then channel will be close on the last
    ready-channel))




(defrecord AsyncExecutor [*context steps ready-channel process-channel]
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
            (let [steps (:steps this)]
              (async-exec this steps ready-channel process-channel))))


(defrecord CachedAsyncExecutor [cache *context steps ready-channel process-channel]
  IExecutor

  (get-step-fn [this step-id]
    (let [step-cfg (get @*context step-id)]
      (if (:expands? step-cfg) ;; todo: add cache flag?
        (:fn step-cfg)
        (cache/memoize! cache (:fn step-cfg) step-id))))

  (execute! [this]
            (let [steps (:steps this)]
              (async-exec this steps ready-channel process-channel))))



(defn executor
  "workflow constuctor"
  ([*context steps]
    (executor *context
                    steps
                    (u/make-channel)
                    (u/make-channel)))
  ([*context steps ready-channel process-channel]
    (->AsyncExecutor *context
                     steps
                     ready-channel
                     process-channel)))


(defn cached-executor
  "workflow constuctor, step function results are memoized"
  ([*context steps]
    (cached-executor *context
                    steps
                    (u/make-channel)
                    (u/make-channel)))
  ([*context steps ready-channel process-channel]
    (->CachedAsyncExecutor (cache/->Cache (atom {}))
                     *context steps
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






