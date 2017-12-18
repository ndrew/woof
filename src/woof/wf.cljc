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





(defn- do-commit!
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
  (let [;; TODO: move the state needed into a model
         *steps (atom steps)
         *steps2add (atom (array-map))
         *steps-left (atom (reduce-kv (fn [a k v] (assoc a k :pending)) {} steps))
         *results (atom (array-map))

         cycle-detector (volatile! (u/now))

         get-freqs (fn []
                     (merge {:working 0} (frequencies (vals @*steps-left))))

         do-save! (fn [id result]
                    ;; (println "DO SAVE " id result)
                    (swap! *results assoc id result)
                    (swap! *steps-left dissoc id))

         save! (fn [id result]
                 #?(:clj (dosync (do-save! id result)))
                 #?(:cljs (do-save! id result)))


         do-expand! (fn [id actions]
                      ;;(println "DO EXPAND: " id actions)
                      (swap! *steps2add into actions)
                      (swap! *results merge {id (keys actions)}) ;; todo: how to save that action had been expanded
                      (swap! *steps-left merge (reduce-kv (fn [a k v] (assoc a k :pending)) {} actions))
                      (swap! *steps-left dissoc id))

         expand! (fn [id actions]
                   #?(:clj (dosync (do-expand! id actions)))
                   #?(:cljs (do-expand! id actions)))




         sync-commit! (fn [id step-id params result]
                        (if (u/channel? result)
                          (let [status (get @*steps-left id)]
                            (when-not (= :working status)
                              (swap! *results assoc id result) ;; store channel
                              (go
                                (let [v (async/<! result)] ;; u/<?
                                  ;; TODO: extract the retry logic
                                  (when-not (put! process-channel [:save [id step-id params v]])
                                    (println "cannot put " [:save [id step-id params v]])
                                    (u/timeout 1000)
                                    (println "retrying")
                                    (put! process-channel [:save [id step-id params v]]))))

                              (swap! *steps-left assoc id :working)))
                          (save! id result)))

         sync-expand! (fn [id step-id params actions]
                        (if (u/channel? actions)
                          (go
                            (let [v (async/<! actions)] ;; u/<?
                              ;; TODO:: handle if > 1000 actions are added

                              (when-not (put! process-channel [:expand [id step-id params v]])
                                (println "cannot put " [:expand [id step-id params v]])
                                (u/timeout 1000)
                                (println "retrying")
                                (put! process-channel [:expand [id step-id params v]]))))
                          (do
                            (expand! id actions))))

         get! (fn [id]  ; get!
                 (let [rr @*results
                       r (get rr id)]
                   (if (nil? r)
                     (if (contains? rr id) ::nil nil)
                     r)))


         commit! (partial do-commit! executor sync-commit! sync-expand!)

         process-step! (partial do-process-step! get! commit!)


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


                            (when (= start-freqs (get-freqs))
                              ; (println "----------------")
                              #_(println  "waiting for "
                                          (keys (into {} (filter #(-> % val u/channel?)) @*results)))

                              )

                            ;; TODO: handle these smarter than 2x retry
                            (when-not (put! process-channel [:steps @*new-steps])
                              (println "waiting to put the steps")
                              (u/timeout 1000)
                              (put! process-channel [:steps @*new-steps]))))]




    ;;
    ;; consumer. processes results from processing channel and pipes them to resulting channel
    (go

      ;; wait if needed, before producing any results
      (when (u/channel? *consumer-debugger*) ;; TODO: how to handle the debuging, via transducer or via macro?
        (async/<! *consumer-debugger*)
        (async/put! *consumer-debugger*
                    {
                      :process-loop-start {
                                            :i 0
                                            :old-results @*results
                                            :steps steps
                                            :steps-left @*steps-left}}))

      (let [first-update (volatile! false)]
        ;; wait for results via process-channel
        (loop [i 0
               old-results @*results
               steps steps
               steps-left @*steps-left]

          ;; TODO: add termination if i is growing

          (let [r (async/<! process-channel)
                [op v] r]

            ;; process the 'message'
            (condp = op
              :save (let [[id step-id params result] v]
                      ; got single items via async channel
                      (save! id result))


              :steps (do
                       ;; loop completed, merge steps and add new ones (if any)
                       (let [new-steps @*steps2add]
                         (when-not (empty? new-steps)
                           (swap! *steps merge new-steps)
                           (reset! *steps2add (array-map)))))


              :expand (let [[id step-id params result] v]
                        ;; got steps after sequential processing
                        (expand! id result))

              :back-pressure (do
                        ;; TODO: is this actually needed?
                               (when-not @first-update

                                 ;; #?(:cljs (.warn js/console (d/pretty v)))
                                 ;; #?(:cljs (.warn js/console (d/pretty @*results)))
                                 (vreset! first-update true)
                                 (async/>! ready-channel [:wf-update @*results])))
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
                    (when (u/channel? *producer-debugger*)
                      (async/<! *producer-debugger*)
                      (async/put! *producer-debugger* {:producer {
                                                                   :steps @*steps}}))

                    ;; handle next loop iteration
                    (process-steps! @*steps)))))

            ;; send done, or keep processing the result
          (if (every? #(= % :ok) (vals @*steps-left))
              (async/>! ready-channel [:done @*results])
              (recur (inc i) @*results @*steps @*steps-left))))))


    ;;
    ;; producer goroutine
    (go

      ;; wait before producing
      (when (u/channel? *producer-debugger*)
        (async/<! *producer-debugger*)
        (async/put! *producer-debugger* {:producer {
                                                     :steps steps}}))

      (async/>! ready-channel [:init @*results]) ;; is this needed?

      (process-steps! steps))

    ; return channel, first there will be list of pending actions, then the completed ones be added, then channel will be close on the last
    ready-channel))




(defrecord AsyncExecutor [*context steps ready-channel process-channel]
  IExecutor

  (get-step-fn [this step-id]
    (let [step-cfg (get @*context step-id)]
      (:fn step-cfg)))

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








