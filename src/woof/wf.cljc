(ns woof.wf
  "woof workflows"
  (:require [woof.data :as d]
            [woof.cache :as cache]
            [woof.graph :as g]

            [compact-uuids.core :as uuid]

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


;; workflow = ƒ(context, steps)


;;
;; steps map

;; steps map is a non-nested map that can be executed by woof
;;
;; steps = kv-map <sid, sbody>,
;; where
;;   sid   - unique id represented as qualified keyword or via sid function
;;   sbody - a tuple [step-id, params], where step-id describes which step handler to use from context with particular params

;; params can be a value or sid


(def sid u/sid)

(def sid? u/sid?)  ;; predicates that check parameter in workflow is a link to other action

(def sid-list? u/sid-list?)




(defn- gen-uuid []
  #?(:clj  (java.util.UUID/randomUUID)
     :cljs (random-uuid)))


(defn rand-sid
  "generates random sid"
  ([]
    (sid (uuid/str (gen-uuid))))
  ([prefix]
   (sid prefix (uuid/str (gen-uuid)))))



(defn sbody
  "generates step body for the specified step-handler-id and a value x"
  ([step-handler-id x]
    (sbody step-handler-id identity x))
  ([step-handler-id f x]
     [step-handler-id (f x)]))





;;
;; context


;; context holds units of execution in woof
;;
;; context = map<step-id, step-handler>,
;; where
;;   step-id      - a unique keyword per context (non-qualified).
;;   step-handler - a special kind of single-arity function with metadata assigned

;; context is defined like,

; {
;    :step-handler-id { :fn (fn [a]
;                             (str "Hello " a))
;                        :some :metadata
;    }
;}

; or via shorthand functions

; { :step-handler-id (step-handler (fn[x]..) :some :metadata )}

;; handler function can return the value (sync handler) or a core.async channel (async handler)

;; also, handlers can behave differently according to the metadata - TODO:


(defn step-handler
  "step hander constructor function"

  [f & {:keys [expands? collect?]
        :or {expands? false
             collect? false}}]
  ;; TODO: add other possible flags and validation
  {:fn f
   :expands? expands?
   :collect? collect?
   ; TODO: add infinite
   })




(defn- steps-from-sids
  [sid-fn step-body-fn ids]
  (into {} (map
             (fn [x]
               [(sid-fn x) (step-body-fn x)])
             ids)))






(defn expand-handler
  "generates an expand step handler

  by default step-handler-fn returns {(rand-sid) (step-body-fn x)}
  "
  ([sid-fn step-body-fn]
   (expand-handler sid-fn step-body-fn
                   (fn[x] {(rand-sid) (step-body-fn x)}))
   )
  ([sid-fn step-body-fn step-handler-fn]
   (step-handler (fn [ids]
                      (if (sid-list? ids)
                        (steps-from-sids sid-fn step-body-fn ids)
                        (if (sid? ids)
                          (steps-from-sids sid-fn step-body-fn [ids])
                          (step-handler-fn ids))))
                    :expands? true))
  )





;; context implementation is WoofContext protocol

(defprotocol WoofContext

  ;; returns step function by its step-id, or throws an error
  (get-step-fn [this step-id])

  ;; returns step handler metadata as a map
  (get-step-config [this step-id])


)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; public protocols
;;


;; constructor protocol that will know which execturor impl to take

(defprotocol WoofExecutorFactory
  ;; factory method for creating executor
  (build-executor [this steps]) ;; TODO: does it belong here?
  )


;; msg handler from from the workflow - for clean-up and some initial setup

(defprotocol WoofWorkflowMsg
  ;; <?> does this have to be synchronous
  (send-message [this event data])

  )


;;
;; executor runs specific workflow
;;
(defprotocol WoofExecutor ; - producer
  "protocol for running workflows"

  ;; starts the workflow, return a channel
  (execute! [this]) ;; TODO: do we need to pass the steps here or in executor constructor?
  ;; todo: pass opts here?


  ;;
  (execute-step! [this id step-id params])



  ;; halts workflow
  (end! [this])) ;; should return channel. and receive opts




;;
;; protocol for processing workflow results
(defprotocol WoofResultProcessor

  ;; processes results received from executor
  (process-results! [this])
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; internal protocols



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



;;
;; todo: refactor WoofState into model and behaviour
(defprotocol WoofState

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
  (get!* [this id-list])

  )


(defprotocol WoofStepProcessor ;; "produces" messages while processing wf

  (process-step! [this step])

  (produce-steps! [this ready-channel process-channel steps])

  (consume-steps! [this ready-channel process-channel])
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; internal impl


(defn- sid-list-from-expand-map [expand-map]
  (let [sids (keys expand-map)]
    (if-let [{expand-key :expand-key} (meta expand-map)]
      (list expand-key)
      sids)
    )
  )


(defn- do-expand-impl! [STEPS *results id actions]


  (if-not (get-infinite-step STEPS id)
    (do

      (swap! *results merge {id (sid-list-from-expand-map actions)})

      (add-steps! STEPS actions)
      (add-pending-steps! STEPS actions)
      (step-ready! STEPS id)

      )
    (do
      ;; TODO: infinite expand-key

      ; (swap! *results merge {id (keys actions)})
      (swap! *results update-in [id]
             (fn[a b]
               (if (seq? a)
                 (concat a b)
                 b))
             (keys actions))


      (add-steps! STEPS actions)
      (add-pending-steps! STEPS actions)
      ;; (step-ready! STEPS id)

      )
    )
  )


; move the params to a :keys
(defrecord WFState [cfg STEPS]
  WoofState
  ;;
  (get-initial-steps [this] (initial-steps STEPS))

  (get-steps [this] (get-steps* STEPS))
  (get-steps2add [this] (get-steps2add* STEPS))
  (get-steps-left [this] (get-steps-left* STEPS))


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



  (update-steps! [this params]
                 (do-update-steps! STEPS params))


  (commit! [this id step-id params result]

           (if (u/channel? result)

             (let [*steps-left (get-steps-left* STEPS)
                   infinite? (:infinite (get-step-config (get this :CTX) step-id))]

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
                   step-cfg (get-step-config (get this :CTX) step-id)
                   infinite? (:infinite step-cfg)]

               (when-not (is-step-working? STEPS id)

                 (swap! (get-results this) assoc id actions) ;; store channel
                 ; (swap! *steps-left assoc id :working)
                 (step-working! STEPS id)


                 (if infinite?
                   (do
                     (when-not (get-infinite-step STEPS id)
                       (set-infinite-step STEPS id actions)

                       (go-loop []
                                (let [v (async/<! actions)]
                                  (put!? (:process-channel cfg) [:expand [id step-id params v]] 1000))

                                (recur)

                                )
                       )

                     )
                   (go
                     (let [v (async/<! actions)]
                       ;; TODO: handle if > 1000 actions are added
                       ;; TODO: handle if cycle is being added

                       ;(println (d/pretty [:expand [id step-id params v]]))

                       (put!? (:process-channel cfg) [:expand [id step-id params v]] 1000)))
                   ))
               )
             (do
               (do-expand! this id actions)
               )))

    (do-expand! [this id actions]
              (let [*results (get-results this)]

                #?(:clj (dosync
                  (do-expand-impl! STEPS *results id actions)))

                #?(:cljs (do-expand-impl! STEPS *results id actions))

                ))



  (ready? [this]
          (steps-ready? STEPS))


  (get! [this id]  ; get!
        (u/nil-get @(get-results this) id))

  (get!* [this id-list]
         ;; todo: nils?
         (map (partial get! this) id-list)
         )

)






(defn make-state-cfg [steps process-channel] ; bs
  {:steps steps
   :process-channel process-channel}
  )





(defn- make-steps-model! [steps]
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
          ; do not distinguish between :add and :update

          (let [new-steps @*steps2add]
            (when-not (empty? new-steps)
                (swap! *steps merge new-steps)

                (reset! *steps2add (array-map)))))


        (get-infinite-step [this id]
            (get-in @*state [:infinite id]))

        (set-infinite-step [this id result]
            (swap! *state update-in [:infinite] assoc id result))


        (steps-ready? [this]
                      (and
                        (empty? (get @*state :infinite {}))
                        (every? #(= % :ok) (vals @*steps-left))))

        (add-steps! [this actions]
          (swap! *steps2add into actions))

        (add-pending-steps! [this actions]
          #?(:clj
              (if (satisfies? clojure.core.protocols/IKVReduce actions)
                (swap! *steps-left merge (reduce-kv (fn [a k v] (assoc a k :pending)) {} actions))
                (swap! *steps-left merge (reduce #(assoc %1 %2 :pending) {} actions))))

          #?(:cljs
              (if (satisfies? cljs.core/IKVReduce actions)
                (swap! *steps-left merge (reduce-kv (fn [a k v] (assoc a k :pending)) {} actions))
                (swap! *steps-left merge (reduce #(assoc %1 %2 :pending) {} actions)))))

        (step-ready! [this id]
                     (swap! *steps-left dissoc id))

        (step-working! [this id]
                      (swap! *steps-left assoc id :working))

        (is-step-working? [this id]
                         (= :working (get @*steps-left id)))
      )
    )
  )





(defn- make-state! [context state-config]

  (let [*results (atom (array-map))

        steps-model (make-steps-model! (:steps state-config))
        ]

    (assoc
      (->WFState state-config steps-model)

      ; pass the optional params
        :CTX context ;; FIXME: pass context implicit
        :RESULTS *results
      )
    )
)








;; kinda debugger - TODO: maybe a context step of more generic way?
(def ^:dynamic *consumer-debugger* nil)
(def ^:dynamic *producer-debugger* nil)

;; backpressure
;;(def WORKING-MAX 30)










;; step processing function. parametrized by get! and commit! fns.
(defn- do-process-step! [get! commit! [id [step-id params]]]
  (let [existing-result (get! id)]

    ; (println "PROCESSING " [id [step-id params]] existing-result )

    ;;#?(:cljs (.warn js/console "PROCESSING: " (pr-str [id params "---" (get! params)]) ) )

    (cond
      (nil? existing-result) ;; no result -> run step
      (if (sid? params)
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







(defrecord WFStepProcessor [STATE XTOR options]
  WoofStepProcessor



  (process-step! ;; FIXME: how this different from handle commit?
    [this [id [step-id params]]]

    (let [existing-result (get! STATE id)]

      (cond
        ;; no result -> run step

        (nil? existing-result)
        (if (sid? params)
          (let [new-params (get! STATE params)]
            (cond
              (nil? new-params) [id [step-id params]]
              (u/channel? new-params) [id [step-id params]]
              :else (execute-step! XTOR id step-id new-params)))

          (execute-step! XTOR id step-id params))

        (u/channel? existing-result) ;; waiting for channel to process result
        [id [step-id params]]

        :else ;; already processed
        [id [step-id params]]))
    )


  (consume-steps!
    [this ready-channel process-channel]

    ;; consumer. processes results from processing channel and pipes them to resulting channel

    (let [ ; PUT_RETRY_T 1000

           *steps (get-steps STATE)
           *steps-left (get-steps-left STATE)
           *results (get-results STATE)

           messenger (get options :messenger)
           notify-ui! (fn [msg-key msg]
                        (if messenger (send-message messenger msg-key msg)))
           ]


    (go

      ;; wait if needed, before producing any results

      (debug! *consumer-debugger* { :process-loop-start { ;; TODO: do not evaluate debug output - be lazy
                                                          :i 0
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


            ;; consume the 'message'.        <?>: what if unknown op is sent?
            (condp = op
              :save (let [[id step-id params result] v]     ; step is done, store the result
                      (do-commit! STATE id result))

              :expand (let [[id step-id params result] v]   ; step expanded
                        (do-expand! STATE id result)
                        (vreset! force-update true))

              :update (do-update! STATE v)  ;; <?> when this happens?


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

              (when (or (not same-results?) @force-update (not (empty? @*steps-left)))
                ; <?> will force-update cover not empty @*steps2add

                (debug! *consumer-debugger* {:process-loop-end {:i i :new-results new-results :steps processed-steps :steps-left new-steps-left}})

                (if-not same-results?                                ; send the processed results
                  (async/>! ready-channel [:process new-results]))


                (when (and                                           ; restart-producing
                        (not-empty new-steps-left)
                        (or @force-update (not same-steps-left?)))
                  (produce-steps! this ready-channel process-channel @*steps)))
              )

            ;; todo: stop not immediately when force stop

            (if (or @force-stop (ready? STATE))                        ; send done, or keep processing the result
              (let [last-results @*results]
                (async/>! ready-channel [:done last-results])
                (notify-ui! :stop last-results))
              (recur (inc i) @*results @*steps @*steps-left))   ;; todo: add timeout/termination if i is growing
            )
          )

         ;;
        )
      )))


  (produce-steps!
    [this ready-channel process-channel steps]

    ;; note that should be in a single go block

    ;; FIXME: put processing into queue

    (let [{*prev-added-steps ::prev-added-steps
           *prev-results ::prev-results
           WORKING-MAX :working-max
           PARALLEL-MAX :parallel-max
           PENDING-MAX :pending-max
           } options

          initial-freqs (freq-map (vals @(get-steps-left STATE)))
          ]


      ;; two ways of limiting the producing:
        ;; 1) do not start the go block
        ;; 2) wait in go block

      (when  (or (> PARALLEL-MAX (:working initial-freqs))
                 false)  ;; (> PENDING-MAX (:pending initial-freqs))

                ;; there is a capacity for producing more

        (go
          (debug! *producer-debugger* {:producer {:steps steps}}) ;; pause the producer if needed

          ;; handle next iteration
          (try
            (if-let [cycles (g/has-cycles steps)]
              (u/throw! (str "cycle detected " (d/pretty cycles))))


            (let [ PUT_RETRY_T 1000
                   *steps (get-steps STATE)
                   *steps-left (get-steps-left STATE)
                   *results (get-results STATE)

                   prev-steps @*prev-added-steps

                   get-freqs (fn [] (freq-map (vals @*steps-left)))

                   *new-steps (volatile! (array-map))

                   i (volatile! 0) ;; cycle counter
                   backpressure-update (volatile! 0)
                   backpressure-sent (volatile! (u/now))

                   ;; TODO: this should be part of backpressure logic
                   start-freqs (get-freqs)
                   prev-freqs (volatile! start-freqs)

                   timer-fn (u/exp-backoff 10 2 10000)

                   CHECK-BACKPRESSURE-ON 10
                   ]


              (doseq [step steps]
                ;; TODO: randomize steps?


                (let [[k v] (process-step! this step)]
                  (vswap! *new-steps assoc k v)

                  ;; send process on first backpressure
                  (when-not (> CHECK-BACKPRESSURE-ON (- @i @backpressure-update))
                    (let [freqs (get-freqs)] ;; check freqs every 20th

                      ;; check the trend
                      (vreset! prev-freqs freqs)

                      (when (> (:working freqs) WORKING-MAX ) ;;

                        ;; (println "backpressure " (d/pretty freqs) @i @backpressure-update)

                        (let [wait-time (timer-fn)]
                          ;#?(:cljs (.warn js/console "pausing producer for " wait-time "backpressure cycle " @backpressure-update))

                          (if (> (- (u/now) @backpressure-sent) 1000) ;; FIXME: send on each second - maybe use transducer?
                            (do
                              (put! process-channel [:back-pressure freqs])
                              (vreset! backpressure-sent (u/now))
                              )
                            ;(vswap! backpressure-sent inc)
                            )

                          (async/<! (u/timeout wait-time)) ;; todo: incremental timeout
                          ))

                      ; #?(:cljs (.warn js/console "resuming" (d/pretty (get-freqs))))

                      ;(println "backpressure!")
                      ;; send this once per


                      (vreset! backpressure-update @i))))

                (vswap! i inc))

              (let [new-steps @*new-steps
                    results @*results

                    steps-added? (not (= prev-steps new-steps))
                    results-changed? (not (= @*prev-results results))]


                (when (or steps-added?
                          results-changed? )

                  ; #?(:cljs (.warn js/console (d/pretty["STEPS:" "added?" steps-added? "results-changed?" results-changed?])))

                  #_(println "__results-changed?" results-changed?
                             "__steps-added?" steps-added?)


                  (when results-changed?
                    ;            (println "PRODUCING CONTUNUES: results changed")

                    (vreset! *prev-results results)

                    (if-not steps-added?
                      (put!? process-channel [:steps [:update new-steps]] PUT_RETRY_T))

                    )

                  ;; if only added
                  (when steps-added? ;(and steps-added? (not results-changed?))
                    ;            (println "PRODUCING CONTUNUES: steps added")

                    (vreset! *prev-added-steps new-steps)
                    (put!? process-channel [:steps [:add new-steps]] PUT_RETRY_T))
                  )

                )


              )

            (catch
              #?(:cljs js/Error) #?(:clj  Exception) e
              (async/>! ready-channel [:error e]))))

        )

      )

    )

  )



(defn step-processor-impl
  [STATE XTOR CTX] ;; context only for send-message

  (->WFStepProcessor STATE XTOR
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
                      ;;
                      })
  )













;; WoofContext impl

(defn- get-step-config-impl [context step-id]
  (get context step-id))


(defn- get-step-impl [context] ;; context as map in atom
  (fn [step-id]
    (let [step-cfg (get-step-config-impl context step-id)]
        (if-let [f (:fn step-cfg)]
          f
          (fn [v]
            (throw
              #?(:clj (Exception. (str "No step handler " step-id " in context" )))
              #?(:cljs (js/Error. (str "No step handler " step-id " in context" )))
            ))))))



(defn- get-step-cached-impl [context cache]
  ;; cache is ICache
  (fn [step-id]
    (let [step-cfg (get-step-config-impl context step-id)]
      (if (:expands? step-cfg) ;; todo: add cache flag?
        (:fn step-cfg)
        (cache/memoize! cache (:fn step-cfg) step-id)))
    )
  )







(defn- handle-commit!
  "run! step implementation for WoofExecutor"
  [executor context wf-state id step-id params]
  ;; TODO: should expand work with different handler or one will do?
  ;; TODO: should the step return some typed responce?
  ;; TODO: what if step handler is not found?

  ;; TODO: catch exception in (f ...)

  (let [save-fn! (partial commit! wf-state)
        expand-fn! (partial expand! wf-state)

        step-cfg (get-step-config context step-id)  ; (get @(:*context executor) step-id) ;; FIXME:
        f (get-step-fn executor step-id)]

    (if (and (:collect? step-cfg)
             (sid-list? params))
      ;; collect
      (let [collected (get!* wf-state params)]
        (when (not-any? #(or (nil? %1) (u/channel? %1)) collected)
          (let [result (f collected)] ;; TODO: can this be (f (filter-collected collected)) for collect? and expands? - or new type of step?
            (if (:expands? step-cfg)
              (expand-fn! id step-id params result)
              (save-fn! id step-id params result))
            )))
      ;; process sid or value
      (let [result (f params)]
        (if (:expands? step-cfg)
          (expand-fn! id step-id params result)
          (save-fn! id step-id params result))

        )
      )
    [id [step-id params]]
    )
  )




(defn- async-consume
  "workflow running algorithm"
  ([context R ready-channel process-channel processor executor ]



   (consume-steps! processor ready-channel process-channel)

   ;; todo: remove executor

   ;; todo: add notification channel for ui updates
   ;; todo: rename R to something better
  (let [ PUT_RETRY_T 1000
         steps (get-initial-steps R)

         *steps (get-steps R)
         *steps-left (get-steps-left R)
         *results (get-results R)
          ]


    ;;
    ;; consumer. processes results from processing channel and pipes them to resulting channel
    #_(go

      ;; wait if needed, before producing any results

      (let [timer-fn (u/exp-backoff 10 2 250)
            first-update (volatile! false)
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
            ;; process the 'message'. TODO: via state?
            (condp = op
              :save (let [[id step-id params result] v]
                      ; got single items via async channel
                      (do-commit! R id result))

              :expand (let [[id step-id params result] v]
                        (do-expand! R id result)

                        (vreset! force-update true))
              :update (do
                        (do-update! R v))

              :steps (let [[steps-op _] v]

                       ;; loop completed, merge steps and add new ones (if any)
                       (update-steps! R v)

                       ;; if update is received - force loop one more time
                       (if (= :update steps-op)
                         (vreset! force-update true))

                       (async/>! ready-channel [:wf-update [@*steps @*results]]))

              :back-pressure (do
                               (when-not @first-update ;; TODO: is this actually needed?
                                 (vreset! first-update true)
                                 ;(async/>! ready-channel [:wf-update @*results])

                                 ;; do we have to pause consumer?
                                 #_(let [wait-time (timer-fn)]
                                    #?(:cljs (.warn js/console "pausing consumer for " wait-time))
                                    (async/<! (u/timeout wait-time)) ;; todo: incremental timeout
                                  )


                                 ;; timer-fn
                                 )
                                (async/>! ready-channel [:back-pressure :what-to-put-here])
                               ; should this event be via resulting channel, or via send-message?
                               ;   +
                               ;   -

                               ; backpressure start/stop?


                               )
              :stop (do
                      ;; todo: gracefully shutdown
                      ;;   close the channels in :infinite
                      (vreset! force-stop true))

              ;; TODO: handle IllegalArgumentException - if unknown op is being sent
              )


            ;; process the result
            (let [processed-steps @*steps
                  new-results @*results
                  same-results? (= old-results new-results)

                  new-steps-left @*steps-left
                  same-steps-left? (= steps-left new-steps-left)
                  ]

              (when (or (not same-results?)
                        @force-update
                        (not (empty? @*steps-left)) ;; - <?> will this cause different results
                        ;(not (empty? @*steps2add))
                        )


                (debug! *consumer-debugger* {:process-loop-end {:i i :new-results new-results :steps processed-steps :steps-left new-steps-left}})

                (if-not same-results?                               ;; send the processed results
                  (async/>! ready-channel [:process new-results]))

                (when (not-empty new-steps-left)
                  ;;

                  (if (or @force-update (not same-steps-left?)) ;; wait
                    ;; restart-producing
                    (produce-steps! processor ready-channel process-channel @*steps)

                    (do ;; do we need to sleep in consumer?
                      #_(let [ms (timer-fn)]
                       (println " wait " ms "ms")
                      (async/<! (u/timeout ms))))

                    )


                  ))
              )

          ;; todo: stop not immediately when force stop
          ;; send done, or keep processing the result
          (if (or @force-stop (ready? R))
              (let [last-results @*results]
                (async/>! ready-channel [:done last-results])
                (if (satisfies? WoofWorkflowMsg context) (send-message context :stop last-results)))
              (recur (inc i) @*results @*steps @*steps-left))))))


    )

    ;;
    ;; producer goroutine
   (go
      ;; notify that processing started. Is this needed? maybe use send-message
      (async/>! ready-channel [:init @(get-results R)]))


   (produce-steps! processor ready-channel process-channel (get-initial-steps R))

   (if (satisfies? WoofWorkflowMsg context) (send-message context :start executor))

   ; return channel, first there will be list of pending actions, then the completed ones be added, then channel will be close on the last
   ready-channel
   ))


;;
;;


(defrecord AsyncExecutor [context model ready-channel process-channel]
  WoofExecutor

  (execute! [this]
            (async-consume context model ready-channel process-channel (step-processor-impl model this context) this))

  ;; todo: move to the processor
  (execute-step! [this id step-id params]
             (handle-commit! this context model id step-id params))


  (end! [this]
        (go
          (async/>! process-channel [:stop this])))

  WoofContext

  (get-step-fn [this step-id]
    (get-step-fn context step-id))

  (get-step-config [this step-id]
    (get-step-config context step-id))

  )


(defrecord CachedAsyncExecutor [cache context model ready-channel process-channel]
  WoofExecutor

  (execute! [this]
            (async-consume context model ready-channel process-channel (context model this context) this ))

  (execute-step! [this id step-id params]
                 (handle-commit! this context model id step-id params))

  (end! [this]
        (go
          (async/>! process-channel [:stop this])))

  WoofContext

  (get-step-fn [this step-id]
    (get-step-fn context step-id))

  (get-step-config [this step-id]
    (get-step-config context step-id))

  )




;; API


;; default executor
(defn- executor
  "workflow constuctor"
  ([context steps]
   (let [process-chan (u/make-channel)]
    (executor context
                    (make-state! context (make-state-cfg steps process-chan))
                    (u/make-channel)
                    process-chan)))

  ([context model ready-channel process-channel]
   (let [xctor (->AsyncExecutor context
                     model
                     ready-channel
                     process-channel)]

     ;; executor should (may?) also act as context, but proxy the impl to actual context
     (reify
        WoofExecutor

        (execute! [this]
                  (execute! xctor))

        (end! [this]
              (end! xctor))

        WoofContext

        (get-step-fn [this step-id]
          (get-step-fn context step-id))

        (get-step-config [this step-id]
          (get-step-config context step-id))

        )
     )

   ))


(defn cached-executor
  "workflow constuctor, step function results are memoized"
  ([context steps]
   (let [process-chan (u/make-channel)]
    (cached-executor context
                    (make-state! context (make-state-cfg steps process-chan))
                    (u/make-channel)
                    process-chan)))
  ([context model ready-channel process-channel]
    (->CachedAsyncExecutor (cache/->Cache (atom {}))
                     context model
                     ready-channel
                     process-channel)))



;; how the executor should be instantiated?




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







;; default impl for a parametrizible result processor via options map
(defrecord ResultProcessor [executor options]
  WoofResultProcessor

  (process-results!
    [this]

    (let [op-handlers-map (get options :op-handlers-map {}) ;; FIXME: add :default handler as in cond

          ;; FIXME: find proper names for these keys

          {execute-fn! :execute
           before-processing! :before-process
           after-processing!  :after-process
           process-loop-handler :process-handler ;; low level, use with caution

           timeout :timeout

           :or
           {
             execute-fn! (fn [executor]
                           (execute! executor))

             before-processing! (fn[exec-chan])

             ;; redefine this with caution
             process-loop-handler (fn[msg]
                                    (let [[status data] msg
                                          continue? (-> status #{:done :error} nil?)]

                                      (if-let [status-handler (get op-handlers-map status)]
                                        (status-handler data))

                                      continue?))

             after-processing! (fn[exec-chan] exec-chan)
             }

           } options ]

      (let [exec-chan (execute-fn! executor)]
        (before-processing! exec-chan)

        (if timeout ;; todo: handle stoping via timeout
          (process-wf-loop exec-chan process-loop-handler timeout (partial end! executor))
          (process-wf-loop exec-chan process-loop-handler))

        (after-processing! exec-chan)
        )
      )
    )
  )








;; deprecated processor
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









;; TODO: add workflow merging


;; TODO: handle stuck workflows after certain timeout
;; TODO: parametrize backpressure handling
;;          Assert failed: No more than 1024 pending puts are allowed on a single channel. Consider using a windowed buffer.

;; TODO: collect expanded steps results

;; TODO: counting puts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; public interface


;;
;; WoofContext




;; WoofContext constructor
(defn make-context
  "creates context from context map
  possible options are:
  {:process-channel <chan>
   :on {:start (fn [x])
        :stop ..
        :other-wf-event-key
        }}
  "
  ([context]
   (make-context context {}))

  ([context options]
   (let [{
            process-channel :process-channel
            events-map :on
          }
         (merge {
                  :process-channel (u/make-channel)
                  :on {:start (fn [data])
                       :stop  (fn [data]) }

                  } options)]
     ;; todo: add cached executor and other options
     (reify
          WoofContext

          (get-step-fn [this step-id]
            ((get-step-impl context) step-id)) ;; todo: get from context impl

          (get-step-config [this step-id]
            (get-step-config-impl context step-id))

          WoofExecutorFactory

          (build-executor [this steps]
              (executor this
                    (make-state! this (make-state-cfg steps process-channel))
                    (u/make-channel)
                    process-channel))

           WoofWorkflowMsg

           (send-message [this event-key data]
            (if-let [f (get events-map event-key)]
              (f data)))



       ))))



;;
;; WoofResultProcessor



;;
;; where options
;; {
;;   ::timeout <ms>
;;
;;   ; custmer handler for each result message
;;
;;   :error
;;   :done
;; }




(defrecord FutureWF [executor options]
  WoofResultProcessor

  (process-results! [this]
    #?(:clj

        ;; TODO: how to handle timeouts, as :timeout?

        (let [t (get this ::timeout 5000)]
          (future
            (let [timeout-chan (async/timeout t)
                  result-chan (execute! executor)]

              (go-loop []
                       (let [[status data] (async/<! result-chan)]
                         (condp = status
                           :error (do
                                    ;; TODO: error handling strategies
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

  ;; js promise?
)


#?(:clj


  ;; can this be done as tranducer?
  (defn sync-execute!
    "executes workflow and returns result as a future"
    ;; TODO: process-results-sync!
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




;;
;; shorthands for common transducers

(def chunk-update-xf u/chunk-update-xf)

(def time-update-xf u/time-update-xf)

(def time-updated-chan u/time-updated-chan)
