(ns woof.wf
  "woof workflows"
  (:require [woof.data :as d]
            [woof.cache :as cache]
            [woof.graph :as g]

            #?(:clj [woof.utils :as u :refer [put!? debug! inline--fn inline--fn1]])
            #?(:cljs [woof.utils :as u])


            #?(:clj [clojure.core.async :as async :refer [go go-loop]])
            #?(:cljs [cljs.core.async :as async])
            )

  #?(:cljs
      (:require-macros
          [cljs.core.async.macros :refer [go go-loop]]
          [woof.utils-macros :refer [put!? debug! inline--fn inline--fn1]]
        )))


;; debug stuff

(defn debug [& r]
  (locking *out*
    (apply println r)))




;; quick hacky way of getting insides for debugging purposes
(defprotocol WoofDebug

  ;; processes results received from executor
  (dbg! [this k])
  )







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

;; shorthands

(def sid u/sid)
(def sid? u/sid?)  ;; predicates that check parameter in workflow is a link to other action
(def sid-list? u/sid-list?)


(def rand-sid u/rand-sid)



(defn sbody
  "generates step body for the specified step-handler-id and a value x"
  ([step-handler-id param]
    (sbody step-handler-id identity param))
  ([step-handler-id f param]
     [step-handler-id (f param)]))





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


;; right now the step handler behaviour is determined at 'compile' time
;    TODO: dynamic expands? collects? etc.


;;
;; step handler constructors


;; generic cosntructor
(defn step-handler
  "step hander constructor function"

  [f & {:keys [expands? collect?]
        :or {expands? false
             collect? false}}]
  ;; TODO: add other possible flags and validation
  ;;   infinite?
  {:fn f
   :expands? expands?
   :collect? collect?
   ; TODO: add infinite
   })


;; expand

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
                    :expands? true)))

;; infinite expand


;; todo: add transducer
(defn infinite-expand-handler
  "generates infinite expand handler that waits for the steps from the channel param"
  []
  {
    :fn (fn[in>]
          (let [chan> (async/chan)]
            (go-loop []
                     (when-let [v (async/<! in>)]
                       (if-not (map? v)
                         (u/throw! (str "invalid expand map passed to :in " (d/pretty v))))

                       ;; (locking *out* (println "PUB: IN" (d/pretty v)))
                       (async/put! chan> v)
                       (recur))
                     )
            chan>))
    :infinite true
    :expands? true
    }

  )





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; public protocols
;;


;;
;; context implementation is WoofContext protocol
(defprotocol WoofContext

  ;; returns step function by its step-id, or throws an error
  (get-step-fn [this step-id])

  ;; returns step handler metadata as a map
  (get-step-config [this step-id])


  ;; todo: expose the context map?

)


;;
;; executor runs specific workflow
;;
(defprotocol WoofExecutor ; - producer
  "protocol for running workflows"

  ;; starts the workflow, return a channel
  (execute! [this]) ;; TODO: do we need to pass the steps here or in executor constructor?
  ;; todo: pass opts here?


  ;; executes specific step
  (execute-step! [this id step-id params])


  ;; halts workflow
  (end! [this])) ;; should return channel. and receive opts



;;
;; constructor protocol that will know which execturor impl to take
(defprotocol WoofExecutorFactory
  ;; factory method for creating executor
  (build-executor [this steps])) ;; TODO: does it belong here?




;;
;; msg handler from from the workflow - for clean-up and some initial setup
(defprotocol WoofWorkflowMsg
  ;; <?> does this have to be synchronous
  (send-message [this event data]))




;;
;; protocol for processing workflow results
(defprotocol WoofResultProcessor

  ;; processes results received from executor
  (process-results! [this])
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; internal protocols



;; step model contains data about steps and their state
(defprotocol WoofSteps

  (initial-steps [this])

  (steps [this])

  (get-steps* [this])
  (get-steps2add* [this])
  (get-steps-left* [this])


  (do-update-steps! [this added-steps])


  (get-infinite-steps [this])
  (get-infinite-step [this id])
  (set-infinite-step [this id result])


  (steps-ready? [this])

  (add-steps! [this steps])
  (add-pending-steps! [this steps])


  (step-ready! [this id])

  (step-working! [this id])
  (is-step-working? [this id])

  ;; tells whether there are infinite steps
  ;; if so - the producing will be syncronized as order of producing is important
  ;; if no - a new go block will be spawn for producing - but for append only order is not important
  (has-infinite? [this])
)



;;
;; todo: refactor WoofState into model and behaviour
(defprotocol WoofState

  (get-initial-steps [this])
  (get-steps [this])
  (get-steps2add [this])
  (get-steps-left [this])
  (get-results [this])

  (do-commit! [this id step-id result])
  (do-update! [this msg])
  (do-update-sync! [this msg])

  (do-expand! [this id actions])

  (commit! [this id step-id params result])

  (expand! [this id step-id params result])

  (update-steps! [this added-steps])

  (ready? [this])

  (sync? [this])

  (get! [this id])
  (get!* [this id-list])

  ;;

)




;;
;; consumer - producer for step results
(defprotocol WoofStepProcessor ;; "produces" messages while processing wf

  (process-step! [this step])

  (produce-steps! [this ready-channel process-channel steps])

  (consume-steps! [this ready-channel process-channel inf-process-chan produce-chan])

)



;;
;;
(defprotocol WoofBackPressure

  (start-producing? [this])

  (stop-producing? [this i]))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; internal impl


(defn- sid-list-from-expand-map [expand-map]
  (let [sids (keys expand-map)]

    ;; fixme: handle expand key for client-server communication
    #_(if-let [{expand-key :expand-key} (meta expand-map)] ;; ???
      (list expand-key)
      sids)

    sids
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
        ;; save all the expanded ids

        (swap! *results update-in [id]
             (fn[a b]
               (if (seq? a)
                 (concat a b)
                 b))
             (keys actions))

        (add-steps! STEPS actions)
        (add-pending-steps! STEPS actions)

      )
    )
  )



(defrecord WFState [cfg STEPS]
  WoofDebug

  (dbg! [this k]
        STEPS)


  WoofState

  ;;
  (get-initial-steps [this] (initial-steps STEPS))

  (get-steps [this] (get-steps* STEPS))

  (get-steps2add [this] (get-steps2add* STEPS))

  (get-steps-left [this] (get-steps-left* STEPS))

  (get-results [this] (get this :RESULTS))

  (do-commit! [this id step-id result]
              (let [*results (get-results this)
                    step-cfg (get-step-config (get this :CTX) step-id)
                    infinite? (and
                                (:infinite step-cfg)
                                (not (nil? (get @(get this :INF) id))))
                    ]


                #?(:clj  (dosync
                           (swap! *results assoc id result)

                           (if-not infinite?
                             (step-ready! STEPS id))

                           ))
                #?(:cljs (do
                           (swap! *results assoc id result)
                           (if-not infinite?
                             (step-ready! STEPS id))
                           ))
                ))


  (do-update!
    [this msg]
    (let [[id step-id params result] msg
          step-cfg (get-step-config (get this :CTX) step-id)
          infinite? (:infinite step-cfg)]


      (let [d-steps (rest (g/get-dependant-steps (steps STEPS) id))]
        ;;

        (if (not infinite?)
          (do
            (swap! (get-results this) (partial apply dissoc) d-steps)
            (add-pending-steps! STEPS d-steps)
            (do-commit! this id step-id result)

            ;; todo: does this is needed?
            (go
              (put!? (:process-channel cfg) [:steps [:update d-steps]] 1000))
            )
          (do

            ;; (locking *out* (println "clean-in up"))

            ;(do-commit! this id step-id result)
            (swap! (get-results this) (partial apply dissoc) d-steps)
            (add-pending-steps! STEPS d-steps)
            ;(do-commit! this id step-id result)

            (go
              (put!? (:process-channel cfg) [:steps [:update d-steps]] 1000))

            )
          )

        )))

  (do-update-sync!
    [this msg]
    (let [[id step-id params result] msg
          step-cfg (get-step-config (get this :CTX) step-id)
          infinite? (:infinite step-cfg)]


      (let [d-steps (rest (g/get-dependant-steps (steps STEPS) id))]
        ;;

        (if (not infinite?)
          (do
            (swap! (get-results this) (partial apply dissoc) d-steps)
            (add-pending-steps! STEPS d-steps)
            (do-commit! this id step-id result)

            ;; todo: does this is needed?
            (go
              (put!? (:process-channel cfg) [:steps [:update d-steps]] 1000))

            ;(do-update-steps! this d-steps)
            )
          (do

            ;; (locking *out* (println "do-update-sync!" d-steps))

            (if (empty? d-steps)
                (do

                  ;;
                  ; (locking *out* (println msg))
                  ;; update id step

                  )
                (do
                  ;(do-commit! this id step-id result)
                  (swap! (get-results this) (partial apply dissoc) d-steps)
                  (add-pending-steps! STEPS d-steps)
                  ;(do-commit! this id step-id result)
                  ; (do-update-steps! this d-steps)
                  )
              )
            ;; FIXME: what to send in :update
              (go
                  (put!? (:process-channel cfg) [:steps [:update d-steps]] 1000))



            )
          )

        )))



  (update-steps! [this params]
                ; previously it indicated that steps were added
                ; FIXME: check if the value of infinite steps changed


                 #_(locking *out* (println
                                  "\nupdate-steps!: \t"
                                  (d/pretty @(get-steps-left* STEPS))
                                  "\n" (d/pretty (get-infinite-steps STEPS))
                                  ))

                 (do-update-steps! STEPS params))


  (commit!
    [this id step-id params result]

    (if (u/channel? result)
      (let [;*steps-left (get-steps-left this)
            step-cfg (get-step-config (get this :CTX) step-id)
            infinite? (:infinite step-cfg)

             *inf (get this :INF)]


        (if-not (is-step-working? STEPS id)
          (do
            (swap! (get-results this) assoc id result)
            (step-working! STEPS id)

            ;; todo: do we need to close the channel
            ;; todo: stop recurring on workflow stop
            (if infinite?
              (do
                (swap! *inf assoc id :working) ; save that we have infinite action

                (go-loop
                  []
                  (when-let [v (async/<! result)] ;; u/<?
                    (put!? (:infinite-channel cfg) [:save [id step-id params v]] 1000)
                    (recur))))
              (go
                (let [v (async/<! result)] ;; u/<?
                  (put!? (:process-channel cfg) [:save [id step-id params v]] 1000)))))
            )
        )

      (do

        ;; (locking *out* (println "\ndo-commit!:\n" id "\n" result))
        (do-commit! this id step-id result)
        )
      )

    )


  (expand! [this id step-id params actions]
          (let [step-cfg (get-step-config (get this :CTX) step-id)]
           (if (u/channel? actions)
             (let [*steps-left (get-steps-left this)

                   infinite? (:infinite step-cfg)
                   ]

               (when-not (is-step-working? STEPS id)

                 (swap! (get-results this) assoc id actions) ;; store channel
                 ; (swap! *steps-left assoc id :working)
                 (step-working! STEPS id)


                 (if infinite?
                   (do
                     (when-not (get-infinite-step STEPS id)
                       (set-infinite-step STEPS id actions)

                       (go-loop []
                                (when-let [v (async/<! actions)]
                                  (put!? (:infinite-channel cfg) [:expand [id step-id params v]] 1000)
                                  (recur)
                                  )
                                )
                       )

                     )
                   (go ;; normal
                     (let [v (async/<! actions)]
                       ;; TODO: handle if > 1000 actions are added
                       ;; TODO: handle if cycle is being added

                       ;(println (d/pretty [:expand [id step-id params v]]))

                       (put!? (:process-channel cfg) [:expand [id step-id params v]] 1000)))
                   ))
               )
             (let [collect? (:collect? step-cfg)]
               (if (and collect? (not (map? actions)))
                 (do-commit! this id step-id actions)
                 (do-expand! this id actions)
               )))))

    (do-expand! [this id actions]
                ;; FIXME: do we have to add this to linked infinite actions?
              #_(locking *out* (println "do-expand!" id "\n\n" (d/pretty actions)))

              (let [*results (get-results this)]

                #?(:clj (dosync
                  (do-expand-impl! STEPS *results id actions)))

                #?(:cljs (do-expand-impl! STEPS *results id actions))

                ))



  (ready? [this]
          (and
            (steps-ready? STEPS)
            (empty? @(get this :INF)))
          )

  (sync? [this]
         (has-infinite? STEPS))

  (get! [this id]  ; get!
        (u/nil-get @(get-results this) id))

  (get!* [this id-list]
         ;; todo: nils?
         (map (partial get! this) id-list)
         )

)






(defn make-state-cfg
  "stores state needed for executor, like channels and steps."
  [steps process-channel inf-process-chan] ; bs
  {:steps steps
   :process-channel process-channel
   :infinite-channel inf-process-chan}
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
            (if (empty? new-steps)
                (do
                  ;; update
                  ;; (locking *out* (println "update steps:\t" nu-steps))
                  )
                (do
                  (swap! *steps merge new-steps)
                  (reset! *steps2add (array-map))))

            ))



        (get-infinite-steps [this]
            (get-in @*state [:infinite]))

        (get-infinite-step [this id]
            (get-in @*state [:infinite id]))


        (set-infinite-step [this id result]
            #_(locking *out* (println "\tinfinite step" id))

            (swap! *state update-in [:infinite] assoc id result))


        (is-step-working? [this id]
                         (= :working (get @*steps-left id)))

        (steps-ready? [this]
                      (let [ready? (and (empty? (get @*state :infinite {}))
                                        (every? #(= % :ok) (vals @*steps-left)))]
                        ready?))

        (add-steps! [this actions]
          #_(locking *out* (println "\tadd steps" actions))

          (swap! *steps2add into actions))


        (add-pending-steps! [this actions]
          #_(locking *out* (println "\tadd pending steps" actions))

          #?(:clj
              (if (satisfies? clojure.core.protocols/IKVReduce actions)
                (swap! *steps-left merge (reduce-kv (fn [a k v] (assoc a k :pending)) {} actions))
                (swap! *steps-left merge (reduce #(assoc %1 %2 :pending) {} actions)))
              )

          #?(:cljs
              (if (satisfies? cljs.core/IKVReduce actions)
                (swap! *steps-left merge (reduce-kv (fn [a k v] (assoc a k :pending)) {} actions))
                (swap! *steps-left merge (reduce #(assoc %1 %2 :pending) {} actions)))))

        (step-ready! [this id]
                     #_(debug "\tstep ready" id)

                     (swap! *steps-left dissoc id))

        (step-working! [this id]
                    #_(locking *out* (println "\tworking" id))

                      (swap! *steps-left assoc id :working))

        (has-infinite? [this]
                (empty? (get @*state :infinite {})))
      )
    )
  )





(defn- make-state! [context state-config]

  (let [*results (atom (array-map))
        *infinite (atom {})
        steps-model (make-steps-model! (:steps state-config))
        ]

    (assoc
      (->WFState state-config steps-model)

      ; pass the optional params
        :CTX context ;; FIXME: pass context implicit
        :RESULTS *results

        :INF *infinite
      )
    )
)








;; kinda debugger - TODO: maybe a context step of more generic way?
(def ^:dynamic *consumer-debugger* nil)
(def ^:dynamic *producer-debugger* nil)






(defn- freq-map!
  "builds map with number of working and pending steps"
  [steps-left]
  (merge {:working 0} (frequencies steps-left)))

(def freq-map (memoize freq-map!))




(defrecord WFSimpleBackPressure [STATE *bp options]
  WoofBackPressure



  (start-producing?  ;; ! should be called first
    [this]

    (let [freqs (merge {:working 0 :pending 0}
                       (freq-map (vals @(get-steps-left STATE))))
          min-t (get options :notify-ms-min 10)
          max-t (get options :notify-ms-max 10000)
          ]

      (swap! *bp merge {
                         :freqs freqs
                         :current-i 0
                         :timer (u/exp-backoff min-t 2 max-t)
                         })

      (let [b (> (:working-threshold options 20) (:working freqs))]
        ; (if b (println "can-produce? " (d/pretty freqs) (> (:working-threshold options 20) (:working freqs))))
        b
        )))


  (stop-producing?
    [this i]

    (let [CHECK-BACKPRESSURE-ON 20 ;; todo: do we need to checking for bp every time or this should be in produce?
          bi (get @*bp :current-i 0)]

      (if (= bi CHECK-BACKPRESSURE-ON)
        (let [freqs (merge {:working 0 :pending 0}
                       (freq-map (vals @(get-steps-left STATE))))

              max-t (get options :notify-ms-max 4000)
              bp-t (get options :working-ms-threshold 80)

              {prev-freqs :freqs
               timer-fn  :timer} @*bp

              {currently-working :working
               currently-pending :pending} freqs

              {prev-working :working
               prev-pending :pending} prev-freqs]


          (swap! *bp merge { :freqs freqs
                             :current-i 0})

          (if (and
                (> currently-working (:working-threshold options 20))
                (> currently-working 0))

            (let [pending-grows? (> (- currently-pending prev-pending) 0)
                  working-grows? (> (- currently-working prev-working) 0)
                  over-working? (> currently-working (:working-max options 50))

                  wait-time (timer-fn)
                  over-threshold? (>= wait-time bp-t)]


              #_(if (and
                    (= prev-freqs freqs)
                    (= max-t wait-time)
                    (not (= {:working 0, :pending 0} freqs)))
                   (println "wf is stuck!") ;; todo: notifying of backpressure
                )



              (let [stop-produce? (or over-working?
                (cond
                  (and pending-grows?       working-grows?)       true
                  (and pending-grows?       (not working-grows?)) over-threshold?
                  (and (not pending-grows?) working-grows?)       over-threshold?
                  :else false
                  ))]

                stop-produce?
                )
              )
            )
          )
        (do
          (swap! *bp update :current-i inc)
          false)
        )

      )

    )
)




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

      :bp (->WFSimpleBackPressure
            STATE
            (atom {:freqs {:working 0 :pending 0}
                   :current-i 0
                   :bp-time 0
                   })
            {
              :working-threshold 30

              :working-max 50

              :working-ms-threshold 160

              :notify-ms-max 4000
              :notify-ms-min 20

              :CTX CTX
              }
            )
       ::intermed-results (volatile! (array-map))

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
        (cache/memoize! cache (:fn step-cfg) step-id)))))





;; FIXME: remove these
;; step processing function. parametrized by get! and commit! fns.
#_(defn- do-process-step! [get! commit! [id [step-id params]]]
  (let [existing-result (get! id)]
    (println "do-process-step! " [id [step-id params]] existing-result )
    ; #?(:cljs (.warn js/console "PROCESSING: " (pr-str [id params "---" (get! params)]) ) )

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
        (let [collected (get!* wf-state params)]

          (when (not-any? #(or (nil? %1)
                               (u/channel? %1)
                               ) collected)

            ;; handle sid-lists for now differently
            ;; FIXME: recursive check
            (if-let [sidz (filter #(u/sid-list? %1) collected)]
              (do
                (when (not-any? #(or (nil? %1)
                                      (u/channel? %1))
                                (flatten (map #(get!* wf-state %1) sidz)))

                    (store-result! (f (map (fn [k]
                                             (if (sid-list? k)
                                               (get!* wf-state k)
                                               k)) collected)))

                  )
                )
              (store-result! (f collected))
              )
            ))
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


;;
;;


;;
;; default executor
(defrecord AsyncExecutor [context model ready-channel ]
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









;; API


;; executor constructor fn
(defn- executor
  "workflow constuctor"
  [context model ready-channel]
  (let [xctor (->AsyncExecutor context
                               model
                               ready-channel)]


    ;; executor should (may?) also act as context, but proxy the impl to actual context
    (reify
      WoofExecutor

      (execute! [this]   (execute! xctor))

      (end!     [this]   (end! xctor))

      WoofContext

      (get-step-fn [this step-id]
                   (get-step-fn context step-id))

      (get-step-config [this step-id]
                       (get-step-config context step-id))

      WoofDebug

      (dbg! [this k]
            model
            )

      )))




;; how the executor should be instantiated?




;; convenience functions
;; ========================================================

(defn- process-wf-result [v]
  (if (u/exception? v)
    (u/throw! v)
    (if (nil? v)
      (u/throw! "workflow stopped due to timeout!")
      v)))



;; go loop for processing messages from exec-chan by
(defn process-wf-loop
  ([exec-chan op-handler]
   (go-loop []
            (if-let [r (async/<! exec-chan)] ;; [status data] r
              (if (op-handler r)
                (recur)))))

  ([exec-chan op-handler t end-fn]
    (process-wf-loop exec-chan op-handler)
    (go
       (async/<! (u/timeout t))
      ;; todo: check if wf had ended
       (end-fn)
    )))







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

             before-processing! (fn[exec-chan executor])

             ;; redefine this with caution
             process-loop-handler (fn[msg]
                                    (let [[status data] msg
                                          continue? (-> status #{:done :error} nil?)]

                                      #_(let [state (dbg! executor :model)
                                            steps-model (dbg! state :steps)
                                            ]
                                        (debug "RP: " (d/pretty (steps steps-model)))
                                        (debug "2add: " (d/pretty @(get-steps2add* steps-model)))
                                        (debug "left" (d/pretty @(get-steps-left* steps-model)))

                                        (debug "\n")

                                        )




                                      (if-let [status-handler (get op-handlers-map status)]
                                        (status-handler data))

                                      continue?))

             after-processing! (fn[exec-chan] exec-chan)
             }

           } options ]

      (let [exec-chan (execute-fn! executor)
            do-process! (fn[]
                      (if timeout ;; todo: handle stoping via timeout
                        (process-wf-loop exec-chan process-loop-handler timeout (partial end! executor))
                        (process-wf-loop exec-chan process-loop-handler))
                      (after-processing! exec-chan))

            init-chan (before-processing! exec-chan executor)]

        (if-not (u/channel? init-chan)
          (do-process!)
          (do
            (go
              (async/<! init-chan)
              (do-process!))
            exec-chan))
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
            infinite-proc-channel :infinite-channel
            events-map :on
          }
         (merge {
                  :process-channel (u/make-channel)
                  :infinite-channel (u/make-channel)
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
                    (make-state! this (make-state-cfg steps process-channel infinite-proc-channel))
                    (u/make-channel))
                          )

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
                       (if-let [[status data] (async/<! result-chan)]
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


;;
;;


;; will there be a :collect? and :expands? case


;;
;; TODO: to test cached executor, for infinite steps


(defrecord CachedAsyncExecutor [cache context model ready-channel process-channel]
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

