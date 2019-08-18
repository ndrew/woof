(ns woof.wf
  "woof workflows"
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

            [woof.impl.async-executor :as async-executor]


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

  by def step-handler-fn returns {(rand-sid) (step-body-fn x)}
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
;; executor runs specific workflow
;;
#_(defprotocol WoofExecutor ; - producer
  "protocol for running workflows"

  ;; starts the workflow, return a channel
  (execute! [this]) ;; TODO: do we need to pass the steps here or in executor constructor?
  ;; todo: pass opts here?


  ;; executes specific step
  (execute-step! [this id step-id params])


  ;; halts workflow
  (end! [this])) ;; should return channel. and receive opts



;;
;; constructor protocol that will know which executor impl to take
(defprotocol WoofExecutorFactory
  ;; factory method for creating executor
  (build-executor [this steps])) ;; TODO: does it belong here?





;;
;; protocol for processing workflow results
(defprotocol WoofResultProcessor

  ;; processes results received from executor
  (process-results! [this])
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




;; API


;; executor constructor fn
(defn- executor
  "workflow constuctor"
  [context model ready-channel]
  (let [xctor (async-executor/->AsyncExecutor context
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







;; def impl for a parametrizible result processor via options map
(defrecord ResultProcessor [executor options]
  WoofResultProcessor

  (process-results!
    [this]

    (let [op-handlers-map (get options :op-handlers-map {}) ;; FIXME: add :def handler as in cond

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
                      (if timeout ;; todo: handle stopping via timeout
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


;; TODO: add workflow merging


;; TODO: handle stuck workflows after certain timeout
;; TODO: parametrize backpressure handling
;;          Assert failed: No more than 1024 pending puts are allowed on a single channel. Consider using a windowed buffer.

;; TODO: collect expanded steps results

;; TODO: counting puts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; public interface




(defn default-context-impl [context-map]
  (reify
    WoofContext

    (get-step-fn [this step-id]
      ((get-step-impl context-map) step-id))

    (get-step-config [this step-id]
      (get-step-config-impl context-map step-id))
    )
  )


(defn default-executor-factory-impl
  ([context]
   (default-executor-factory-impl context {}))

  ([context options]
   (let [{
          process-channel       :process-channel
          infinite-proc-channel :infinite-channel
          }
         (merge {
                 :process-channel  (u/make-channel)
                 :infinite-channel (u/make-channel)
                 } options)]
     (reify
       WoofExecutorFactory

       (build-executor [this steps]
         (executor context
                   (make-state! context (make-state-cfg steps process-channel infinite-proc-channel))
                   (u/make-channel))
         )
       )
     )
    )
  )


(defn default-executor
  ([context-map steps]
   (default-executor context-map steps {}))
  ([context-map steps opts]
   (let [context (default-context-impl context-map) ;; wrap context-map to a context model
         ;; create executor factory from context (so we can pass channels to control execution)
         executor-factory (default-executor-factory-impl context opts)]
     ; create an executor instance for steps
     (build-executor executor-factory steps)
     )
    )
  )

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
;; shorthands for common transducers

(def chunk-update-xf u/chunk-update-xf)

(def time-update-xf u/time-update-xf)

(def time-updated-chan u/time-updated-chan)


;;
;;


;; will there be a :collect? and :expands? case

