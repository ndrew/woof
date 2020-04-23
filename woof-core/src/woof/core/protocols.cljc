(ns woof.core.protocols
  "woof protocols"
  ;(:gen-class)
  )


; candidate for removal
(defprotocol WoofDebug
  "quick hacky way of getting insides for debugging purposes"

  (dbg! [this k]
    "processes results received from executor"
    )
  )

(defprotocol WoofContext
  "context implementation is WoofContext protocol"

  (get-step-fn [this step-id]
    "returns step function by its step-id, or throws an error")

  (get-step-config [this step-id]
    "returns step handler metadata as a map")
  )




(defprotocol WoofWorkflowMsg
  "msg handler from from the workflow - for clean-up and some initial setup"

  ;; <?> does this have to be synchronous
  (send-message [this event data]))



(defprotocol WoofSteps
  "step model contains data about steps and their state"

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
  (has-infinite? [this]))


;;
;; todo: refactor WoofState into model and behaviour
(defprotocol WoofState
  "woof workflow state model"

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

  (get-all! [this vs])
  )


(defprotocol WoofStepProcessor
  "consumer - producer for step results.
'produces' messages while processing wf"

  (process-step! [this step])

  (produce-steps! [this ready-channel process-channel steps])

  (consume-steps! [this ready-channel process-channel inf-process-chan produce-chan])
  )


(defprotocol WoofBackPressure

  (start-producing? [this])

  (stop-producing? [this i]))




;;; public protocols

(defprotocol WoofExecutor ; - producer
  "protocol for running workflows"

  (execute! [this]
    "starts the workflow, return a channel")

  ;; can be internal
  (execute-step! [this id step-id params]
    "executes specific step")

  (end! [this]
    "halts workflow. should return channel."))




(comment

  ;; FIXME: to be migrated later

  (defprotocol WoofExecutorFactory
    "constructor protocol that will know which executor impl to take"

    (build-executor [this steps]
      "factory method for creating executor"))


  (defprotocol WoofResultProcessor
    "protocol for processing workflow results"

    (process-results! [this]
      "processes results received from executor"))

  ;; overrides get-steps
  #_(defprotocol WoofWorkflow

      (get-params [this])
      ;; get args?

      (get-context-map [this])

      (get-steps [this])


      )



  )