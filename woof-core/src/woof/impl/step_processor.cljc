(ns woof.impl.step-processor
  "woof step processor impl"
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

;; depends on woof executor
