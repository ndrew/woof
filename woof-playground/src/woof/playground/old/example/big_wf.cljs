(ns woof.playground.old.example.big-wf
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]

    [woof.data :as d]
    [woof.wf :as wf]

    [woof.utils :as u]
    [woof.ui :as ui]

    [woof.playground.old.wf-ui :as wf-ui]

    [woof.playground.old.results :as r]
    [woof.test-data :as test-data]


    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


(defonce TEST-WF-STEP-COUNT 300)




(defn prepare-params! []
  (test-data/get-test-steps-and-context TEST-WF-STEP-COUNT)
  )

;; factory for channels


(defn context-map-fn [& {:keys [context]}]
  context
  )


(defn steps-fn [& {:keys [steps]}]
  steps
  )


(defn actions-fn [& {:keys []}]
    {
     ;; :start! (fn[])
     :stop!  (fn[])
     ;; :reset! (fn[])

     :actions [

      ]
  })
