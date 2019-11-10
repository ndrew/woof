(ns woof.playground.old.example.big-wf
  (:require
    [woof.test-data :as test-data]))


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
