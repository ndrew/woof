(ns woof.core.api
  "woof public protocols")

;; idiotic name

;; protocols that are exposed to user


(defprotocol WoofWorkflow

  (get-params [this])

  (get-context-map [this])

  (get-steps [this])
  )
