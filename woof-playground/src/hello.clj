(ns hello
  (:require [woof.base :as base]))


;; ensure that base lib is avaialable
(defn -main []
  (println (base/rand-sid))
  )