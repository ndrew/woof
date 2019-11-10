(ns woof.state.core
  "woof state mgmt"
  (:require
    #?(:cljs [cljs.pprint :as cljs-pprint])
    #?(:cljs [cljs.reader])

    #?(:clj [clojure.pprint :as clj-pprint])))


;; state factory
;; provides a sub-atom with id for workflow

(defprotocol StateFactory
  (sub-state [this id initial-v]))

(defn state-factory [*state-map]
  (reify StateFactory
    (sub-state [this id initial-v]
      (swap! *state-map assoc id initial-v)
      (rum/cursor-in *state-map [id])
      )))




