(ns woof.utils
  "helper and utils."
  (:require
    [woof.data :as d]
    [cljs.core.async :as async])
  (:require-macros [cljs.core.async.macros :refer (go go-loop)]))


;; predicates that check parameter in workflow is a link to other action
(defn action-id? [params]
  ;; FIXME: for now, action-id should be a qualified keyword (so we can distinguish it as parameter)
  (qualified-keyword? params))

(defn action-id-list? [keyz]
  (every? action-id? keyz))



(defn timeout
  [ms]
  (let [c (async/chan)]
    (js/setTimeout (fn [] (async/close! c)) ms)
  c))


(defn channel?
  [x]
  (satisfies? cljs.core.async.impl.protocols/Channel x))


(defn now
  []
  (.getTime (js/Date.)))



(defn make-channel []
  ;; our channel impl
  (async/chan))


(defn throw! [s]
  (throw
    (js/Error. s)))



