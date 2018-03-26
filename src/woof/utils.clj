(ns woof.utils
  "helper and utils."
  (:require
    [woof.data :as d]
    [clojure.core.async :as async :refer [go go-loop]]))


;; FIXME: action-id

;; predicates that check parameter in workflow is a link to other action
(defn action-id? [params]
  ;; FIXME: for now, action-id should be a qualified keyword (so we can distinguish it as parameter)
  (qualified-keyword? params))

(defn action-id-list? [keyz]
  (and (coll? keyz)
    (every? action-id? keyz)))


(defn timeout
  [ms]
  (let [c (async/chan)]
    (do ; sync timeout for java
          (Thread/sleep ms)
          (async/close! c))
    c))


(defn channel?
  [x]
  (satisfies? clojure.core.async.impl.protocols/Channel x))


(defn now
  []
  (System/currentTimeMillis))


;; tries to put a payload into channel, if failed - retries after timeout t
;; as a macro - it should work in go block
(defmacro put!? [process-channel payload t]
  `(when-not (async/put! ~process-channel ~payload)
    (async/<! (timeout ~t))
    (async/put! ~process-channel ~payload)))

;; debug macro
(defmacro debug! [c payload]
  `(when (channel? ~c)
        (async/<! ~c)
        (async/put! ~c
                    ~payload)))



;; TODO: do we need this?
(defn make-channel []
  ;; our channel impl
  (async/chan))


(defn exception? [e]
  (instance? Throwable e))

(defn throw! [s]
  (throw
    (if (exception? s) s (Exception. s))))


(defn nil-get [rr id]
  (let [r (get rr id)]
    (if (nil? r)
      (if (contains? rr id) :nil nil)
      r)))

