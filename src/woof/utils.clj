(ns woof.utils
  "helper and utils."
  (:require
    [woof.data :as d]
    [clojure.core.async :as async :refer [go go-loop]]))


;; fixme: migrate to woof.wf-core

(defn sid
  "generates a particular id for a step — sid"
  ([id]
   (keyword (str *ns*
                 "/"
                 (if (keyword? id) (name id) id))))
  ([prefix id]
   (sid (str prefix
             (if (keyword? id) (name id) id)))))

;; predicates that check parameter in workflow is a link to other action
(defn sid?
  "checks if id is sid: it should be qualified keyword. So we can distinguish it as parameter"
  [id]
  (qualified-keyword? id))


(defn sid-list?
  "checkis if sids is a collection where all items are sid"
  [sids]
  (and (coll? sids) (every? sid? sids)))


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

