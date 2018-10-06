(ns woof.utils
  "helper and utils."
  (:require
    [woof.data :as d]
    [woof.u :as base]
    [clojure.core.async :as async :refer [go go-loop]]))


(def sid base/sid)

(def sid? base/sid?)

(def sid-list? base/sid-list?)

(def sid-map base/sid-map )

(def rand-sid base/rand-sid)

(def subsitute-with-rand-sids base/subsitute-with-rand-sids)


(def nil-get base/nil-get)

(defn timeout
  "returns timeout channel"
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




(def wiretap-chan base/wiretap-chan)


;; transducers
;;

(defn chunk-update-xf
  "passes one :process items  "
  [buf-size]
  (fn [rf]
    (let [ctr (volatile! 0)]
      (fn
        ([] (rf)) ; init (arity 0) - should call the init arity on the nested transform xf, which will eventually call out to the transducing process
        ([result] ; completion (arity 1)
         ; why this is never called?
         ; (println "COMPLETE" result)
         (rf result))
        ; Step (arity 2) - this is a standard reduction function but it is expected to call the xf step arity 0 or more times as appropriate in the transducer.
        ; For example, filter will choose (based on the predicate) whether to call xf or not. map will always call it exactly once. cat may call it many times depending on the inputs.
        ([result v]                         ; we ignore the input as
         (let [[status data] v]
           ;;(println "~~~" @ctr "~" status "~~~")
           (if-not (= :process status)
             (rf result v)
             (do
               (vswap! ctr inc)
               (when (= buf-size @ctr)
                 (vreset! ctr 0)
                 (rf result v)
               ))
             )
           ))))))



(defn time-update-xf [interval]
  (fn [rf]
    (let [ctr (volatile! 0)]
      (fn
        ([] (rf))              ; init (arity 0)
        ([result] (rf result)) ; completion (arity 1)
        ; Step (arity 2) - this is a standard reduction function but it is expected to call the xf step arity 0 or more times as appropriate in the transducer.
        ; For example, filter will choose (based on the predicate) whether to call xf or not. map will always call it exactly once. cat may call it many times depending on the inputs.
        ([result v]                         ; we ignore the input as
         (let [[status data] v]
           ;;(println "~~~" @ctr "~" status "~~~")
           (if-not (= :process status)
             (rf result v)
             (when (-> (now) (- @ctr) (> interval))
               (vreset! ctr (now))
               (rf result v)))))))))



(defn time-updated-chan [exec-chan-0 tick-interval]
  (let [exec-chan (async/pipe exec-chan-0
                              (async/chan 1 (time-update-xf tick-interval)))]

    exec-chan))


(defn exp-backoff
  "exponential backoff timeout higher order function"
  [time rate max]
  (let [t (volatile! time)]
    (fn []
      (let [v @t
          nu-t (* v rate)]
        (vreset! t (if (< nu-t max) nu-t max))
        v))))



;; single arity fn inline macro
(defmacro inline--fn [the-fn]
  `(~the-fn))

(defmacro inline--fn1 [the-fn v]
  `(~the-fn ~v))

;;
(defn close-channels!
  "closes channels in arguments array"
  [r]
  (doall
    (doseq [c r]
      (when (channel? c)
        (async/close! c)
        )
    ))
  )

