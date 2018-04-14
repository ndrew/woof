(ns woof.utils
  "helper and utils."
  (:require
    [woof.data :as d]
    [cljs.core.async :as async])
  (:require-macros [cljs.core.async.macros :refer (go go-loop)]))


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


(defn exception? [e]
  (instance? js/Error e))

(defn throw! [s]
  (throw
    (if (exception? s) s (js/Error. s))))


(defn nil-get [rr id]
  (let [r (get rr id)]
    (if (nil? r)
      (if (contains? rr id) :nil nil)
      r))
  )







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
