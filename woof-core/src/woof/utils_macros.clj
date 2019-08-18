(ns woof.utils-macros
  "macros utils."
  ;(:require [cljs.core.async :as async])
  )

;(def put! #'async/put!)

;; tries to put a payload into channel, if failed - retries after timeout t
;; as a macro - it should work in go block
(defmacro put!? [process-channel payload t]
  `(when-not (cljs.core.async/put! ~process-channel ~payload)
    (cljs.core.async/<! (woof.utils/timeout ~t))
    (cljs.core.async/put! ~process-channel ~payload)))




;; debug macro
(defmacro debug! [c payload]
  `(when (satisfies? cljs.core.async.impl.protocols/Channel ~c)
        (cljs.core.async/<! ~c)
        (cljs.core.async/put! ~c
                    ~payload)))


;; single arity fn inline macro
(defmacro inline--fn [the-fn]
  `(~the-fn))


(defmacro inline--fn1 [the-fn v]
  `(~the-fn ~v))
