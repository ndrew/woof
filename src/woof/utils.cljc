(ns woof.utils
  "helper and utils."
  (:require
    [woof.data :as d]
    [woof.pipeline :as P]

    #?(:clj  [clojure.core.async :as async :refer [go go-loop]])
    #?(:cljs [cljs.core.async :as async]))

  #?(:cljs
      (:require-macros [cljs.core.async.macros :refer (go go-loop)])))


(defn timeout
  [ms]
  (let [c (async/chan)]
    #?(:clj
        (do ; sync timeout for java
          (Thread/sleep ms)
          (async/close! c))
      :cljs
        (js/setTimeout (fn [] (async/close! c)) ms))
    c))


(defn channel?
  [x]
  #?(:clj
      (satisfies? clojure.core.async.impl.protocols/Channel x)
    :cljs
      (satisfies? cljs.core.async.impl.protocols/Channel x)))


(defn now
  []
  #?(:clj (System/currentTimeMillis)
     :cljs (.getTime (js/Date.))))


(defn make-channel []
  ;; our channel impl
  (async/chan))


(comment
;; TODO: do the following functions are needed?

  (defn throw-err [e]
    (when (instance?
            #?(:clj Throwable)
            #?(:cljs js/Error)
            e) (throw e))
    e)


  ;; FIXME:
  (defmacro <? [ch]
    #?(:clj `(throw-err (clojure.core.async/<! ~ch)))
    ;#?(:cljs `(throw-err (cljs.core.async/<! ~ch)))
    ;#?(:cljs (cljs.core.async/<! ch))

      #?(:cljs (println "<?")))


  ;; FIXME:
  (defmacro >? [ch v]
    #?(:clj `(throw-err (clojure.core.async/>! ~ch ~v)))
   ; #?(:cljs `(throw-err (cljs.core.async/>! ~ch ~v)))

      #?(:cljs (println ">?")))


  ;; FIXME:
  (defmacro put!? [ch v]
    #?(:clj `(throw-err (clojure.core.async/put! ~ch ~v)))
  ;  #?(:cljs `(cljs.core.async/put! ~ch ~v))
    #?(:cljs (println "put!?")))

)
