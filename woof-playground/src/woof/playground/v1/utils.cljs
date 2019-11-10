(ns ^:figwheel-hooks woof.playground.v1.utils
  (:require
    [rum.core :as rum]

    ;; client core
    [woof.base :as base]
    [woof.wf :as wf]
    [woof.ui :as ui]
    [woof.u :as u]
    [woof.data :as d]
    [woof.utils :as utils]

    [woof.wfc :as wfc
     :refer [WoofWorkflow
             get-params
             get-context-map
             get-steps]]


    ;; core async
    [cljs.core.async :as async]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; pretty print kvs without clutter

(declare dstr)


;; todo: how to pass dynamically a ns to this function?


(def ^:dynamic *curr-ns* nil)

(defn kstr [v]
  (str (if (simple-keyword? v)
         ":"
         (if (= (str (namespace v))
                (if (nil? *curr-ns*)
                  (str (namespace ::this))
                  *curr-ns*)
                )
           "::"
           (str ":" (namespace v) "/") ))
       (name v)))


(defn vstr
  ([v]
   (vstr "" v))
  ([indent v]
   (try
     (cond
       (map? v)
       (str "{\n" (apply str (dstr (str indent "  ") (into (sorted-map) v))) indent "}")

       (satisfies? IAtom v) "<atom>"
       (fn? v) "<fn>"

       (utils/channel? v) "<chan>"

       ; (satisfies? WoofWorkflow v) "<wf>"

       (keyword? v) (kstr v)
       :else (pr-str v)
       )
     (catch js/Error e
       ;(.error js/console e)
       "<err>"
       )))
  )

(defn dstr
  ([kv]
   (dstr "" kv))
  ([indent kv]
   (map (fn [[k v]]
          (str indent
               (vstr k)
               "  " (vstr indent v)
               "\n"))
        kv)
   )

  )