(ns woof.data.core
  (:require [woof.data :as data]))


(defprotocol DATA
  "DATA is a main entity (in general sense) for woof
   Everything that woof works with should be DATA."
  (v [in] "think of this as deref")
  (get-type [in] "human readable name for the type"))


(extend-protocol DATA
  ;; primitive types
  nil
    (v [in] in)
    (get-type [in] :edn)

  boolean
    (v [in] in)
    (get-type [in] :edn)


  ;js/Object
  object
    (v [in] in)
    (get-type [in] :object)


  ; clojure primitive types
  cljs.core.Keyword
    (get-type [in] :edn)
    (v [in] in)

  number
    (get-type [in] :edn)
    (v [in] in)

  string
    (get-type [in] :edn)
    (v [in] in)


  ; collections derived from IPersistentCollection
    cljs.core/PersistentArrayMap

    (get-type [in] (::type (meta in) :edn))
    (v [in]
       (if (map? in)
         (data/substitute in)
        in))

    cljs.core/PersistentVector
      (get-type [in] :edn)
      (v [in] in)

    cljs.core/PersistentHashSet
      (get-type [in] :edn)
      (v [in] in))


