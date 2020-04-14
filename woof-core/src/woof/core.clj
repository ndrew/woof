(ns woof.core
  (:require
    [woof.data :as data])
  (:gen-class)
  )

;; DATA
;;

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


  java.lang.Object
    (v [in] in)
    (get-type [in] :object)

  java.lang.Long
    (get-type [in] :edn)
    (v [in] in)

  java.lang.Double
    (get-type [in] :edn)
    (v [in] in)

  ; strings
  java.lang.String
    (get-type [in] :edn)
    (v [in] in)



  ; clojure primitive types
  clojure.lang.Keyword
    (get-type [in] :edn)
    (v [in] in)


  ; collections derived from IPersistentCollection
  clojure.lang.IPersistentCollection
    (get-type [in] (::type (meta in) :edn))
    (v [in]
       (if (map? in)
         (data/substitute in)
        in)))


