(ns woof.data.core
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




;; DATA tests
#_(deftest core-data
         (let [test-collection #^{::type :specific-edn} [1]]

           (is (= :edn (core/get-type nil)))
           (is (= :edn (core/get-type :azaza)))
           (is (= :edn (core/get-type "")))
           (is (= :edn (core/get-type 1)))
           (is (= :edn (core/get-type 1.0)))
           (is (= :edn (core/get-type []))) ;; collections could have

           (is (= :edn (core/get-type test-collection)))
           (is (= :edn (core/get-type {})))
           (is (= :object (core/get-type (new java.util.Date))))

           ;;  (core/get-type (js/Date.))

           ;; v should return identity for primitive types
           (is (= [] (core/v [])))
           (is (= nil (core/v nil)))
           (is (= 1 (core/v 1)))
           (is (= 1.0 (core/v 1.0)))
           (is (= :foo (core/v :foo)))
           (is (= "" (core/v "")))

           ; todo: does placeholder is needed anymore?
           ;  (core/v {:foo :bar
           ;           :p (d/placeholder [:foo])})
           )

         )

