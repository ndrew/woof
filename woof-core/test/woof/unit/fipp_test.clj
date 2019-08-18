(ns woof.unit.fipp-test
  (:require
    [clojure.test :refer :all]

    [woof.data :as d]

    [woof.fipp :refer [prepare-edn] :as fipp]
    ))


;; fipp provides a way of representing edn data in more ui friendly format

;; todo: maybe use REBL ?

(deftest fipp-test

  (is (= [:node :string "data"]) (prepare-edn "data"))


  (is (= [:group :vector
          '([:node :number 1]
            [:separator [:separator :line]]
            [:node :number 2]
            [:separator [:separator :line]]
            [:node :number 3])
            ]
         (prepare-edn [1 2 3])

         ))


  )
