(ns woof.unit.data-test
  (:require
    [clojure.test :refer :all]
    ;[woof.core :as core]
    [woof.data :as d]
    [clojure.java.io :as io]))




(deftest to-primitive-test
  (is (= :foo (d/to-primitive ":foo")))
  )

