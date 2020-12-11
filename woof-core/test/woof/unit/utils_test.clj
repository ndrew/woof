(ns woof.unit.utils-test
  (:require
    [clojure.test :refer [deftest testing is]]

    [woof.base :as base]
    [woof.u :as u]
    ))


(deftest utils__sid-list__tests

  ;; correctly determine if the collection is a sid list

  (is (= false (u/sid-list? {})))
  (is (= false (u/sid-list? #{})))

  (is (= true (u/sid-list? '())))
  (is (= true (u/sid-list? [])))

  (is (= true (u/sid-list? '(::foo))))
  (is (= true (u/sid-list? [::bar ::baz])))

  (is (= false (u/sid-list? [::bar ::baz [] ])))
  (is (= false (u/sid-list? [::bar {} ::baz ])))
  )


