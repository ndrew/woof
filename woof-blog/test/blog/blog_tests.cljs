(ns blog.blog-tests
  (:require
    [cljs.core.async :as async]
    [cljs.test :refer-macros [use-fixtures deftest is testing async]]

    [woof.base :as base]
    ; [woof.test-base :as tb]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


(deftest sample-test

  (is (= true false))

  )