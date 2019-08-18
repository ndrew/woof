(ns woof.test-runner
  (:require
    [cljs-test-display.core]
    [cljs.test :refer-macros [run-tests]]
    ;; require all the namespaces that have tests in them
;    [example.core-test]
    ;[example.other-test]
    ))

#_(run-tests (cljs-test-display.core/init! "app-testing")
           'example.core-test
           ;'example.other-test
           )