(ns woof.a001-single-arity-fn-composition-test
  (:require
    [clojure.test :refer [deftest testing is]]

    [woof.base :as base]
    ))

;; idea: use only functions **of** **arity 1**

; This allows to use function composition $f(g(x))$ on all such functions (technically).
; This seems pretty limiting, but the hypothesis is that simplicity of composition will overshadow
; the fuss with dealing with multi-arity.
;
; *(technically, multi-arity can be simulated by providing a collection as a single parameter)*


(deftest combining-fns-returning-map-test

  (testing "fn chaining: init-fn composition into zero-arity-fn"

    ;; for init functions we use normal function composition f( g( z(x) ) )

    (let [init-fn1 (fn[_]
                     {::some-specific-key 42})
          init-fn2 (fn[params]
                     {:some-other-ns/specific-key (* (::some-specific-key params) 2)})

          combined-fn (base/combine-init-fns [init-fn1
                                              init-fn2])
          ]
      ;; result is 1-arity function. for convenience there is also a zero-arity fn that will use initial params as {}

      ;; test zero arity version
      (is (=
            {::some-specific-key 42
             :some-other-ns/specific-key 84}
            (combined-fn)
            ))

      ;; test 1 arity also
      (is (=
            {::some-specific-key 42
             :some-other-ns/specific-key 84}
            (combined-fn {})
            ))

      )
    )


  ;; if the single arity fn returns map, these can be combined, by merging only the resulting maps
  ;; each fn receives the same input arg
  (testing "single-arity fn: composing/merging results (if they are maps)"

    ;; combine(fns...)
    ;;    fn-1(arg)=>r1
    ;;    fn-2(arg)=>r2
    ;;    ...
    ;;  => merge(r1, r2, ...)
    (let [ARG {:i-am-what-:will-be-passed :into-each-fn}

          fn-1 (fn [arg]
                 (is (= (:i-am-what-:will-be-passed arg) :into-each-fn))
                 {:boo :hoo
                  :this-will-be-overridden :fn-1
                  })
          fn-2 (fn [arg]
                 (is (= (:i-am-what-:will-be-passed arg) :into-each-fn))
                 {:foo :bar
                  :this-will-be-overridden :fn-2
                  })

          ;; combine fn-1 and fn-2
          combined-fn (base/combine-fns [fn-1
                                         fn-2])]

      ;; ensure results are merged
      (is (= (combined-fn ARG) {:boo :hoo
                                :foo :bar
                                :this-will-be-overridden :fn-2
                                }))

      ;; test for single arity also
      (is (= ((base/combine-fns [fn-1]) ARG)
             {:boo :hoo
              :this-will-be-overridden :fn-1}))
      (is (= ((base/combine-fns [fn-2]) ARG)
             {:foo :bar
              :this-will-be-overridden :fn-2}))
      )
    )

  ;; sometimes we'll need a different merge function, in order to handle key collisions

  (testing "composing opt-fns: different merge-results fn"
    ;; opts-fn return nested map, that should be merged differently
    (let [
          PARAMS-MAP {:iam :params-map}

          opt-fn-1 (base/build-opt-on-done (fn [params result]
                                             ;;
                                             (is (= PARAMS-MAP params))
                                             (assoc result :opt-fn-1 true)
                                             ))

          opt-fn-2 (base/build-opt-on-done (fn [params result]
                                             (is (= PARAMS-MAP params))

                                             (is (:opt-fn-1 result))
                                             (assoc result :opt-fn-2 true)
                                             ))

          opt-fn-3 (base/build-opt-on-done (fn [params result]
                                             (is (= PARAMS-MAP params))

                                             (is (:opt-fn-1 result))
                                             (is (:opt-fn-2 result))

                                             (assoc result :opt-fn-3 true)
                                             ))

          combined-opts-fn (base/combine-fns [opt-fn-1
                                              opt-fn-2
                                              opt-fn-3
                                              ] :merge-results base/merge-opts-maps)
          ]

      (let [opts-map (combined-opts-fn {:iam :params-map})
            on-done (get-in opts-map [:op-handlers-map :done])]

        (is (= {:iam      :a-result
                :opt-fn-1 true
                :opt-fn-2 true
                :opt-fn-3 true} (on-done {:iam :a-result})))

        ;; todo: :before-process merging
        ;; todo: :after-process merging


        )

      )


    )

  #_(base/combine-fns
      [(base/arg-fn fs-process-opts) fs-xtor-opts]
      :merge-results base/merge-opts-maps)

  ;(is (= false true))
  )


(deftest combing-keys-fns-to-single-arity

  ;; one of the initial woof ideas was to use var-args fns, like
  ;; (fn [& {:keys [foo bar]}] ... )
  ;; and convert them automatically into single arity ones iva arg-fn

  (let [var-args-fn (fn [& {:keys [foo bar] :as params}]
                     {::FOO foo
                      ::BAR bar}
                     )
        arity-1-fn (base/arg-fn var-args-fn)
        ]

    (is (= {::FOO "foo" ::BAR "bar"}
           (var-args-fn :foo "foo" :bar "bar")))

    (is (= {::FOO "foo" ::BAR "bar"}
            (arity-1-fn { ;; note, that we are using hash-map here
                         :foo "foo"
                         :bar "bar"
                         })))
    )
  )



(deftest combining-with-meta

  (let [fn-1 (fn [params]
               (with-meta {:fn-1 :1}
                          {:meta-1 :meta})
               )
        fn-2 (fn [params]
               {:fn-2 :2})


        combined-fn-1 (base/combine-fns [fn-1]
                                        :merge-results (fn [a b]
                                                         (let [m-1 (meta a)
                                                               m-2 (meta b)]
                                                              (with-meta
                                                                (merge a b)
                                                                (merge {} m-1 m-2)
                                                                )
                                                              )

                                                         ))
        combined-fn-2 (base/combine-fns [fn-1 fn-2])
        ]

    (let [r-1 (combined-fn-1 {})
          r-2 (combined-fn-2 {})
          ]

      (is (nil? (meta r-2)))
      (is (= {:meta-1 :meta} (meta r-1)))
      ;(.log js/console (meta r-1))
      ;(.log js/console (meta r-2))
      )
    )



  )