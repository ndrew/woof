(ns woof.data-test
  (:require
    [clojure.test :refer :all]
    [woof.core :as core]
    [woof.data :as d]
    [clojure.java.io :as io]))




(deftest selector-test
  (is (= (d/selector? (d/selector [:foo]))))

  (binding [*print-meta* true]
    (let [serialized-selector (pr-str (d/selector [:1 :2 :3]))]
      (is (d/selector? (read-string serialized-selector))))
  )
)



(deftest substitution-test
  (let [data {
               :foo :bar
               :foo-placeholder (d/selector [:foo])
               :foo-selector ^{:woof.data/q true} [:foo] ;; internal representation
               :string "sss"
               :vector [0 1 2]
               :seq (drop 3 (range 5))
               :set #{"foo" "bar"}
               :map {:foo :bar} }]
    (is (= {:foo :bar, :foo-placeholder :bar, :foo-selector :bar, :string "sss", :vector [0 1 2], :seq '(3 4), :set #{"foo" "bar"}, :map {:foo :bar}}
           (d/substitute data))))

  (let [nested-data { :nested {:a {:b {:c :boo}}}
                      :nested-selector (d/selector [:nested :a :b :c])}]
    (is (= {:nested {:a {:b {:c :boo}}}, :nested-selector :boo}
           (d/substitute nested-data))))

  (let [vector-data { :v [0 1 2 3 4 5 6]
                      :v-selector (d/selector [:v 5]) }]
    (is (= {:v [0 1 2 3 4 5 6], :v-selector 5}
           (d/substitute vector-data)))
    )

  (let [seq-data { :seq (drop 3 (range 5))
                  :seq-selector (d/selector [:seq 1]) }]
    ;; TODO:
    )

  ;; TODO: add empty selector
  ;; TODO: add map selector

  (let [data { :foo :bar }
        defaults { :baz (d/selector [:foo])}
        ]
    (is (= {:baz :bar}
           (d/substitute data defaults)))
    )
  )




(deftest update-data-test
  (is (= #{"bar"} (d/update-value (atom #{"foo"}) (d/selector ["foo"]) "bar")))

  ;; TODO: add more update data test cases
  ; (println (d/update-value (atom '(1 #{"foo"})) [1 "foo"] "bar"))
  )

(deftest to-primitive-test
  (is (= :foo (d/to-primitive ":foo")))
  )


;  woof.core test

;; DATA tests
(deftest core-data
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

