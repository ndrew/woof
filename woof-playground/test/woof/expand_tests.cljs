(ns woof.expand-tests
  (:require
    [cljs.core.async :as async]
    [cljs.test :refer-macros [use-fixtures deftest is testing async]]

    [woof.base :as base]
    [woof.test-data :as td]
    [woof.utils :as utils]

    [woof.test-base :as tb]
    [woof.data :as d])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))

;;

(defn test-expand-wf [n]

  (let [{ctx :context
         steps :steps

         }
        (binding [td/*new-chan!* (fn []
                                   (.log js/console "new chan")
                                   (async/chan)
                                   )]
          (td/get-test-steps-and-context n)
          )
        ]
    {
     :ctx-fns   [(fn [params] ctx)]
     :steps-fns [(fn [params] steps)]
     }
    )
  )


;; the same test for test exand as in woof.core

(deftest wf-expand-01-test


  (let [N 10
        {
         ctx-fns :ctx-fns
         steps-fns :steps-fns
         } (test-expand-wf N)

        ]
    (tb/test-wf
      []
      ctx-fns
      steps-fns
      [(base/build-opt-on-done (fn[params result]
                                 (is (= (into (sorted-set)
                                              (filter number? (vals   result)))
                                        (into (sorted-set) (range N))))

                                 ))] )

    )
  )


;; todo: channel factory

(defonce NS_NAME (.substr (.replace (str ::test) "/test" "") 1))

(deftest wf-expand-02-test


 (tb/run-simple-wf
    {
     :v      {
              :fn identity
              }
     :expand {:fn       (fn [xs]
                          (into (array-map)
                                (map-indexed (fn [i x]
                                               [(keyword (str NS_NAME "/e" i)) [:v x]]) xs)))
              :expands? true
              }
     :wait   {
              :fn (fn [v]
                    (let [chan (async/chan 1)]
                         (go
                           (async/<! (utils/timeout 30))
                           (async/put! chan v)
                           )

                         chan)
                    )
              }
     }
    {
     ::test        [:v "this is a test"]
     ::wait        [:wait "for testing ui state"]
     ::result      [:v ::wait]

     ::expand-test [:expand ["a" "b" "c"]]
     }
    (fn [result]



      (let [expected-result {::test        "this is a test",
                             ::wait        "for testing ui state",
                             ::expand-test (list ::e0 ::e1 ::e2),
                             ::e0          "a",
                             ::e1          "b",
                             ::e2          "c",
                             ::result      "for testing ui state"}]

           (.log js/console (d/pretty expected-result))
           (.log js/console (d/pretty result))

           (is (= expected-result
                  result
                  ))

           #_(is (= expected-result result)))
      )

    )
  )

