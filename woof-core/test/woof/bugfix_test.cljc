(ns woof.bugfix-test
  (:require
    [clojure.test :refer [deftest testing is]]

    [woof.base :as base]
    [woof.u :as u]
    ))


;; regression tests for discovered errors


;; composing steps of maps was not working properly
(deftest ^:bug base__as-fn-list__bug

  (let [v (base/as-fn-list [{
                     ::hello [:value "woof"]
                     }])]


    (is (vector? v))
    (let [[f] v]
      (is (= {
              ::hello [:value "woof"]
              } (f {})))
      )))




#?(:clj
   (deftest ^:bug collecting-empty-map-via-collect-step

     (let [wf-impl (base/wf!
                     :ctx {
                           :value   {:fn (fn [v] v)}
                           :collect {:fn       (fn [v] v)
                                     :collect? true
                                     }
                           }

                     :steps [{
                              ::step-returning-empty-map [:value {}]

                              ::collect                  [:collect [::step-returning-empty-map]]
                              }])]

       (let [result @(base/sync-run-wf! wf-impl)]
         ;; resulting {} should not be converted to '()
         (is (= '({}) (::collect result)))
         )
       )
     )
   )


#?(:clj
   (deftest ^:bug kv-zip

     (let [kv-ctx {
                   :mem-k*   {
                              :fn       (fn [o]
                                          {(base/rand-sid "mem-k-") [:identity {:k o}]})
                              :expands? true
                              }

                   ;; kv zipping - joins keys with values
                   :*kv-zip  {
                              :fn       (fn [[[k] vs]]
                                          (let [ks (:k k)]
                                            (apply assoc {} (interleave ks vs))
                                            ))
                              :collect? true
                              }

                   :identity {:fn identity}

                   :collect  {
                              :fn       (fn [xs] xs)
                              :collect? true
                              }
                   }
           wf-impl (base/wf!
                     :ctx [kv-ctx
                           {
                            :process* (base/expand-into :process)
                            :process  {:fn (fn [v] (str "_" v))}
                            }
                           ]

                     :steps [{
                              ;; step returns values
                              ::listings  [:identity [1 2 3]]

                              ;; processes each value via separate step
                              ::listings* [:process* ::listings]

                              ::k         [:mem-k* ::listings*]
                              :result/KV  [:*kv-zip [::k ::listings*]]

                              }])]

       (let [result @(base/sync-run-wf! wf-impl)
             KV (:result/KV result)]
         ;; resulting {} should not be converted to '()

         ;; values are taken from ::listings
         (is (= #{"_1" "_2" "_3"} (into #{} (vals KV))))

         (is (u/sid-list? (keys KV)))
         )
       )
     )
   )
