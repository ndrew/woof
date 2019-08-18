(ns woof.unit.cache-test
  (:require
    [clojure.test :refer :all]

    [woof.data :as d]
    [woof.utils :as u]

    [clojure.core.async :as async :refer [go go-loop]]

    [woof.cache :as cache]
    ))


;; TODO: go through such cache design

(deftest cache-test
  (let [*cache (atom {})
        cache (cache/->Cache *cache)

        sync-f (fn [params]
                 (println "sync(" params ")")
                 (str "hello " params))

        c (async/chan)
        async-f (fn [params]
                  (println "async(" params ")")
                  (go
                    (u/timeout 1000)
                    (async/put! c (str "ahello " params)))
                  c)
        ]

    ;   id  params v
    (cache/put! cache :foo :bar :baz)
    (is (= :baz (cache/get! cache :foo :bar)))

    ;; memoize called only once
    (is (= "sync( :bar )\n"
           (with-out-str
             ((cache/memoize! cache sync-f :foo) :bar)
             ((cache/memoize! cache sync-f :foo) :bar)
           )
         )
    )

    (comment
      ;; todo: async caching

      #_(let [nf (memoize! cache async-f :async-foo)]
        (println (d/pretty @*cache))
        (println "1" (nf :bar))
        (println (d/pretty @*cache))
        (println "2" (nf :bar))
        (timeout 2000)
        (println "3" (nf :bar)))

      #_(go
          (timeout 2000)
          (println (d/pretty @*cache))
          (println "result!" ((memoize! cache async-f :foo :bar) :bar)))
      )


  )
)

