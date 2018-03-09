(ns woof.wf-data
  (:require [woof.data :as d]
            [woof.cache :as cache]
            [woof.graph :as g]


            #?(:clj [woof.utils :as u :refer [put!? debug!]])
            #?(:cljs [woof.utils :as u])


            #?(:clj [clojure.core.async :as async :refer [go go-loop]])
            #?(:cljs [cljs.core.async :as async])
            )

  #?(:cljs
      (:require-macros
          [cljs.core.async.macros :refer [go go-loop]]
          [woof.utils-macros :refer [put!? debug!]]
        )))




(defn- extract-result [result k]
  (let [r (get result k)]
      (if (u/action-id-list? r)
        (mapcat (fn [a]
                 (let [u (extract-result result a)]
                   (if (seq? u) u [u]))) r)
        r)))


(defn extract-results
  "get workflow results, but for certain keys"
  [result keyz]
  (into (array-map)
        (map (fn[k]
               [k (extract-result result k)])
             keyz)))


(defn inline-results
  "substitues expanded items by it's values and removes them from map"
  [results]

  (let [new-results (reduce
                      (fn [a [k v]]
                        (if (u/action-id-list? v)
                          (do
                            (let [nu-vals (reduce (fn [a k]
                                                    (assoc a k (get results k))
                                                    ) (::tmp a) v)]

                              (assoc a
                                k (map (fn[id] (get nu-vals id)) v)
                                ::tmp nu-vals)
                              )
                            )
                          (assoc a k v))
                        ) {::tmp {} } results)]

    (let [{tmp ::tmp} new-results]
      (apply dissoc new-results (conj (keys tmp) ::tmp))
      )
    )
  )

