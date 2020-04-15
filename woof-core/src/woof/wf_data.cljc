(ns woof.wf-data
  (:require #?(:clj [woof.utils :as u :refer [put!? debug!]])
            #?(:cljs [woof.utils :as u])


            #?(:clj [clojure.core.async :as async :refer [go go-loop]])
            #?(:cljs [cljs.core.async :as async])
            )

  #?(:cljs
      (:require-macros
          [cljs.core.async.macros :refer [go go-loop]]
          [woof.utils-macros :refer [put!? debug!]]
        ))
  ;(:gen-class)
  )



;; todo: migrate to base


(defn extract-result
  "gets the final values from workflow result for specified keyz"

  ([result k]
   (extract-result (fn [all-keys v] v) result [] k))

  ([f result all-keys k]
   (let [r (get result k)]
     (if (u/sid-list? r)
       (let [nu-keys (conj all-keys k)]
         (mapcat (fn [a]
                 (let [u (extract-result f result nu-keys a)]
                   (if (seq? u)
                     u [u]))) r)
         )

       ;; todo: maybe pass here the parent key also, or smth like [:parent-key :key]
       (f
         (conj all-keys k) r)))))




(defn extract-results
  "get workflow results, but for certain keys"
  ([result keyz f]
    (into (array-map)
        (map (fn[k]
               [k (extract-result f result [] k)])
             keyz)))

  ([result keyz]
    (extract-results result keyz (fn [all-keys v] v))))


;; todo: recursive extract results




(defn inline-results
  "substitues expanded items and removes intermediary values from the result map"
  [results]

  (let [new-results (reduce
                      (fn [a [k v]]
                        (if (u/sid-list? v)

                          (let [nu-vals (reduce (fn [a k]
                                                  (assoc a k (get results k))
                                                  ) (::tmp a) v)]

                            (assoc a
                              k (map (fn[id] (get nu-vals id)) v)
                              ::tmp nu-vals)
                            )

                          (assoc a k v))
                        ) {::tmp {} } results)]

    (let [{tmp ::tmp} new-results]
      (apply dissoc new-results (conj (keys tmp) ::tmp))
      )))

