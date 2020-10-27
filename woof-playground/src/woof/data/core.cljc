(ns woof.data.core
  (:require [woof.data :as data]))




(defn safe-compare [k a b]
  (let [ka (k a)
        kb (k b)]
    (if (or (nil? ka)
            (nil? kb))
      (do
        #?(:cljs (.log js/console  "can't compare" a ka b kb "key-fn" k))
        #?(:clj (println "can't compare" a ka b kb "key-fn" k))
        0)
      (do
        #?(:cljs (.localeCompare ka kb))
        #?(:clj (compare ka kb))
        )
      )
    )
  )

(defn locale-comparator [k & ks]
  (fn [a b]

    (loop [c1 (safe-compare k a b)
           ks ks]
      (if (not= 0 c1)
        c1
        (if-not (seq ks)
          c1
          (let [k (first ks)]
            (recur (safe-compare k a b) (rest ks)))
          )
        )
      )
    )
  )


;;;;;



(defn map-1-1 [source-vector index-map f]
  (reduce (fn [a [k v]]
            (let [vs (map f (vals (select-keys source-vector v)))]
              (if (= 1 (count vs))
                (assoc a k (first vs))
                (do
                  #?(:cljs (.log js/console "non 1-1 mapping" k vs))
                  (assoc a k vs)
                  )

                )
              )

            ) {} index-map)
  )


(defn z-map-1
  ([meta-xf persist-fn f]
   (let [_meta-results (transient (meta-xf)) ;; always use vector for meta log
         add-meta! (fn [input] (meta-xf _meta-results (meta-xf input)))
         ]
     (fn [rf]
       (fn
         ([] (rf))
         ([result]
          (persist-fn (persistent! _meta-results))
          (rf result))
         ([result input]
          (let [nu-v (f input)]
            (add-meta! nu-v)
            (rf result nu-v)
            )
          )
         ([result input & inputs]
          (let [nu-v (apply f input inputs)]
            (add-meta! nu-v)
            (rf result nu-v))
          ))))
   )
  )


(defn juxt-mapper [& fns]
  (fn
    ([] [])
    ([item]
     (let [xf (apply juxt fns)]
       (xf item))
     )
    ([trans-col _metas]
     (let [metas (if (or (seq? _metas) (vector? _metas)) _metas
                                                         [_metas])]
       (apply conj! trans-col (filter some? metas))
       ))
    )
  )

(defn cond-juxt-mapper [cond? & fns]
  (fn
    ([] [])
    ([item]
     (let [xf (apply juxt fns)]
       (xf item))
     )
    ([trans-col _metas]
     (let [metas (if (or (seq? _metas) (vector? _metas)) _metas
                                                         [_metas])]
       (apply conj! trans-col (filter #(and (some? %) (cond? %)) metas))   ;; (comp some? )
       ))
    )
  )