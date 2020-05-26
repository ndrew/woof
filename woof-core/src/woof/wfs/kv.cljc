(ns woof.wfs.kv
  (:require
    [woof.base :as base]))


;; workflow steps for working with maps

(defonce KV-CTX-MAP {
                ;; make a map from 2 values k and v
                  :kv        {:fn (fn [[k v]]
                                    {k v})}

                ;; build
                  :kv*       {:fn       (fn [vs]
                                          ;; todo: check that vs count is even
                                          (let [pairs (partition 2 vs)]
                                            (reduce (fn [a [k v]]
                                                      (assoc a k v)) {} pairs)
                                            )
                                          )
                              :collect? true}

               ;; merge maps
                  :kv-merge* {:fn       (fn [maps]
                                          (apply merge maps))
                              :collect? true}


              ;; kv zipping - joins keys with values

                  ;; as we need to save qualified keyword, we need to use a map for that
                  ;; <!> note about :identity
                  :mem-k*    {:fn       (fn [o]
                                          {(base/rand-sid "mem-k-") [:identity {:k o}]})
                              :expands? true}

                  :*kv-zip   {:fn       (fn [[[k] vs]]
                                          (let [_ks (:k k)
                                                ks (if (vector? _ks) _ks (vec _ks))
                                                ]
                                            (apply assoc {} (interleave ks vs))
                                            ))
                              :collect? true}

                  })

(defn ctx-fn [params]
  KV-CTX-MAP)

