(ns blog.core
  (:require
    [clojure.core.async :as async :refer [go go-loop]]

    [woof.base :as base]
    [woof.data :as d]
    [woof.utils :as u]
    ))


(defn common-ctx-fn [params]
  (let [basic {
               :id       {:fn identity}
               :identity {:fn identity}

               :collect  {:fn       (fn [v] v)
                          :collect? true}
               }
        evt-loop {
                  :infinite-expander { ;; todo: find shorter name :8-xpand, :evt-loop
                                      :fn       (fn [in-chan] in-chan)
                                      :infinite true
                                      :expands? true
                                      }
                  }
        kv {
            :kv        {:fn (fn [[k v]]
                              {k v})}

            :kv*       {:fn       (fn [vs]
                                    (let [pairs (partition 2 vs)]
                                         (reduce (fn [a [k v]]
                                                   (assoc a k v)) {} pairs)
                                         )
                                    )
                        :collect? true}

            :kv-merge* {:fn       (fn [maps]
                                    (apply merge maps))
                        :collect? true}
            }
        kv-zipping {
                    ;; kv zipping - joins keys with values
                    ;; memoize keys
                    :mem-k*  {:fn       (fn [o]
                                          {(base/rand-sid "mem-k-") [:identity {:k o}]})
                              :expands? true}

                    :*kv-zip {:fn       (fn [[[k] vs]]
                                          (let [ks (:k k)]
                                               (apply assoc {} (interleave ks vs))
                                               ))
                              :collect? true}
                    }
        wait {
              :wait-others {
                            :fn       (fn [[v & rest]]
                                        v)
                            :collect? true
                            }
              }
        ]
    (merge
      basic
      kv
      kv-zipping
      evt-loop
      wait)))


(defn debug-ctx-fn [params]
  {
   :prn   {
           :fn       (fn [v]
                       (if (seq? v)
                           (print (count v) ":\t"))
                       (println (d/pretty v))
                       v)
           :collect? true
           }
   :prn-1 {
           :fn (fn [v]
                 (prn v)
                 v
                 )
           }

   :wait {:fn (fn [v]
                (let [chan (async/chan 1)]
                     (go
                       (async/<! (u/timeout 1000))
                       (async/put! chan v))

                     chan))
          }
   :wait-for {
              :fn (fn [[ch-map data]]
                    (async/put! (:chan ch-map) data)
                    data
                    )
              :collect? true
              }

   :save-in-state {
                   :fn (fn [[k v]]
                         (let [state (base/&state params)]
                              (swap! state assoc (:k k) v)
                              )
                         )
                   :collect? true
                   }
   }
  )


