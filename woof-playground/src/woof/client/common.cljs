(ns woof.client.common
  (:require
    [cljs.core.async :refer [go] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]

    [woof.base :as base]
    [woof.client.dom :as woof-dom]
    [woof.client.dbg :as dbg :refer [__log]]
    [woof.data :as d]
    [woof.utils :as u]))


;; common step handlers
(defn common-ctx [params]
  {
   :log     {:fn (fn[v] (.log js/console v) true)}
   :warn     {:fn (fn[v] (.warn js/console v) true)}

   :&log     {:fn (fn[v] (.log js/console v) true)
              :collect? true
              }

   :prn     {:fn (fn[v] (.log js/console (d/pretty! v)) "")}
   :*prn     {:fn (fn[v] (.log js/console (d/pretty! v)) "")
              :collect? true
              }
   :prn-seq{:fn (fn[vs]
                  (doseq [v vs]
                    (.log js/console (d/pretty! v))
                    )
                  "")
            :collect? true
            }

   :wait-rest      {
                    :fn       (fn [[v & rest]] v)
                    :collect? true
                    }

   :wait-steps {
                :fn (fn [[steps & rest]]
                      steps
                      )
                :collect? true
                :expands? true
                }

   ;;
   ;; mem
   :mem-k*             {
                        :fn       (fn [o]
                                    {(base/rand-sid "mem-k-") [:identity {:k o}]})
                        :expands? true
                        }

   :mem-zip {:fn       (fn [xs]
                         (partition-all (count xs)
                                        (apply interleave
                                               (reduce (fn [col [a]]
                                                         (conj col (:k a)))
                                                       [] xs))))
             :collect? true
             }

   :mem-zip* {
              :fn       (fn [xs]

                          ;; todo: what if sub-sid-list are of different length
                          (let [grouped-sids (partition-all (count xs)
                                                            (apply interleave
                                                                   (reduce (fn [col [a]]
                                                                             (conj col (:k a)))
                                                                           [] xs)))]
                            (reduce
                              (fn [a x]
                                (assoc a (base/rand-sid "mem-zip-") [:collect x])
                                )
                              {} grouped-sids)
                            )

                          )
              :expands? true
              :collect? true
              }

   ;; kv zipping - joins keys with values
   :*kv-zip            {
                        :fn       (fn [[[k] vs]]
                                    (let [ks (:k k)]
                                      (apply assoc {} (interleave ks vs))
                                      ))
                        :collect? true
                        }

   :identity {:fn identity }
   :identity*    (base/expand-into :identity)
   :v {:fn identity }
   :v-8 {:fn identity
         :infinite true
         }
   :v* (base/expand-into :v)



   :collect  {
              :fn       (fn [xs]
                          ; (.warn js/console xs)
                          xs)
              :collect? true
              }


   :save-to-state {
                   :fn (fn [[k v]]
                         (let [*state (base/&state params)]
                           (swap! *state assoc k v)
                           ))
                   :collect? true
                   }

   :tick {:fn       (fn [[t max-num]]
                      (let [chan-factory (base/&chan-factory params)
                            chan (base/make-chan chan-factory (base/rand-sid))]

                        (async/go-loop [i 0]
                          (async/>! chan (u/now))
                          (async/<! (u/timeout t))

                          (if (< i max-num)
                            (recur (inc i))))

                        chan))
          :infinite true}

   }
  )
