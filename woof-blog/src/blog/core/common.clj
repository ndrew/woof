(ns blog.core.common
  (:require
    [woof.base :as base]

    [woof.wfs.kv :as kv]
    [woof.wfs.evt-loop :as evt-loop]
    ))


(defn common-ctx-fn [params]
  (let [
        wait {
              :wait-others {
                            :fn       (fn [[v & rest]]
                                        v)
                            :collect? true
                            }

              }


        ]
    (merge
      base/BASE-CTX-MAP
      kv/KV-CTX-MAP
      evt-loop/EVT-LOOP-CTX-MAP
      wait)))


(defn parse-int [number-string]
  (try (Integer/parseInt number-string)
       (catch Exception e nil)))
