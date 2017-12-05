(ns woof.data
  "woof data processing."
 (:require
   #?(:cljs [cljs.pprint :as cljs-pprint])
   #?(:cljs [cljs.reader])

   #?(:clj [clojure.pprint :as clj-pprint])))


;; placeholders/selectors

;; it should be possible to have declarative quieries in the data, the same way as used in update-in and similar funcs
;; right now the implementation should work for hash maps and other associative structures, like [:query :q1 :q2]

(defn selector
  "creates placeholder query vector. adds meta-data, so it's possible to distinguish from vector"
  [v]
  (with-meta v {::q true}))


(defn selector?
  "predicate that indicates whether v is placeholder"
  [v]
  (::q (meta v)))


(defn substitute
  "substitutes placeholder values in a map. You can also substitute values from other map, usually map with defaults"
    ([f original m k v] ; impl
     (let [nv (if (selector? v)
                (get-in original v)
                (if (map? v)
                  (reduce-kv (partial f f original) {} v)
                  v))]
      (assoc m k nv)))
    ([dict data]                                        ; substitute from separate map
     (reduce-kv
        (partial substitute substitute dict) {} data))
    ([data]                                             ; substitute with itself
     (reduce-kv
       (partial substitute substitute data) {} data)))


(defn pretty
  "pretty print edn as string"
  [data]     ;; TODO: maybe use fipp here
  #?(:cljs
     (with-out-str (cljs-pprint/pprint data)))
  #?(:clj
     (with-out-str (clj-pprint/pprint data))))


(defn to-primitive [s]     ;; FIXME: provide proper implementation
  "converts string to edn"
  (if (some true? ((juxt keyword? number? ) s))
    s
    #?(:cljs
        (let [res (cljs.reader/read-string s)]
          (if (symbol? res)
            (cljs.reader/read-string (str \" (clojure.string/escape s {\" "\\\""}) \"))
            res)))
    #?(:clj
        (let [res (read-string s)]
          (if (symbol? res)
            (read-string (str \" (clojure.string/escape s {\" "\\\""}) \"))
            res)))))



(defn- replace-in-list [coll n x]
  (concat (take n coll) (list x) (nthnext coll (inc n))))


(defn- get-parent-selector [selector]
  (into [] (drop-last selector)))


(defn merge-selectors
  "combines two selectors"
  [parent-selector selector]
  (let [parent parent-selector
        child (if (or (vector? selector) (seq? selector)) selector [selector])]
    (into parent child)))




(defn update-value [data selector new-value]
  (let [edn @data]
    (if (= [] selector)
      (reset! data new-value)

      (let [parent-selector (get-parent-selector selector)
            p (get-in edn parent-selector)] ; TODO: change get-in to work for sequences
        (if (associative? p)
          (swap! data assoc-in selector new-value)
          (cond
            (set? p) (update-value data parent-selector (conj (disj p (get-in edn selector)) new-value))
            (seq? p) (update-value data parent-selector (replace-in-list p (last selector) new-value))

            (nil? p) (do ;; should not happen
                       (println "can't update " (pretty selector) "with" (pretty new-value))
                       @data)))))))





