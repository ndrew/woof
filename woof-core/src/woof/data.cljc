(ns woof.data
  "woof data processing functions"
 (:require
   #?(:cljs [cljs.pprint :as cljs-pprint])
   #?(:cljs [cljs.reader])

   #?(:clj [clojure.pprint :as clj-pprint]))
  ;(:gen-class)
  )

;; todo: remove not used stuff


;; todo: this can trigger error for atoms and other nested structures
(defn pretty!
  "pretty print edn as string"
  [data]     ;; TODO: maybe use fipp here
  #?(:cljs
     (with-out-str (cljs-pprint/pprint data)))
  #?(:clj
     (with-out-str (clj-pprint/pprint data))))


(def pretty (memoize pretty!))


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

