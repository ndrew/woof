(ns woof.fipp
  "Use fipp pretty printer to produce the hiccup-style tree of the edn data.
  Primitive types are represented as [:node TYPE VALUE]
  Composite types are represented as [:group TYPE VALUE]
  "
    #?(:clj  (:refer-clojure :exclude [boolean?])
       :cljs (:refer-clojure :exclude [boolean? char?]))

  (:require
    [fipp.edn :refer [pprint pretty-coll]  :rename {pprint fipp}]
    [fipp.clojure :as fippc]
    [fipp.visit :refer [visit visit*]]
    [fipp.engine :refer (pprint-document)]
    [fipp.ednize :refer [edn record->tagged override?]]
))


;; TODO: use print-level as limiter
(defn coll [{:keys [group separator print-level] :as printer}
             t xs sep f]
    (let [
           ppp (cond-> printer print-level (update :print-level dec))

        xform (comp ;(if print-length (take print-length) identity)
                    (map #(f printer %))
                    (interpose (separator sep)))
        ys ;(if (pos? (or print-level 1))
             (sequence xform xs)
            ; "#")
           ]
      (group t ys)
  ))


;;
;; EdnPrinter is
(defrecord EdnPrinter [node group separator
                       selector
                       symbols print-meta print-length print-level]
  fipp.visit/IVisitor

  (visit-unknown [this x]
    (visit this (edn x)))

  (visit-nil [this]
    (node :nil nil))

  (visit-boolean [this x]
    (node :boolean x))

  (visit-string [this x]
    (node :string x))

  (visit-character [this x]
    (node :character x))

  (visit-symbol [this x]
    (node :symbol x))

  (visit-keyword [this x]
    (node :keyword x))

  (visit-number [this x]
    (node :number x))

  (visit-seq [this x]
    (if-let [pretty (symbols (first x))]
      (pretty this x) ; (pretty-coll this "(" x :line ")" visit)
      (coll this :seq x :line visit)
      ))

  (visit-vector [this x]
    (coll this :vector x [:separator :line] visit)) ;(pretty-coll this "[" x :line "]" visit)

  (visit-map [this x] ;
      (coll this :map x [:separator :line] (fn [printer [k v]]
        [:kv (visit this k) (visit this v)]))
    )



  (visit-set [this x]
    ;(pretty-coll this "#{" x :line "}" visit)
    (coll this :set x [:separator :line] visit))



  (visit-tagged [this {:keys [tag form]}] ; TODO:
    #_[:group "#" (pr-str tag)
            ;(when (or (and print-meta (meta form))
            ;          (not (coll? form)))
            ;  " ")
            (visit this form)]
    [:group :object (pr-str tag)
            (when (or (and print-meta (meta form))
                      (not (coll? form)))
              " ")
            (visit this form)]
                )


  (visit-meta [this m x] ; TODO:
    (if print-meta
      ;[:align [:span "^" (visit this m)] :line (visit* this x)]
      [:meta (visit this m) [:separator :line] (visit* this x)]
      (visit* this x)))

  (visit-var [this x]
    (node :var x))

  (visit-pattern [this x]
    (node :pattern x))

  (visit-record [this x]
    (visit this (record->tagged x)))
)


(defn visit-data [node-fn group-fn separator-fn data]
  (let [printer (EdnPrinter.
                  node-fn group-fn separator-fn
                  []
                  {} false nil nil)]
    (visit printer data)
    )
  )


;; primitive types
;; [:node TYPE value]

;; composite types

;; vector
;; [:group TYPE v]
;; empty [:group :vector ()]
;; 1 [:group :vector ([:node TYPE v])]
;; 2 [:group :vector ([:node TYPE v] [:separator :line] [:noe TYPE v])]



#_(let [printer (EdnPrinter.
                  (fn [t v] [:node t v]) ; node fn
                  (fn [t v] [:group t v]) ; group fn
                  (fn [t] [:separator t]) ; separator fn
                  [] ; selector ?
                  {} ; symbols ?
                  true ; false ; print-meta
                  nil ; print-lenght
                  nil) ; print-level
      ;data :foo

      ;data #{:set :of :shit}

      ; data {:foo :bar :baz :buzz :v []}

      ; data [:foo :bar :baz]

      ; data '(:foo :bar)
      ; data '()
      ; data (range 10)

      ;data (with-meta [] {::q true})

     ; TODO: tagged
      data (java.util.Date.)
      ]

      (visit printer data)
)




(defn prepare-edn [data]
   (let [printer (EdnPrinter.
                  (fn [t v] [:node t v])
                  (fn [t v] [:group t v])
                  (fn [t] [:separator t])
                  []
                  {} true nil nil)]
     (visit printer data)))





