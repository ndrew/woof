(ns woof.client.playground.streets.ds
  (:require
    [cljs.core.async :as async]
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]

    [goog.dom :as dom]
    [goog.dom.classes :as classes]
    [goog.Uri :as uri]

    [rum.core :as rum]

    [woof.base :as base]
    [woof.data :as d]
    [woof.utils :as utils]

    [woof.browser :as woof-browser]
    [woof.client.dom :as wdom]

    [woof.client.dom :as woof-dom]
    [woof.client.playground.ui :as pg-ui]
    [woof.client.playground.ui.wf :as wf-ui]
    [woof.client.ws :as ws]

    [woof.wfs.evt-loop :as evt-loop]
    [clj-fuzzy.metrics :as metrics]

    [clojure.core.reducers :as r]
    [clojure.set :as set])

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))




(defonce
  DISTRICTS-DEFS
  [
   [:fs :kga :drv]
   ["darnytskyi"]
   ["desnianskyi"]
   ["dniprovskyi"]
   ["holosiivskyi"]
   ["obolonskyi"]
   ["pecherskyi"]
   ["podilskyi"       "Подільський"]
   ["shevchenkivskyi" "Шевченківський"]
   ["solomianskyi"]
   ["sviatoshynskyi"]
   ])


;; kga->drv


(defn kga-house-n-alts [h-ks] ;; set of house number
  (reduce (fn [a k]
      (assoc a
        (str/upper-case k) k
        (str/replace k "-" "/") k
        (str/replace k "-" "") k
        (str/replace k "/" "-") k
        (str/replace k "/" "") k
        (first (str/split k "/")) k
        (first (str/split k "-")) k
        )
      )
    {} h-ks)
  )


(defn drv-house-n-alts [kga-mapping drv-diff]
  (reduce
    (fn [a k]
      (cond
        (get kga-mapping (str/replace k #"[А-Я]" ""))
        (assoc a k (str/replace k #"[А-Я]" "") )

        (get kga-mapping (first (str/split k "/")))
        (assoc a k (first (str/split k "/")))

        (get kga-mapping (str/replace (first (str/split k "/")) #"[А-Я]" ""))
        (assoc a k (str/replace (first (str/split k "/")) #"[А-Я]" ""))

        :else a
        )
      )
    {} drv-diff)
  )

(defn enrich-kga-houses [kga-hs drv-hs]
  (let [kga-ks (into #{} (keys kga-hs))
        drv-ks (into #{} (keys drv-hs))

        drv-diff (set/difference drv-ks kga-ks)

        h-mapping (kga-house-n-alts kga-ks)
        drv-mapping  (drv-house-n-alts h-mapping drv-diff)

        common (set/intersection drv-ks kga-ks)

        *no-matches (volatile! #{})
        *used (volatile! #{})
        ]

    {
     :kga-houses (merge
                   kga-hs
                   ;; copy drv data for intersection
                   (reduce (fn [a k]
                             (assoc a k
                                      (dissoc (first (get drv-hs k)) :n)
                                      )
                             ) {} common)
                   (reduce (fn [a k]
                             (if-let [nu-k (get h-mapping k)]
                               (do
                                 (vswap! *used conj k)
                                 (assoc a nu-k
                                          (dissoc (first (get drv-hs k)) :n))
                                 )

                               (if-let [nu-k (get drv-mapping k)]
                                 (do
                                   (vswap! *used conj k)
                                   (assoc a nu-k (dissoc (first (get drv-hs k)) :n))
                                   )
                                 (do
                                   (vswap! *no-matches conj k)
                                   (assoc a k
                                            {:no-match true}))
                                 )

                               )
                             )
                           {} drv-diff)
                   (reduce (fn [a k]
                             (assoc a k (assoc
                                          (dissoc (first (get drv-hs k)) :n)
                                          :drv-only? true
                                          )
                                      )
                             )
                           {} @*no-matches)
                   )
     :used @*used
     :no-matches @*no-matches
     }
    )

  )

;;


;; street processing

(def vul-regex #"вул\.\s" )
(def vul-regex-1 #"вулиця\s" )

(def pl-regex #"пл\.\s" )
(def pl-regex-1 #"площа\s" )
(def prov-regex #"пров\.\s" )
(def prosp-regex #"просп\.\s" )

(def bulv-regex #"бульв\.\s" )

(def prosp-regex-1 #"проспект\s" )
(def shose-regex #"шосе\s" )


;; move get type to be last
;; todo: deprecated. can use more generic
(defn normalize-ua-geonim [street-name]
  ;; maybe this could be optimized by spliting string to words and checking the substitutions on first/last words

  (cond
    (re-find vul-regex street-name) (str (str/replace street-name vul-regex "") " вулиця")
    (re-find vul-regex-1 street-name) (str (str/replace street-name vul-regex-1 "") " вулиця")

    (re-find pl-regex street-name) (str (str/replace street-name pl-regex "") " площа")
    (re-find pl-regex-1 street-name) (str (str/replace street-name pl-regex-1 "") " площа")

    (re-find prov-regex street-name) (str (str/replace street-name prov-regex "") " провулок")

    (re-find prosp-regex street-name) (str (str/replace street-name prosp-regex "") " проспект")
    (re-find prosp-regex-1 street-name) (str (str/replace street-name prosp-regex-1 "") " проспект")

    (re-find bulv-regex street-name) (str (str/replace street-name bulv-regex "") " бульвар")

    (re-find shose-regex street-name) (str (str/replace street-name shose-regex "") " шосе")
    :else street-name
    )
  )


(defn str-extract [shortenings street]
  (loop [shortenings shortenings]
    (when (seq shortenings)
      (let [[geonim v] (first shortenings)]
        (if-not (nil? (str/index-of street geonim))
          [v (str/trim (str/replace street geonim ""))]
          (recur (rest shortenings))
          )))))


(defn match-geonim [geonims street]
  (if-let [extracted-geonims (str-extract geonims street)]
    extracted-geonims
    ["" street]
    ))


(def ua-geonim (partial match-geonim (array-map
                                       "вулиця" "вулиця"
                                       "вул." "вулиця"

                                       "провулок" "провулок"
                                       "пров." "провулок"

                                       "проспект" "проспект"
                                       "просп." "проспект"

                                       "бульвар" "бульвар"
                                       "бульв." "бульвар"

                                       "площа" "площа"
                                       "пл." "площа"

                                       "алея" "алея"
                                       "дорога" "дорога"
                                       "набережна" "набережна"
                                       "проїзд" "проїзд"
                                       "тупик" "тупик"
                                       "узвіз" "узвіз"
                                       "шосе" "шосе"

                                       )))


(def ru-geonim (partial match-geonim (array-map
                                     ", ул." "вулиця"
                                     "ул." "вулиця"
                                     "аллея" "алея"
                                     "ал." "алея"
                                     "дорога" "дорога"
                                     "улица" "вулиця"
                                     "пл." "площа"
                                     "пер." "провулок"
                                     "бульв." "бульвар"
                                     "просп." "проспект"
                                     "проезд" "проїзд"
                                     "наб." "набережна"
                                     "туп." "тупик"
                                     "шоссе" "шосе"
                                     "спуск" "узвіз"
                                     )))

(def en-geonim (partial match-geonim (array-map
                                     "vul." "вулиця"
                                     "prov." "провулок"
                                     "pl." "площа"
                                     )))