(ns ^:figwheel-hooks woof.client.browser.domik.houses
  (:require
    [goog.object]
    [goog.dom :as dom]
    [goog.object]
    [goog.dom.classes :as classes]

    [cljs.core.async :as async :refer  [go go-loop]]
    [clojure.string :as str]

    [woof.base :as base]
    [woof.client.ws :as ws]
    [woof.client.dom :as wdom]
    [woof.data :as d]
    [woof.utils :as u]

    [woof.client.browser.scraper.scraping-ui :as sui]
    ))


(defn- address-text [s]
  (if-let [[_ addr] (re-matches #"Адрес: (.*)" s)]
    (let [[_ _ distr street house] (map str/trim (str/split addr ","))]
      {
       :house house
       :street street
       :district distr
       }
      )
      {}
    )
  )


(defn- detail-k [k]
  (let [s (str/trim k)
        kw (get {"Год постройки" [:house :year]
                 "Высота стен" [:house :ceiling]
                 "Этажность" [:house :floors]
                 "GPS-координаты" [[:house :lat]
                                   [:house :lng]]
                 "Тип дома" [:house :type]
                 "Тип" [:house :t]
                 "Материал стен" [:house :walls]
                 } s s)]

    (cond
      (keyword? kw) [kw]
      (vector? kw) kw


      ;;(str/starts-with? kw "Метро ") :nearest
      :else (do
              [:house :metro kw]
              )
      )
    )
  )


(defn- detail-v [k v]
  (if v
    (let [s (str/trim v)]

      (cond
        (= [[:house :lat]
            [:house :lng]
            ] k) (vec (map js/parseFloat (str/split s " ")))
        (= [:house :year] k) (js/parseInt s 10)

        ;; (= :nearest k) (str s " " k)
        :else s
        )
      )
      "NO VALUE!"))



(defn- parse-details [els]

  (reduce (fn [a el]
            ;;; handle `Property: value` text values
            (let [s (dom/getTextContent el)
                  ;; try spliting
                  [_k _v] (str/split s ":")]
              (if _v ; k: v property
                ;; handle KKK: asdasd
                (let [k (detail-k _k)
                      nu-v (detail-v k _v)]

                  (if (every? vector? k)
                    ;; handle nested key selectors like [[:a :b] [:a :c]]
                    ;; not that value should be in a vector [:b-val :c-val]
                    (reduce
                      (fn [a [k v]] (assoc-in a k v))
                      a
                      (partition 2 (interleave k nu-v)))
                    ;; handle map selector [:k]
                    (assoc-in a k nu-v)
                    )
                  )

                ;; handle ZZZ - asasa
                (let [[_k _v] (str/split _k "-")
                      k (detail-k _k)
                      v (detail-v k _v)]
                  (assoc-in a k v)
                  )

                )
              )
            )
          {} els)
  )

(defn parse-house-details [el]

  (merge

    (if-let [$addr (.querySelector el ".krat_op > h2")]
      {:addr (address-text (dom/getTextContent $addr))} {:addr {}})

    (let [$details (wdom/query-selector* ".krat_op > p")]
      (parse-details $details)
      )

    (let [$el (.querySelector el ".krat_op .rating_block")]
      {:rating (wdom/attr $el "ratingval")
       :rating-num-votes (wdom/attr $el "votes_count")}
      )

    (let [$el (.querySelector el ".link-for-video a")]
      {:video (wdom/href $el)}
      )

    {}
    )

  )