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

(comment
  (let [        aEl      (.querySelector el ".tittle_obj [clickcntid]")
        houseEls (.querySelectorAll el ".adress_addInfo a")
        metroEl  (.querySelector el ".adress_addInfo .metro")

        projectEl (.querySelector el ".objava_detal_info .project_link")

        ;_ (.warn js/console
        ;         el
        ;         (.querySelector el ".objava_detal_info .project_link"))

        bodyEls (array-seq (.querySelectorAll el ".objava_detal_info .color-gray"))

        houseTypeEl (.querySelector el ".objava_detal_info .color-gray a")
        ; color-gray

        raw-address (dom/getTextContent (.querySelector el ".adress_text"))

        [_ _ district street building] (str/split raw-address #", ")

        project (if projectEl (.getAttribute projectEl "href") nil)

        birka (.querySelector el ".birka")
        ]
    123
    )
  )


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
        kw (get {"Год постройки" :year
                 "Высота стен" :h
                 "Этажность" :floors-total
                 "GPS-координаты" :gps
                 "Тип дома" :house-type
                 "Тип" :type
                 "Материал стен" :walls
                 } s s)]

    (cond
      (keyword? kw) kw

      ;;(str/starts-with? kw "Метро ") :nearest

      :else kw
      )


    )
  )

(defn- detail-v [k v]
  (if v
    (let [s (str/trim v)]
      (cond
        (= :gps k) (vec (map js/parseFloat (str/split s " ")))
        (= :year k) (js/parseInt s 10)
        ;; (= :nearest k) (str s " " k)
        :else s
        )
      )
    "AAAAA")
  )


(defn- parse-details [els]

  (reduce (fn [a el]
            (let [s (dom/getTextContent el)
                  [_k _v] (str/split s ":")]

              (if _v
                ;; handle KKK: asdasd
                (let [k (detail-k _k)]
                  (assoc a k (detail-v k _v)))

                ;; handle ZZZ - asasa
                (let [[_k _v] (str/split _k "-")
                      k (detail-k _k)]
                  (assoc a k (detail-v k _v)))
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
    {}
    )

  )