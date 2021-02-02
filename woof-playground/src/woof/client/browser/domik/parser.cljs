(ns woof.client.browser.domik.parser
  (:require
    [goog.dom :as dom]
    [goog.dom.classes :as classes]
    [goog.dom.dataset :as dataset]

    [cljs.core.async :refer [go go-loop] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]

    [cljs-time.core :as time]
    [cljs-time.format :as time-fmt]

    [clojure.string :as str]

    [woof.base :as base :refer [&chan-factory make-chan]]
    [woof.data :as d]

    [woof.client.dom :as woof-dom :refer [q q* attr txt txt-only mark!]]
    [woof.utils :as u]))



(defn parse-area [area-string]
  (let [z (str/replace area-string "Площадь (общ./жил./кух.): " "")
        [total-area living-area kitchen-area] (str/split z "/")
        ]
    {
     :area z  ;s
     :area_total (d/to-primitive total-area)
     :area_living (d/to-primitive living-area)
     :area_kitchen (d/to-primitive kitchen-area)
     }
    )
  )

(defn parse-listing-text [$ID bodyEls]
  (reduce
    (fn [m s]
      (cond
        (str/starts-with? s "Этаж: ")
        (merge m
               (let [[raw-floor & material] (str/split s #", ")
                     [floor floor-total] (str/split (str/replace raw-floor #"Этаж: " "") #"/")]
                 {
                  :floor       (js/parseInt floor 10)
                  :floor_total (js/parseInt floor-total 10)

                  :house_walls (str/join ", " material)
                  }))

        (str/starts-with? s "Площадь")
        (merge m (parse-area s))

        :else (assoc
                m :other s)
        )
      )
    {
     :info (str (dom/getTextContent $ID) "\n"
                (reduce (fn [a p] (str a (goog.dom/getTextContent p) "\n")) "" bodyEls))
     } (map goog.dom/getTextContent bodyEls))
  )


(defn extract-listing-price [costEl commissionEl area]
  (let [raw-cost (dom/getTextContent costEl)

        cost-uah (str/replace raw-cost #"₴|\s" "")
        raw-commission (dom/getTextContent commissionEl)

        _commission (str/replace raw-commission #"\$|\s" "")
        [cost-usd commission] (str/split _commission #"\+")

        UAH (js/parseInt cost-uah 10)
        USD (js/parseInt cost-usd 10)
        ]

    {
     :UAH UAH
     :USD USD
     :UAH_M2 (.floor js/Math (/ UAH area))
     :USD_M2 (.floor js/Math (/ USD area))

     :commission (if commission commission "")
     }
    )

  )


(defn parse-img [img-el]
  {
   :img-1 (. img-el -src)
   }
  )


(def domik-upd-format (time-fmt/formatter "dd.MM.yyyy hh:mm"))

(def out-format (time-fmt/formatter "yyyy.MM.dd"))


(defn parse-updated [$date]
  (let [s (txt $date)]
;; 30.01.2021 13:21
    {
     ;; :upd
     :added (time-fmt/unparse out-format
                              (time-fmt/parse domik-upd-format s))
     }
    )
  )

(defn do-scrape! [id el]
  (let [$ID (q el ".tittle_obj [clickcntid]")


        ;; to know that it's a novobudova
        projectEl (.querySelector el ".objava_detal_info .project_link")

        ;_ (.warn js/console
        ;         el
        ;         (.querySelector el ".objava_detal_info .project_link"))


        houseTypeEl (.querySelector el ".objava_detal_info .color-gray a")
        ; color-gray

        raw-address (dom/getTextContent (.querySelector el ".adress_text"))

        [_ _ district street building] (str/split raw-address #", ")

        project (if projectEl (.getAttribute projectEl "href") nil)





        ]

    (merge
      ;;
      ;; GENERAL
      {
       :source :domik
       :id     id
       :url    (str "http://domik.ua"  (attr $ID "href"))
       }
      ;;
      ;; INFO

      ;; PRICE+INFO
      (let [$info-els (q* el ".objava_detal_info .color-gray")

            data (parse-listing-text $ID $info-els)
            ;;
            ;; PRICE
            ]
        (merge
          data
          (extract-listing-price (q el ".price .cost")
                                 (q el ".price .commission")
                                 (:area_total data)
                                 )

          )


        )

      ;;
      ;; ADDR
      {
       :lat          (attr el "geolat")
       :lng          (attr el "geolng")

       :addr    raw-address

       :addr_district     district
       :addr_street       street
       :addr_house     building

       :addr_subway        (if-let [metroEl (q el ".adress_addInfo .metro")]
                             (.getAttribute metroEl "title"))

       ;:house        (if houseEls (map #(.getAttribute % "href") (array-seq houseEls)) nil)
       ;:houseTypeUrl (if houseTypeEl (.getAttribute houseTypeEl "href"))
       :house_walls    (if houseTypeEl
                         (let [ht (dom/getTextContent houseTypeEl)]
                           (if (= "Сталинки" ht)
                             (classes/add el "WOOF-CANDIDATE"))
                           ht) "")
       }

      ;; guess rooms
      (let [full-txt (str/lower-case (txt (q el ".objava_content"))) ]

        {
         :rooms (cond
                  (str/includes? full-txt "одно") 1
                  (str/includes? full-txt "1-к") 1

                  (str/includes? full-txt "двух") 2
                  (str/includes? full-txt "2-комн") 2
                  (str/includes? full-txt "2-х") 2
                  (str/includes? full-txt "2х") 2
                  (str/includes? full-txt "2 кім") 2
                  (str/includes? full-txt "2-к") 2

                  (str/includes? full-txt "трех") 3
                  (str/includes? full-txt "3-х") 3
                  (str/includes? full-txt "3-к") 3
                  (str/includes? full-txt "3х") 3

                  (str/includes? full-txt "4-х") 4
                  (str/includes? full-txt "4-к") 4
                  :else (do
                          (classes/add el "WOOF-ERROR")
                          -1)
                  )
         }
        )





      ;;
      ;; IMG
      (if-let [$img (q el ".informer_fotka_block .image")]
        (-> $img
            parse-img))

      ;; META
      (if-let [$date (q el ".objava_data_cod > span + span")]
        (-> $date ;(mark! "UPD")
            parse-updated))

      ;; :paid-info

      ;;         birka (.querySelector el ".birka")
      #_{
       ; numer of imgs?
       :birka (if birka
                (dom/getTextContent birka)
                "")
       ;; :html    (. el -innerHTML)
       ;;:kod     (dom/getTextContent (.querySelector el ".objava_data_cod > span"))
       ; :project project
       }
      (if project {:paid true} {})
      (if-let [$paid (q el ".objava_data_hot .hot")] {:paid-info "hot"})
      (if-let [$paid (q el ".objava_data_hot .vip")] {:paid-info "vip"})
      )
    )
  )

(defn parse-listing [el]
  (let [$id (q el ".tittle_obj [clickcntid]")
        id (attr $id "clickcntid")]
    (do-scrape! id el)
    ))



