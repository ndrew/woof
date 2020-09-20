(ns woof.client.browser.blago.listings
  (:require
    [goog.object]
    [goog.dom :as dom]
    [goog.object]
    [goog.dom.classes :as classes]

    [cljs.core.async :as async :refer  [go go-loop]]
    [clojure.string :as str]

    [woof.base :as base]
    [woof.client.ws :as ws]
    [woof.client.dom :as woof-dom]
    [woof.data :as d]
    [woof.utils :as u]

    [woof.client.browser.scraper.scraping-ui :as sui]
    ))


(defn extract-listing-text [bodyEls]
  (reduce
    (fn [m s]
      (cond
        (str/starts-with? s "Этаж: ")
        (merge m
               (let [[raw-floor & material] (str/split s #", ")
                     [floor floor-total] (str/split (str/replace raw-floor #"Этаж: " "") #"/")]
                 {
                  :floor       (js/parseInt floor 10)
                  :floor-total (js/parseInt floor-total 10)
                  :material (str/join ", " material)
                  }))

        (str/starts-with? s "Площадь")
        (merge m
               {
                :square s
                }
               )

        :else (assoc
                m :other s)
        )
      )
    {
     :full-text (reduce (fn [a p] (str a (goog.dom/getTextContent p) "\n")) "" bodyEls)
     } (map goog.dom/getTextContent bodyEls))
  #_(let [[raw-floor & rest] (map goog.dom/getTextContent bodyEls)

          full-text (reduce (fn [a p] (str a (goog.dom/getTextContent p) "\n")) "" bodyEls)
          [floor-str & type ] (str/split raw-floor #", ")

          ]


      (prn rest )

      (merge {
              :full-text full-text
              ;:square raw-sq
              }
             (if floor-str
               (let [[floor floor-total] (str/split (str/replace floor-str #"Этаж: " "") #"/")]
                 {
                  :floor (js/parseInt floor 10)
                  :floor-total (js/parseInt floor-total 10)
                  :material (str/join ", " type)
                  })
               {}
               )

             )
      )
  )


(defn extract-listing-price [costEl commissionEl]
  (let [raw-cost (dom/getTextContent costEl)

        cost-uah (str/replace raw-cost #"₴|\s" "")
        raw-commission (dom/getTextContent commissionEl)

        _commission (str/replace raw-commission #"\$|\s" "")
        [cost-usd commission] (str/split _commission #"\+")
        ]

    {
     :uah (js/parseInt cost-uah 10)
     :usd (js/parseInt cost-usd 10)
     :commission commission
     }
    )

  )

;; parsing implementation
(defn parse-listing [el]
  ;; (js-debugger)
  (.log js/console "parsing" el)
  #_(let [

          aEl      (.querySelector el ".tittle_obj [clickcntid]")
          houseEls (.querySelectorAll el ".adress_addInfo a")
          metroEl  (.querySelector el ".adress_addInfo .metro")

          ;; to know that it's a novobudova
          projectEl (.querySelector el ".project_link")

          bodyEls (array-seq (.querySelectorAll el ".objava_detal_info .color-gray"))

          houseTypeEl (.querySelector el ".objava_detal_info .color-gray a")
          ; color-gray

          raw-address (dom/getTextContent (.querySelector el ".adress_text"))

          [_ _ district street building] (str/split raw-address #", ")

          ]
      (merge {

              :id      (.getAttribute aEl "clickcntid") ;; or get id from top of the page

              :kod     (dom/getTextContent (.querySelector el ".objava_data_cod > span"))
              :date    (dom/getTextContent (.querySelector el ".objava_data_cod > span + span"))

              :url     (.getAttribute aEl "href")
              :project (if projectEl (.getAttribute projectEl "href") nil)

              :title   (dom/getTextContent aEl)

              :addr    {
                        :lat          (.getAttribute el "geolat")
                        :lng          (.getAttribute el "geolng")

                        :full-addr    raw-address
                        :district     district
                        :street       street
                        :building     building

                        :metro        (if metroEl (.getAttribute metroEl "title") nil)
                        :house        (if houseEls (map #(.getAttribute % "href") (array-seq houseEls)) nil)
                        :houseTypeUrl (if houseTypeEl (.getAttribute houseTypeEl "href"))
                        :houseType    (if houseTypeEl (dom/getTextContent houseTypeEl))
                        }


              :price   (extract-listing-price (.querySelector el ".price .cost")
                                              (.querySelector el ".price .commission"))

              }
             (extract-listing-text bodyEls)
             )
      )
  )

