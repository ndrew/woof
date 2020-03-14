(ns ^:figwheel-hooks ^:figwheel-always
  woof.scraper
  (:require
    [woof.lib :as lib]
    [goog.object]
    [goog.dom :as dom]
    [goog.object]
    [goog.dom.query :as query]
    [goog.dom.classes :as classes]

    [clojure.string :as str]

    [woof.data :as d]

    [woof.base :as base]
    [woof.wf :as wf]
    [woof.utils :as u]))


(defn extract-listing-text [bodyEls]
  (let [[floor sq & rest] (map goog.dom/getTextContent bodyEls)]
    {
     :full-text (reduce (fn [a p] (str a (goog.dom/getTextContent p) "\n")) "" bodyEls)

     :floor floor
     :square sq

     }
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

(defn parse-listing [el]
  (let [

        aEl      (.querySelector el ".tittle_obj [clickcntid]")
        houseEls (.querySelectorAll el ".adress_addInfo a")
        metroEl  (.querySelector el ".adress_addInfo .metro")

        ;; to know that it's a novobudova
        projectEl (.querySelector el ".project_link")

        bodyEls (array-seq (.querySelectorAll el ".objava_detal_info .color-gray"))

        houseTypeEl (.querySelector el ".objava_detal_info .color-gray a")
        ; color-gray
        ]

    {

     :id (.getAttribute aEl "clickcntid") ;; or get id from top of the page

     :kod (dom/getTextContent (.querySelector el ".objava_data_cod > span"))
     :date (dom/getTextContent (.querySelector el ".objava_data_cod > span + span"))

     :url (.getAttribute aEl "href")
     :project (if projectEl (.getAttribute projectEl "href") nil)

     :title (dom/getTextContent aEl)

     :addr {
            :lat (.getAttribute el "geolat")
            :lng (.getAttribute el "geolng")

            :addr (dom/getTextContent (.querySelector el ".adress_text"))
            :metro (if metroEl (.getAttribute metroEl "title") nil)
            :house (if houseEls (map #(.getAttribute  % "href") (array-seq houseEls))  nil)
            :houseTypeUrl (if houseTypeEl (.getAttribute houseTypeEl "href") )
            :houseType    (if houseTypeEl (dom/getTextContent houseTypeEl))
            }


     :price (extract-listing-price (.querySelector el ".price .cost")
                                   (.querySelector el ".price .commission"))

     :text  (extract-listing-text bodyEls)

     }
    )
  )

(defn prepare-data [data]

  ;; sort or
  data
  )
