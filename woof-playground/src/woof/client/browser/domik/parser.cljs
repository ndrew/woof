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


      ;(prn rest )

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


(defn parse-images [el]
  (let [imgs (array-seq (.querySelectorAll el ".informer_fotka_block .image"))]
    {
     :images (vec
               (map (fn [img-el]
                      (. img-el -src)
                      ) imgs ))
     }
    )
  )


(defn do-scrape! [id el]
  (let [aEl (q el ".tittle_obj [clickcntid]")
        houseEls (.querySelectorAll el ".adress_addInfo a")
        metroEl  (.querySelector el ".adress_addInfo .metro")

        ;; to know that it's a novobudova
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



        model {
               :birka (if birka
                        (dom/getTextContent birka)
                        "")

               ;; :html    (. el -innerHTML)
               :id      id

               :kod     (dom/getTextContent (.querySelector el ".objava_data_cod > span"))
               :date    (dom/getTextContent (.querySelector el ".objava_data_cod > span + span"))

               :url     (.getAttribute aEl "href")
               :project project

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
        ]

    (let [listing (merge model
                         (extract-listing-text bodyEls)
                         (parse-images el)
                         (if project {:ad? true} {}))]

      listing
      )
    )
  )


(defn scrape-element [params el]
  (let [$id (q el ".tittle_obj [clickcntid]")]
    (if-let [id (attr $id "clickcntid")]
      (let [cf (&chan-factory params)
            chan (make-chan cf (base/sid))
            result (do-scrape! id el)
            btn (dom/createDom "button" "ok-btn WOOF-DOM" "✅OK")]

        (let [ids @(:*IDS params)]
          ; (.warn js/console (:id result) ids)
          (if-not (get ids (:id result))
            (do
              (swap! (:*IDS params) conj (:id result))
              (dom/appendChild el btn)
              (woof-dom/on-click btn
                                 (fn [e]
                                   (async/put! chan result))
                                 )
              chan)
            (do
              ;; (.warn js/console "ALREADY PROCESSED!!!")

              (classes/add el "WOOF-SEEN")
              (classes/add el "WOOF-SKIP")

              nil
              )
            )
          )
        )
      (u/throw! "CAN'T FIND ID IN ELEMENT")
      )
    )

  )
