(ns ^:figwheel-hooks woof.client.browser.domik.listings
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


(defn parse-listing [el]
  (let [

        aEl      (.querySelector el ".tittle_obj [clickcntid]")
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
               :id      (.getAttribute aEl "clickcntid") ;; or get id from top of the page

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


(defn safe-parse-listing [el]
  (try
    (parse-listing el)
    (catch js/Error e
      (do
        (.error js/console e el)
        {
         ;; for now, treat parsed with error as ads
         :ad? true
         }))
    )
  )

(defn async-parse-listing [ch el]

  ;(let [html-before (. el -innerHTML) ]
  ;; (.scrollIntoView el true)

  (go
    ;; (async/<! (u/timeout 1000))


    #_(let [html-after (. el -innerHTML)]
        (if (not= html-after html-before)
          (.log js/console
                "BEFORE:\n"
                html-before
                "AFTER:\n"
                html-after
                )
          )
        )


    (async/put! ch (safe-parse-listing el)))
  ch
  ; )

  )






;; {
;; :new       - listings that are not yet in summary
;; :duplicate - listings that are in summary, with same prices
;; :updated   - listings that are in summary, but with updated prices
;; :ad        - ads or invalid listings
;; }







(defn ui-listing-model-text [model]
  (d/pretty model))

;; format currency

(defn ui-listing-summary-text [model]
  (str
    (:id model) " - " (:title model) " "

    (get-in model [:price :usd]) "$, "
    (get-in model [:price :uah]) " UAH"
    )



  )


(defn listing-ui! [_ g model]
  ;; always find a new element, as dom element may get reused

  ;; what if no element is found? - skip
  (when-let [el (.querySelector (.-body js/document) (str "#objavaDiv" (:id model) ))]

    (classes/add el "woof-listing-parsed")

    (if (= :ad g)
      (classes/add el "woof-listing-ad")
      )

    ;;
    (if-let [ui-el (.querySelector el ".woof-custom-listing-ui")]
      ; update already added UI
      (let [summary-el (.querySelector el ".woof-custom-listing-ui > summary")
            model-el (.querySelector el ".woof-listing-model")

            model-text (ui-listing-model-text model)
            summary-text (ui-listing-summary-text model)
            ]



        (dom/setTextContent summary-el summary-text)
        (dom/setTextContent model-el model-text)
        )
      ; add new UI
      (let [model-text (ui-listing-model-text model)
            summary-text (ui-listing-summary-text model)

            inner-ui-el (dom/createDom "pre" "woof-listing-model" model-text)
            summary-el (dom/createDom "summary" "" summary-text)

            details-el (dom/createDom "details" "woof-custom-listing-ui" summary-el)
            ]

        (dom/append details-el inner-ui-el)
        (dom/insertChildAt el details-el 0)
        )
      )
    )

  )



;;;
;;


(defn- group-listing [summary model el]
  (if (:ad? model)
    :ad
    (if-let [s (get summary (:id model))]
      (do
        ;; todo: implement checking for duplicates
        (if (= (:header model) (:header s))
          :duplicate
          :updated
          ))
      :new)
    )
  )


(defn partition-listing [summary model]
  (group-by (fn [[model el]]
              (let [g (group-listing summary model el)]
                (listing-ui! el g model)
                g
                ))
            model)
  )