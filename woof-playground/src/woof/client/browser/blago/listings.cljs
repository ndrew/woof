(ns woof.client.browser.blago.listings
  (:require
    [goog.object]
    [goog.dom :as dom]
    [goog.object]
    [goog.dom.classes :as classes]

    [cljs.core.async :as async :refer  [go go-loop]]
    [clojure.string :as str]

    [woof.base :as base]
    [woof.client.dom :as woof-dom]
    [woof.data :as d]
    [woof.utils :as u]

    [goog.dom.dataset :as dataset]


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


(def text dom/getTextContent)


(defn dataset->clj [el]
  (js->clj (.parse js/JSON (.stringify js/JSON el.dataset))
           :keywordize-keys true
           )
  )



(defn- extract-uah [uah]
  {:uah (->
          uah
          (str/replace #"\s" "")
          (str/replace #"\*грн\." "")
          int
          )
   }
  )

(defn- extract-apartment [ap]
  (let [re #"(\d+) кім. квартира (\d*).+"
        [_ rooms m2 ] (re-matches re ap)
        ]
    {:rooms (int rooms)
     :m2 (int m2)
     }
    )
  )

(defn- extract-addr [addr]
  (let [[ap
         street
         house
         city] (str/split addr ",")]
    (merge
      (extract-apartment ap)
      {:street (str/trim street)
       :house  (str/trim house)}
      )
    )

  )

(defn- extract-usd [usd]
  (let [[_usd
         _
         _rate
         ] (->
             usd
             (str/replace #"\s" "")
             (str/split #"\$")
             )]

    {
     :usd (int _usd)
     :usd2uah (-> _rate
                  (str/replace #"=" "")
                  (str/replace #"грн\.\)" "")
                  js/parseFloat
                  )
     }

    )
  )


(defn- extract-eur [eur]
  (let [[_eur
         _
         _rate
         ] (->
             eur
             (str/replace #"\s" "")
             (str/split #"€")
             )]

    {
     :eur (int _eur)
     :eur2uah (-> _rate
                  (str/replace #"=" "")
                  (str/replace #"грн\.\)" "")
                  js/parseFloat
                  )
     }
    )
  )


;; parsing implementation
(defn parse-listing [el]


  (let [

        $link (.querySelector el ".col-md-11 a.link")

        raw-link-text (dom/getTextContent $link)

        link-href (.getAttribute $link "href")

        $uah (.querySelector el ".price > p")
        $eur (.querySelector el ".price > .m-euro")
        $usd (.querySelector el ".price > .m-dollar")

        $regs (woof-dom/q* el ".info-region a")

        $complex (.querySelector el ".info-complex a")

        $photos (woof-dom/q* el ".house-photo img")
        $info (woof-dom/q el ".info-text .col-md-9")
        ]


    (merge
      {
       :id (:objectCode (dataset->clj el))
       :href link-href
       }
      (extract-addr raw-link-text)
      (extract-uah (text $uah))
      (extract-eur (text $eur))
      (extract-usd (text $usd))
      {
       :link-text raw-link-text


       :short-info (text $info)

       ;:uah (text $uah)
       ;:eur (text $eur)
       ;:usd (text $usd)

       :region (vec (map (fn [$el]
                           {:href (.getAttribute $el "href")
                            :t (text $el)
                            }
                           ) $regs))

       :photos (vec (map (fn [$el]
                           {:src (.getAttribute $el "src")
                            :alt (.getAttribute $el "alt")
                            :title (.getAttribute $el "alt")
                            }) $photos))


       }

      )



    )

  )

