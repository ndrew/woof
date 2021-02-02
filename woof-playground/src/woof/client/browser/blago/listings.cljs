(ns woof.client.browser.blago.listings
  (:require
    [goog.object]
    [goog.dom :as dom]
    [goog.dom.classes :as classes]
    [goog.dom.dataset :as dataset]

    [cljs.core.async :as async :refer  [go go-loop]]
    [clojure.string :as str]

    [woof.base :as base :refer [&chan-factory make-chan]]
    [woof.data :as d]
    [woof.utils :as u]

    [woof.client.dom :as woof-dom :refer [q q* attr txt txt-only mark!]]

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


(defn- extract-addr [addr]
  (let [[ap
         street
         house
         city] (str/split addr ",")]
    (merge
      (let [re #"(\d+) кім. квартира (\d*).+"
            [_ rooms m2 ] (re-matches re ap)]
        {:rooms (int rooms)
         :area_total (int m2)
         })
      {:addr_street (str/trim street)
       :addr_house  (str/trim house)}
      )
    )

  )


(defn- extract-uah [area-total uah]
  (let [UAH (->
              uah
              (str/replace #"\s" "")
              (str/replace #"\*грн\." "")
              int
              )]
    {
     :UAH UAH
     :UAH_M2 (.floor js/Math (/ UAH area-total))
     }
    )

  )

(defn- extract-usd [area-total usd]
  (let [[_usd
         _
         _rate] (-> usd
                   (str/replace #"\s" "")
                   (str/split #"\$"))
        USD (int _usd)
        ]

    {
     :USD USD
     :USD2UAH (-> _rate
                  (str/replace #"=" "")
                  (str/replace #"грн\.\)" "")
                  js/parseFloat)
     :USD_M2 (.floor js/Math (/ USD area-total))

     }

    )
  )


(defn- extract-eur [area-total eur]
  (let [[_eur _ _rate] (->
             eur
             (str/replace #"\s" "")
             (str/split #"€")
             )
        EUR (int _eur)
        ]

    {
     :EUR     EUR
     :EUR2UAH (-> _rate
                  (str/replace #"=" "")
                  (str/replace #"грн\.\)" "")
                  js/parseFloat)
     }
    )
  )


;; parsing implementation
(defn parse-listing [el]
  (let [

        $link (q el ".col-md-11 a.link")
        raw-link-text (dom/getTextContent $link)
        link-href (attr $link "href")

        $uah (q el ".price > p")
        $eur (q el ".price > .m-euro")
        $usd (q el ".price > .m-dollar")

        $regs (woof-dom/q* el ".info-region a")

        $complex (q el ".info-complex a")

        $photos (woof-dom/q* el ".house-photo img")
        PHOTOS (vec (map-indexed (fn [i $el]
                                   {:src (attr $el "src")
                                    :alt (attr $el "alt")
                                    :title (attr $el "alt")
                                    :i i
                                    }) $photos))

        $info (woof-dom/q el ".info-text .col-md-9")

        REGIONS (vec (map (fn [$el]
                            (text $el)
                           #_{:href (attr $el "href")
                            :t (text $el)}
                           ) $regs))
        [addr-district addr-district-1] REGIONS

        ]


    (merge
      {
       :id (:objectCode (dataset->clj el))
       :url link-href
       :source :blago
       }
      (let [data (extract-addr raw-link-text)
            area-total (:area_total data)
            ]
        (merge
          data
          (extract-uah area-total (text $uah))
          (extract-eur area-total (text $eur))
          (extract-usd area-total (text $usd))
          )
        )

      {

       :info (str
               raw-link-text " \n"
               (text $info)  )

       :addr_district     addr-district
       :addr_district_1   addr-district-1

       :imgs PHOTOS

       }
      (reduce
        (fn [a p]
          (assoc a
            (keyword (str "img-" (:i p)))
            (:src p))
          )
        {} PHOTOS)
      )
    )
  )



(defn scrape-element [params el]
  (let [cf (&chan-factory params)
        chan (make-chan cf (base/sid))
        result (parse-listing el)
        btn (dom/createDom "button" "ok-btn WOOF-DOM" "✅OK")]

    (if-let [post-scrape-fn (:scraper/post-scrape-fn params)]
      (post-scrape-fn el result))

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

  )