(ns woof.client.browser.rieltor.parser
  (:require
    [goog.dom.classes :as classes]
    [goog.dom.dataset :as dataset]

    [cljs.core.async :refer [go go-loop] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]

    [cljs-time.core :as time]
    [cljs-time.format :as time-fmt]

    [clojure.string :as str]

    [woof.data :as d]

    [woof.client.dom :as wdom :refer [q q* attr txt txt-only mark!]]))


;;;;;;;;;;;

(defn safe-href [el selector]
  (if-let [sub-el (q el selector)]
    (attr sub-el "href")
    (do
      (.log js/console "can't find element for selector: " selector "parent-el" el)
      (classes/add el "WOOF-PARSE-ERROR")
      "")))


(defn safe-txt [el selector]
  (if-let [sub-el (q el selector)]
    (txt sub-el) ;; don't trim for now
    (do
      (.log js/console "can't find element for selector: " selector "parent-el" el)
      (classes/add el "WOOF-PARSE-ERROR")
      "")))



(defn ->price [$PRICE $PRICE-M2]
  ;; "250 000 $" or hrn
  (let [price-text (-> $PRICE
                       (txt)
                       (str/replace " " ""))
        price-m2-text (-> $PRICE-M2
                          (txt)
                          (str/replace " " "")
                          ) ;; "2 326 $/м²"

        xchange-price-text (attr $PRICE "title")  ;; " По курсу НБУ - 3 291 123 грн / 65 977 грн/м²"
        [xchange-price xchange-price-m2] (-> xchange-price-text
                                             (str/trim)
                                             (str/replace "По курсу НБУ - " "")
                                             (str/replace " " "")
                                             (str/split "/"))

        price->curr (partial re-find #"^(\d+)(.+)$")
        ]

    (merge
      (let [[_ p c] (price->curr price-text)]
        {(if (= "$" c) :USD :UAH) (d/to-primitive p)})
      (let [[_ p c] (price->curr xchange-price)]
        {(if (= "$" c) :USD :UAH) (d/to-primitive p)})
      (let [[_ p c] (price->curr price-m2-text)]
        {(if (= "$/м²" c) :USD_M2 :UAH_M2) (d/to-primitive p)})
      (let [[_ p c] (price->curr xchange-price-m2)]
        {(if (= "$/м²" c) :USD_M2 :UAH_M2) (d/to-primitive p)})
      )
    )
  )


(defn ->house [$HOUSE]
  (let [t (txt $HOUSE)
        [_rooms _area] (str/split t "·")

        [_total _living _kitchen] (map d/to-primitive (-> _area
                                                          (str/replace " " "")
                                                          (str/replace "м²" "")
                                                          (str/split "/")))

        [_room_n _floor _material] (str/split _rooms ",")

        [floor floor-total] (map d/to-primitive (re-seq #"\d+" _floor))

        rooms-n (d/to-primitive (re-find #"\d+" _room_n))

        walls ((fnil str/trim "") _material)
        ]

    {
     :house_walls walls

     :area_total   _total
     :area_living  _living
     :area_kitchen _kitchen

     :floor       floor
     :floor_total floor-total

     :rooms rooms-n

     ;; " 66.7 / 32 / 10 м² " total bedrooms kitchen
     :_floor _floor
     :_rooms _room_n
     :_area _area
     }
    )
  )


(def custom-formatter (time-fmt/formatter "yyyy.MM.dd"))


(defn- ua->date [s]
  (let [t (cond
            (= "сьогодні" s) (time/today)
            (= "вчора" s) (time/yesterday)

            ;;
            :else (let [[_ _n unit] (re-find #"(\d+)(.+)тому" s)
                        n (d/to-primitive _n)]
                    (time/minus
                      (time/today) (cond
                                     (#{"дні" "днів"} unit) (time/days n)
                                     (= "тиж." unit) (time/weeks n)
                                     (= "міс." unit) (time/months n)))
                    )
            )]

    (time-fmt/unparse custom-formatter t)
    )

  )

(defn parse-UPD [$UPD]
  (let [t (-> $UPD
              txt
              (js/decodeURI)
              (str/replace " " "")
              )
        [_ _upd _added] (re-find #"^Онов:(.+)\sДод:(.+)$" t)
        ]

    {
     :upd   (ua->date (str/trim _upd))
     :added (ua->date (str/trim _added))
     }
    )
  )


(defn parse-AGENT [$AGENT]
  (let [n   (txt $AGENT)
        tel (-> (attr $AGENT "href"))

        [_ _tel] (re-find #"(\d+)\.rieltor.ua" tel)
        ]
    {
     :agent-id _tel
     :agent-name n
     }
    )
  )


(defn ->addr [$ADDR]
  (let [t (-> $ADDR
              txt)
        [_str _house-n _district] (str/split t ",")
        ;;[_ _upd _added] (re-find #"^Онов:(.+)\sДод:(.+)$" t)
        ]
    {
     :_ADDR t

     :addr_street   (str/trim _str)
     :addr_house    (str/trim _house-n)
     :addr_district (str/trim _district)
     }
    )
  )


(defn ->img [$IMG $IMG-NUM]

  (let [src (attr $IMG "src")
        src-set (attr $IMG "srcset")
        alt (attr $IMG "alt")

        img-n (-> $IMG-NUM
                  (txt-only)
                  (str/trim)
                  (d/to-primitive))
        ]

    {
     :imgs [src] ; ; maybe use src-set
     :img-1 src
     :img-n img-n
     :img-alt alt
     }
    )
  )


(defn parse-LABELS [$LABELS label-map ]
  (reduce (fn [m $lbl]
            (let [classes' (attr $lbl "class")
                  classes (into #{} (str/split classes' #"\s"))]
              (merge
                m
                (if (classes "label_no_commission") {:no_commission true})

                (if (classes "label_attention")
                  {
                   ;:paid true
                   :paid_info (str/trim (wdom/txt $lbl))
                   })

                (if (or (classes "label_location")
                        (classes "label_location_subway"))
                  (let [distr-url (attr $lbl "href")
                        distr (txt $lbl)]
                    (merge
                      {
                       :district_1     distr
                       :district_1_url distr-url
                       }
                      (if (classes "label_location_subway")
                        {:subway distr}))))
                (if (classes "label_new-building") {:house_new true})))
            )
          label-map
          $LABELS)
  )

(defn gen-id [href]
  (last (str/split href "/")))


(defn do-scrape! [id el]
  (merge
    ;;
    ;; GENERAL
    {
     :source :riel
     :id     id
     :url    (str "https://rieltor.ua" (safe-href el ".catalog-item__img A"))
     }
    ;;
    ;; INFO
    (if-let [$INFO (q el ".catalog-item__info .catalog-item_info-description") ]
      {:info (-> $INFO
                 ; (mark! "DESCR")
                 (txt)
                 (str/replace "... далі" "…")
                 (str/trim))}
      {:info ""})
    ;;
    ;; ADDR
    (when-let [$ADDR (q el ".catalog-item__general-info > H2:nth-child(1) > A:nth-child(1)")]
      (-> $ADDR ;(mark! "ADDR")
          ->addr))
    ;;
    ;; PRICE
    (let [$PRICE    (q el ".catalog-item__price-column .catalog-item__price")
          $PRICE-M2 (q el ".catalog-item__price-column .catalog-item__price-per-sqm")]
      ;;(mark! $PRICE "PRICE USD")
      ;;(mark! $PRICE-M2 "PRICE M^2")

      ;; todo: commision
      (->price $PRICE $PRICE-M2))

    ;;
    ;; IMG
    (let [$IMG     (q el ".catalog-item__img IMG")
          $IMG-NUM (q el ".catalog-item__img .catalog-item__img-num")]
      (->img $IMG $IMG-NUM))

    ;;
    ;; HOUSE
    (when-let [$HOUSE (q el ".catalog-item__general-info .catalog-item_info-item-row")]
      (-> $HOUSE ;(mark! "HOUSE")
          ->house))

    ;;
    ;; META
    (when-let [$AGENT (q el ".ov-author__info .ov-author__name A")]
      (-> $AGENT ;(mark! "AGENT")
          parse-AGENT))

    (when-let [$UPD (q el ".catalog-item__additional-info-container .catalog-item__additional-info")]
      (-> $UPD ;(mark! "UPD")
          parse-UPD))

    (when-let [$LABELS (q* el ".catalog-item__general-info > H2:nth-child(1) > DIV .label")]
      (let [initial (if (q el ".paid") {:paid true} {})]
        (-> $LABELS ;(mark! "LABELS")
            (parse-LABELS initial))))
    )
  )



;;
;; element scraping function
(defn scrape-element [el]
  ;; $ID      (q el ".catalog-item__img A")
  (if-let [_id (safe-href el ".catalog-item__img A")]
    (do-scrape! (gen-id _id) el))

  ;; todo: handle errors and nils?
  )
