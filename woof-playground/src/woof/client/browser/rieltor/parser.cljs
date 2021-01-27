(ns woof.client.browser.rieltor.parser
  (:require
    [goog.dom :as dom]
    [goog.dom.classes :as classes]
    [goog.dom.dataset :as dataset]

    [cljs.core.async :refer [go go-loop] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]
    [clojure.string :as str]

    [woof.base :as base]
    [woof.data :as d]

    [woof.client.dom :as wdom :refer [q q* attr txt]]
    [woof.client.dbg :as dbg :refer [__log]]
    [woof.client.ws :as ws]

    [woof.utils :as u]

    [woof.wfs.evt-loop :as evt-loop]
    [woof.wfs.watcher :as watcher]

    [cljs-time.core :as time]
    [cljs-time.format :as time-fmt]

    ;; todo:
    [woof.client.browser.scraper.scraping-ui :as sui]
    [woof.client.browser.rieltor.ui :as wf-ui]

    [rum.core :as rum]
    ))

(def ALLOW-DOUBLE-PARSE true)



;;;;;;;;;;;

(defn safe-href [el selector]
  (if-let [sub-el (q el selector)]
    (attr sub-el "href")
    (do
      (.log js/console "can't find element for selector: " selector "parent-el" el)
      (classes/add el "parsed-error")
      ""
      )
    )
  )


(defn safe-txt [el selector]
  (if-let [sub-el (q el selector)]

    ;; don't trim for now
    (txt sub-el)
    (do
      (.log js/console "can't find element for selector: " selector "parent-el" el)
      (classes/add el "parsed-error")
      ""
      )
    )
  )


;;
(defn mark! [el parse-id]
  (when el
    (classes/set el "DDD")
    (dataset/set el "parseId" parse-id)
    ))

;;


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

(defn ->upd [$UPD]
  (let [t (-> $UPD
              (txt)
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


(defn ->agent [$AGENT]
  (let [n (wdom/txt $AGENT)
        tel (-> (wdom/attr $AGENT "href"))

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
              (wdom/txt)
              ;(js/decodeURI)
              ;(str/replace " " "")
              )
        [_str _house-n _district] (str/split t ",")

        ;;[_ _upd _added] (re-find #"^Онов:(.+)\sДод:(.+)$" t)
        ]
    {
     :_ADDR t

     :addr_street (str/trim _str)
     :addr_house (str/trim _house-n)
     :addr_district (str/trim _district)
     }
    )
  )


(defn ->img [$IMG $IMG-NUM]

  (let [src (wdom/attr $IMG "src")
        src-set (wdom/attr $IMG "srcset")
        alt (wdom/attr $IMG "alt")

        img-n (-> $IMG-NUM
                  (wdom/txt-only)
                  (str/trim)
                  (d/to-primitive)
                  )
        ]

    {
     :imgs [src
            ; src-set
            ]
     :img-n img-n
     :img-alt alt
     }
    )
  )

;;
;; element scraping function
(defn scrape-element [el]
  ;; saving parsing status in dom
  #_(if-not ALLOW-DOUBLE-PARSE
      (when (classes/has el "parsed")
        (.warn js/console "PARSE WAS CALLED TWICE")
        (classes/add el "parsed-twice"))
      )
  ;(classes/add el "parsed")

  ;; *PROCESSING-MAP
  (if-let [id (safe-href el ".catalog-item__img A")]
    (let [$ID (wdom/q el ".catalog-item__img A")            ; "DIV:nth-child(1) > DIV > A:nth-child(1)" -> ".catalog-item__img A"

          $IMG (wdom/q el ".catalog-item__img IMG")         ; "DIV:nth-child(1) > DIV > A:nth-child(1) > IMG"
          $IMG-NUM (wdom/q el ".catalog-item__img .catalog-item__img-num")


          $ADDR (wdom/q el ".catalog-item__general-info > H2:nth-child(1) > A:nth-child(1)") ; "DIV:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1) > H2:nth-child(1) > A:nth-child(1)"
          $HOUSE (wdom/q el ".catalog-item__general-info .catalog-item_info-item-row") ; "DIV:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1) > DIV:nth-child(2)"

          $LABELS (wdom/q* el ".catalog-item__general-info > H2:nth-child(1) > DIV .label")

          $DESCR (wdom/q el ".catalog-item__info .catalog-item_info-description") ;;

          $PRICE (wdom/q el ".catalog-item__price-column .catalog-item__price") ; "DIV:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(2) > STRONG:nth-child(1)"
          $PRICE-M2 (wdom/q el ".catalog-item__price-column .catalog-item__price-per-sqm")

          $UPD (wdom/q el ".catalog-item__additional-info-container .catalog-item__additional-info")

          $AGENT (wdom/q el ".ov-author__info .ov-author__name A")
          ]
      ;;(mark! (wdom/q el ".catalog-item__general-info H2 DIV A.label") "LBL")
      ;;(mark! $HOUSE "HOUSE-DETAILS")
      ;;(mark! $DESCR "DESCR")
      ;;(mark! $PRICE "PRICE USD")
      ;;(mark! $PRICE-M2 "PRICE M^2")
      ;;(mark! $UPD "UPD")
      ;;(mark! $AGENT "RIELTOR")
      ;;$LABELS


      (merge {
              :source :riel
              :id     id
              :url    (str "https://rieltor.ua" id)
              }
             ;; meta info block
             (reduce (fn [m $lbl]
                       (let [classes' (wdom/attr $lbl "class")
                             classes (into #{} (str/split classes' #"\s"))]

                         ;(.log js/console classes $lbl)
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
                             (let [distr-url (wdom/attr $lbl "href")
                                   distr (wdom/txt $lbl)]
                               (merge
                                 {
                                  :district_1     distr
                                  :district_1_url distr-url
                                  }
                                 (if (classes "label_location_subway")
                                   {:subway distr}))))
                           (if (classes "label_new-building") {:house_new true})

                           )
                         )
                       )
                     (if (wdom/q el ".paid")
                       {:paid true}
                       {})
                     $LABELS)

             (->addr $ADDR)
             (->price $PRICE $PRICE-M2)

             ;; todo: commision
             (->img $IMG $IMG-NUM)

             (->house $HOUSE)

             (->agent $AGENT)
             (->upd $UPD)
             (if $DESCR
               {:info (-> $DESCR
                          (wdom/txt)
                          (str/replace "... далі" "…")
                          (str/trim))}
               {:info ""}
               )
             )
      )
    )
  )
