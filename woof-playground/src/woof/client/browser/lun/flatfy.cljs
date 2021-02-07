(ns woof.client.browser.lun.flatfy
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

(defn try-parse-listing [el]
  ;(.log js/console el)


  (merge
    ;;
    ;; GENERAL
    (let [$id (q el "A.realty-preview__content-link")
          url (attr $id "href")

          id (attr el "id")

          ;; _ (mark! $id "ID")
          ]

      (when (nil? id)
        (mark! el "NO ID")
        (u/throw! "NO ID!!!"))

      (when (str/starts-with? url "https://lun.ua/novostroyki")
        (mark! el "novobud")
        (u/throw! "novobud")
        )

      {
       :source :flatfy
       :id     id
       :url    (str "https://flatfy.ua" url)
       }
      )

    ;;
    ;; INFO
    (if-let [$INFO (q el "P") ]
        {:info (-> $INFO
                   ;;(mark! "DESCR")
                   (txt)
                   (str/trim))}
        {:info ""})
    ;;
    ;; ADDR
    (when-let [$ADDR (q el ".realty-preview__title-link")]
        {
         :addr_street (-> $ADDR ; (mark! "ADDR")
                          (txt))
         })
    (when-let [$room (q el ".rooms")]
      {:rooms (-> $room
                  (txt)
                  (str/split #"\s")
                  (first)
                  (js/parseInt)
                  )}
      )

    ;;
    ;; PRICE
    (let [$PRICE    (q el ".realty-preview__price")
            $PRICE-M2 (q el ".realty-preview__price--sqm")]
        ;; todo: commision
        {
         :USD (-> $PRICE
                   (txt)
                   (str/replace "$" "")
                   (str/replace #"\s" "")
                   (str/trim)
                   (js/parseInt)
                  )
         :USD_M2 (-> $PRICE-M2
                     (txt)
                     (str/replace "$ за м²" "")
                     (str/replace  #"\s" "")
                     (str/trim)
                     (js/parseInt)
                     )
         }
        )

    ;;
    ;; IMG
    (let [$IMG  (q el "IMG")]
      {:img-1 (-> $IMG
                  (attr "src"))}
        )

    ;;
    ;; HOUSE
    (when-let [$area (q el ".area")]
      (let [_area (txt $area)
            [_total _living _kitchen] (map d/to-primitive (-> _area
                                                              (str/replace " " "")
                                                              (str/replace "м²" "")
                                                              (str/split "/")))]
        {:area _area
         :area_total _total
         :area_living _living
         :area_kitchen _kitchen}
        )
      )

    ;; todo: extract year and
    #_(let [$props (q* el ".realty-preview__info:not(.area):not(.realty-preview__info--time)")
          props (map txt $props)
          ]
      (reduce (fn [a s]
                (cond
                  (str/starts-with? s "поверх") a ;(assoc a )
                  :else a
                  )

                )
        props (map txt $props))
      )

    #_(when-let [$HOUSE (q el ".catalog-item__general-info .catalog-item_info-item-row")]
        (-> $HOUSE ;(mark! "HOUSE")
            ->house))

    ;;
    ;; META
    #_(when-let [$AGENT (q el ".ov-author__info .ov-author__name A")]
        (-> $AGENT ;(mark! "AGENT")
            parse-AGENT))

    (let [$time-els (q* el ".realty-preview__info--time")]
      (reduce (fn [a el]
                (let [[op & ss] (-> el
                          (txt)
                          (str/trim)
                          (str/split #"\s"))
                      ]
                  (cond
                    (= "Оновлено:" op) (assoc a :upd (str/join ss " "))
                    (= "Знайдено:" op) (assoc a :added (str/join ss " "))
                    :else a
                    ))
                ) {} $time-els)

      )

    #_(when-let [$UPD (q el ".catalog-item__additional-info-container .catalog-item__additional-info")]
        (-> $UPD ;(mark! "UPD")
            parse-UPD))

    #_(when-let [$LABELS (q* el ".catalog-item__general-info > H2:nth-child(1) > DIV .label")]
        (let [initial (if (q el ".paid") {:paid true} {})
              ;dbg! (fn [v] (.warn js/console v)  v)
              ]
          (-> $LABELS ;(mark! "LABELS")
              (parse-LABELS initial)
              ;dbg!
              )))
    )

  )

(defn parse-listing [el]
  (try
    (try-parse-listing el)
    (catch js/Error e
      (do
        (.error js/console e)
        (classes/add el "WOOF-SKIP")
        (classes/add el "WOOF-ERROR")
        nil
        )
      )
    )
  )

