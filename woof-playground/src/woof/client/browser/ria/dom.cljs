(ns woof.client.browser.ria.dom
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


(defn parse-listing [el]

  (let [_id (.-id el)
        [_ id] (re-matches #"search-realty-(\d+)" _id)
        $URL (q el "a.photo-340x220")
        ]

    (merge
      ;;
      ;; GENERAL
      {
       :id     id
       :url    (str "https://dom.ria.com" (attr $URL "href"))
       :source :ria
       }
      ;;
      ;; INFO
      (let [$INFO (q el ".descriptions-ticket")]
        {
         :info (txt $INFO)
         })
      ;;
      ;; ADDR
      (let [$ADDR (q el ".wrap_desc .size18 A")]
        {
         :addr_street   (txt (q $ADDR "SPAN:nth-child(1)"))
         :addr_district (woof-dom/txt-only $ADDR)
         :_addr         (str/trim (txt $ADDR))
         })
      ;;
      ;;
      (let [[$rooms $area] (q* el ".wrap_desc .mb-10 li")
            area-total (-> $area
                           txt
                           str/trim
                           (str/split " ")
                           first
                           js/parseInt)]
        (merge {
                :rooms      (-> $rooms
                                txt
                                str/trim
                                (str/split " ")
                                first
                                js/parseInt
                                )
                :area_total area-total
                }
               ;;
               ;; PRICE
               (let [$PRICE (q el ".mb-5 > span > span:not(.hide)")
                     $USD (q $PRICE "b")
                     _USD (txt $USD)

                     USD (-> $USD
                             txt
                             (str/replace #" " "")
                             (str/replace #"\$" "")
                             js/parseInt)
                     UAH (-> $PRICE
                             woof-dom/txt-only
                             str/trim
                             (str/replace #" " "")
                             (str/replace #"грн" "")
                             js/parseInt)
                     ]
                 {
                  :_price (txt $PRICE)

                  :UAH    UAH
                  :UAH_M2 (.floor js/Math (/ UAH area-total))

                  :USD    USD
                  :USD_M2 (.floor js/Math (/ USD area-total))
                  }
                 )
               )

        )
      )
    )
  )