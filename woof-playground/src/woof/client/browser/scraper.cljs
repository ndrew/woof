(ns ^:figwheel-hooks woof.client.browser.scraper
  (:require
    [goog.object]
    [goog.dom :as dom]
    [goog.object]
    [goog.dom.query :as query]
    [goog.dom.classes :as classes]

    [cljs.core.async :as async]
    [woof.base :as base]

    [clojure.string :as str]
    [woof.data :as d]

    [woof.client.ws :as ws]
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




(defn listing-text-ui [listing]
  (d/pretty listing)
  )

;;
(defn custom-ui [listing]

  ;(scraper/parse-listing el)
  (when-let [
             ;existing-el (.querySelector (.-body js/document) (str "a[clickcntid='" (:id listing) "']"))
             existing-el (.querySelector (.-body js/document) (str "#objavaDiv" (:id listing) ))
             ]

    ;; implement filter

    (classes/add existing-el "woof-listing-parsed")


    ;; todo: use filter
    (if (> (get-in listing [:price :uah])
           1000000
           )
      (classes/addRemove existing-el "woof-listing-show" "woof-listing-hide")
      (classes/addRemove existing-el "woof-listing-hide" "woof-listing-show")
      )


    (if-let [ui-el (.querySelector existing-el ".woof-custom-listing-ui")]
      ; update custom ui
      (dom/setTextContent ui-el (listing-text-ui listing))
      (let [inner-ui-el (dom/createDom "pre" "woof-custom-listing-ui"
                                       (listing-text-ui listing))]

        (dom/insertChildAt existing-el inner-ui-el 0)
        )
      )

    )


  ;; sort or
  listing
  )




;; use global state for now
(def *STATE (atom {
                   ::ids #{}
                   ::socket nil
                   }))



(defn common-ctx [params]
  {
   :log {:fn (fn[v]
               ;(prn v)
               (.log js/console v)
               v)}
   :export-edn {:fn (fn[v]
                      (prn v)
                      "")}

   }
  )



(defn common-opt[params]
  {
   :op-handlers-map {
                     :done  (fn [result]

                              (.log js/console result)

                              (.log js/console
                                    "RESULT"
                                    (::RESULT result))


                              )

                     :error (fn [result]
                              (.error js/console result))

                     }

   })


(defn scraper-init [params]
  ;; FIXME: ugly
  {:start-chan (async/chan)}
  )

(defn scraper-ctx [params]
  {

   ;; splits elements to a separate step
   :expand*            (base/expand-into :identity)


   ;; splits sid-list into
   :process*           (base/expand-into :process)

   :process            {
                        :fn (fn [el]
                              (parse-listing el))
                        }

   :listing-ui*        (base/expand-into :listing-ui)
   :listing-ui         {
                        :fn (fn [listing]
                              (custom-ui listing)

                              "ok"
                              )
                        }


   :filter-scraped {
                    :fn (fn [kv]
                          (let [ids (::ids *STATE)]
                               (reduce (fn [a [k v]]
                                         (if-not (get ids (:id v))
                                                 (assoc a (base/rand-sid "filter-") [:identity k])
                                                 a
                                                 )) {} kv)
                               )

                          )
                    :expands? true
                    }



   :post-process       {
                        :fn (fn [listings]
                              (sort-by
                                :uah
                                (map #(get % :price) listings)
                                )

                              )
                        }
   ;; todo: convenience wrapper for working with collection with single
   }
  )

(defn ws-ctx-fn [params]
  ;; "ws:localhost:8081/ws"
  (let [*state (atom {
                      ::socket nil
                      ::ids #{}
                      ::listings {}
                      })

        gen-msg-handler  (fn []
                       (let [first-ids (volatile! false)]
                            (fn [msg]
                              (let [[t body] (:msg msg)]
                                   (when (= t :ids)
                                     (swap! *state assoc :ids body)
                                     (when-not @first-ids
                                       ;; -->
                                       (async/put! (:start-chan params) true)
                                       (vswap! first-ids not)
                                       )
                                     )
                                   (.log js/console "PAYLOAD" msg)
                                   )
                              )
                            )
                       )


        ]
    {
     :init-socket    {
                      :fn (fn [url]
                            (let [ch (async/chan)
                                  msg-handler (gen-msg-handler)
                                  ]
                                 (ws/chan-connect url
                                                  :chan ch
                                                  :on-message (fn [payload]
                                                                (let [msg (ws/read-transit payload)]
                                                                     (msg-handler msg)))
                                                  )
                                 )
                            )
                      }

     :wait-rest      {
                      :fn       (fn [[v & rest]]
                                  v)
                      :collect? true
                      }

     :send-msg!      {
                      :fn (fn [msg]
                            (ws/send-transit! (:socket msg) (:msg msg))
                            ::sent
                            )
                      }

     :store-listings {
                      :fn       (fn [[socket listings]]

                                  (let [ids (::ids @*state)
                                        kv-listings (reduce (fn [a o]
                                                              (if-not (ids (:id o))
                                                                      (assoc a (:id o) o)
                                                                      a)
                                                              ) {} listings)

                                        ]
                                       {
                                        (base/rand-sid "store-") [:send-msg! {:socket socket
                                                                              :msg    [:listings kv-listings]
                                                                              }
                                                                  ]
                                        })
                                  )
                      :collect? true
                      :expands? true
                      }

     }
    )

  )

;; avoiding duplicates:
;; a) not returning via expand*
;; b) not including during kv-zipping



(defn scraper-steps [params]
  {
   ::ws [:init-socket "ws:localhost:8081/ws"]

   ::got-ids [:identity (:start-chan params)]

   ::selector [:query-selector-all ".cnt .objava"]
   ::all [:wait-rest [::selector ::got-ids]]

   ;::expand-id [:expand* ::all]

   ;::processed-elements [:process* ::expand-id]
   ::processed-elements [:process* ::all]
   ::RESULT [:collect ::processed-elements]

   ;;
   ;   ::css-1 [:css-rule ".objava { background: #fff; }" ]
   ::css-1 [:css-rule ".woof-custom-listing-ui { font-family: 'DejaVu Sans Mono'; font-size: 7pt; }" ]
   ::css-2 [:css-rule ".woof-listing-hide { opacity: 0.25;}" ]
   ::css-3 [:css-rule ".woof-listing-show { outline: 3px solid crimson;  }" ]




   ;; hacky way to pass the key as a value

   ::k [:mem-k* ::processed-elements]
   ::KV [:*kv-zip [::k ::processed-elements]]

   ::new-listings [:filter-scraped ::KV]

   ::new-ui [:listing-ui* ::new-listings]

   ::save-results [:store-listings [::ws ::new-listings]]

   ;; ::export2console [:export-edn ::new-listings]

   ;; ::post-process [:post-process ::RESULT]

   }
  )
