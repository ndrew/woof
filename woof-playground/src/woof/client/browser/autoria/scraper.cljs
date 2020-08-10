(ns woof.client.browser.autoria.scraper
  (:require
    [goog.object]
    [goog.dom :as dom]
    [goog.object]
    [goog.dom.classes :as classes]

    [cljs.core.async :as async]
    [woof.base :as base]

    [clojure.string :as str]
    [woof.data :as d]
    [woof.utils :as u]

    [woof.client.ws :as ws]

    [woof.wfs.evt-loop :as evt-loop]
    ))

;; autoria scraper

;; (.clear js/console)


;; accessors
(defn &ws? [params] (get params :ws? false))
(defn &skip-processed? [params] (get params :ws/skip-processed? false))




;; use global state for now
(def *STATE (atom {
                   ::ids #{}
                   ::socket nil
                   ::summary {}
                   }))





(defn dataset [el]
  (js->clj (.parse js/JSON (.stringify js/JSON el.dataset))
           :keywordize-keys true
           )
  )

;; parsing implementation
(defn parse-listing [el]
  ;; (js-debugger)

  (let [nfoEl (.querySelector el "div.hide[data-id]")
        listing-class (.-className el)

        tt-el (.querySelector el ".ticket-title a")

        nfoModel (if nfoEl (dataset nfoEl)
                           {
                            :id (.. el -dataset -advertisementId)
                            :linkToView (.getAttribute tt-el "href")
                            :title (.getAttribute tt-el "title")
                            })

        price-el (.querySelector el ".price-ticket")

        prices (array-seq (.querySelectorAll price-el "[data-currency]"))

        price-map (reduce
          (fn [a el]

            (let [raw-price (str/replace (dom/getTextContent el) #"\s" "")
                  price (js/parseInt (str/replace (dom/getTextContent el) #"\s" "") 10)
                  ]
                 (assoc a
                   (keyword (.. el -dataset -currency))
                   (if (js/isNaN price) :negotiate
                                        price)
                   )

                 )


            )
          {} prices)

        desc-el (.querySelector el ".show-desc")
        base-el (.querySelector el ".base_information")

        attrs-el (array-seq (.querySelectorAll el ".characteristic li"))

        attrs-el (reduce (fn [a el]
                           (assoc a
                                  (keyword (str/replace (.-className (.querySelector el "i")) #"icon-" ""))
                                  (str/trim (dom/getTextContent el)))
                           )
                         {} attrs-el)

        meta-el (.querySelector el ".footer_ticket [data-add-date]")

        meta-new-el (.querySelector el ".footer_ticket > span > span")

        meta-model (if meta-el (dataset meta-el)
                               {:raw-updated (dom/getTextContent meta-new-el)} )


        model (merge
                nfoModel
                (dataset price-el)
                meta-model
                price-map
                attrs-el

                {:desc (if desc-el
                         (dom/getTextContent desc-el)
                         (dom/getTextContent base-el)
                         )

                 :paid? (str/includes? listing-class "paid")
                 }
                )
        ]

    #_(when-not (:USD model)
      (.log js/console el)
      )

    model
    )

  #_[:section.ticket-item.new__ticket.t.paid {:data-text "searchPage_v3" :data-advertisement-id "27219467"} [:div.up-my-offer.person-block.hide] [:div.action-bar [:span.item.checked_ad.hide [:a.unlink.white {:onclick "_gaq.push(['_trackEvent', 'Vin_verification', 'Click_on_badge_report_nais_search_output', 'final_page'])" :href "https://auto.ria.com/auto_volkswagen_jetta_27219467.html"} [:span.icon-car-check] "Перевірений VIN-код"] [:span.js-paidCheckTooltip.popup.__larr.financeua [:span.hide "Проверено AUTO.RIA по базам УБКИ, банков, страховых компаний и официальным дилерским базам."] [:span "AUTO.RIA проверил VIN-код и сравнил информацию от продавца с данными реестров МВД." [:a {:target "_blank" :onclick "_gaq.push(['_trackEvent', 'Search', 'clickon_vin', 'check_history'])" :href "https://auto.ria.com/check_selection/27219467/"} "Проверить всю историю авто"]]]] [:a.item.small-promote-level {:href "javascript:void(0)" :js-withlevelpopup "" :onclick "_gaq.push(['_trackEvent', 'BuSearch', 'ClickOn_top_plashka', 'pop_up'])" :data-toggle "modal" :data-advertisement-id "27219467" :title "25"} "25"] [:span.label-hot.blue.hide "новый"] [:span.label-hot.hide "предзаказ"] [:span.label-hot.nowrap.hide "под заказ"] [:a.item.icon-chips.hide {:href "javascript:void(0)" :data-toggle "modal" :title "0"} "0"] [:a.item {:js-addtonotepad "" :href "javascript:void(0)" :title "Нажмите, чтобы перейти к управлению объявлением в блокноте" :target "_self" :data-autoid "27219467"} [:i.icon-favorite]] [:a.item {:js-compare "" :href "javascript:void(0)" :title "Нажмите, чтобы добавить к сравнению автомобиль" :target "_self" :data-autoid "27219467"} [:i.icon-compare]]] [:div.content-bar [:a.m-link-ticket {:href "https://auto.ria.com/auto_volkswagen_jetta_27219467.html"}] [:div.ticket-photo.loaded [:a.photo-185x120.loaded {:onclick "_gaq.push(['_trackEvent', 'BuSearch', 'ClickOn_ad_photo', 'go_to_ad_page'])" :data-template-v "21" :title "Volkswagen Jetta 2016 в Киеве" :href "https://auto.ria.com/auto_volkswagen_jetta_27219467.html"} [:i.icon-news-video.hide] [:picture [:source {:type "image/webp" :srcset "https://cdn0.riastatic.com/photosnew/auto/photo/volkswagen_jetta__335886005bx.webp"}] [:img.outline.m-auto {:title "Volkswagen Jetta 2016 в Киеве" :alt "Volkswagen Jetta 2016 в Киеве" :src "https://cdn0.riastatic.com/photosnew/auto/photo/volkswagen_jetta__335886005bx.jpg"}]] [:img.hide {:alt "Volkswagen Jetta 2016 в Киеве" :title "Volkswagen Jetta 2016 в Киеве"}]]] [:div.content [:div.head-ticket [:div.item.ticket-title [:a.address {:data-template-v "15" :target "" :title "Volkswagen Jetta 2016 в Киеве" :href "https://auto.ria.com/auto_volkswagen_jetta_27219467.html"} [:span.blue.bold "Volkswagen Jetta S"] "2016"]]] [:div.price-ticket [:span.size15 [:span.bold.green.size22 {:data-currency "USD"} "10 800"] "&nbsp;" [:span.bold.green.size22 "$"] "&nbsp;" [:span.point "•"] "&nbsp;" [:span.i-block [:span {:data-currency "UAH"} "291 492"] "&nbsp;грн"]] [:span.size15.hide [:span.bold.green.size22 {:data-currency "USD"} "договорная"] "&nbsp;"]] [:div.definition-data [:ul.unstyle.characteristic [:li.item-char [:i.icon-mileage {:title "Пробег"}] "77 тыс. км"] [:li.item-char.view-location [:i.icon-location] "Киев"] [:li.item-char [:i.icon-fuel {:title "Тип топлива"}] "Бензин, 1.4 л."] [:li.item-char [:i.icon-transmission {:title "Тип коробки передач"}] "Автомат"]] [:div.base_information] [:input#RadioType-27219467.check-desc {:type "checkbox" :value ""}] [:p.descriptions-ticket.show-desc [:span "Авто в отличном состоянии !
Полностью в ЗАВОДСКОМ ОКРАСЕ! Не утопленник !!!!! Была без переднего бампера !
ТО пройдено ! заменнено масло в маторе и каробке !
НОВАЯ РЕЗИНА !"] [:label {:aria-label "Подробнее" :title "Подробнее" :for "RadioType-27219467"} "..."]]] [:div.footer_ticket [:span {:title "Объявление добавлено 4.07.2020"} [:i.icon-time-grey] [:span "4.07.2020"]] [:div.area.favorite-footer [:a.item.unlink.mhide {:js-compare "" :href "javascript:void(0)" :title "Нажмите, чтобы добавить к сравнению автомобиль" :data-autoid "27219467"} [:i.icon-compare-m]] "&nbsp;" [:a.item.unlink {:js-addtonotepad "" :href "javascript:void(0)" :title "Нажмите, чтобы перейти к управлению объявлением в блокноте" :data-type "UsedAuto" :data-autoid "27219467"} [:i.icon-favorite-head]]]]]]]


  #_[:section.ticket-item.new__ticket.t.paid
   {:data-advertisement-id "26941719" :data-user-id "8906613"}
   [:div.hide {:data-advertisement-data "" :data-user-id "8906613" :data-id "26941719" :data-link-to-view "/auto_ford_focus_26941719.html" :data-mark-name "Ford" :data-model-name "Focus" :data-year "2015" :data-expire-date "2020-07-11 11:38:09" :data-level "109" :data-label "0" :data-level-expire-date "2020-07-11 09:16:10" :data-template_name "searchItem_v4.ect"}]
   [:span.generateDate.hide "Mon Jul 06 2020 21:40:15 GMT+0300 (EEST)"]
   [:div.up-my-offer.person-block.hide]
   [:div.action-bar
    [:span.item.checked_ad [:a.unlink.white {:href "https://auto.ria.com/auto_ford_focus_26941719.html" :onclick "_gaq.push(['_trackEvent', 'Vin_verification', 'Click_on_badge_report_nais_search_output', 'final_page'])"} [:span.icon-car-check] "Перевірений VIN-код"] [:span.popup.__larr.financeua "AUTO.RIA проверил VIN-код и сравнил информацию от продавца с данными реестров МВД." [:a {:href "https://auto.ria.com/check_selection/26941719/" :target "_blank" :onclick "_gaq.push(['_trackEvent', 'Search', 'clickon_vin', 'check_history'])"} "Проверить всю историю авто"]]]
    [:a.item.small-promote-level {:href "javascript:void(0)" :data-advertisement-id "26941719" :js-withlevelpopup "" :title "109" :onclick "_gaq.push(['_trackEvent', 'BuSearch', 'ClickOn_top_plashka', 'pop_up'])" :data-toggle "modal"} "109"]]
   [:div.content-bar
    [:a.m-link-ticket {:href "https://auto.ria.com/auto_ford_focus_26941719.html"}]
    [:div.ticket-photo.loaded [:a.photo-185x120.loaded {:onclick "_gaq.push(['_trackEvent', 'BuSearch', 'ClickOn_ad_photo', 'go_to_ad_page'])" :href "https://auto.ria.com/auto_ford_focus_26941719.html" :title "Ford Focus 2015 в Херсоне" :target "_self"}
                               [:picture [:source {:srcset "https://cdn0.riastatic.com/photosnew/auto/photo/ford_focus__329618155bx.webp" :type "image/webp"}]
                                [:img.outline.m-auto {:loading "lazy" :src "https://cdn0.riastatic.com/photosnew/auto/photo/ford_focus__329618155bx.jpg" :title "Ford Focus 2015 в Херсоне" :alt "Ford Focus 2015 в Херсоне"}]]]]
    [:div.content
     [:div.head-ticket [:div.item.ticket-title [:a.address {:data-template-v "6" :href "https://auto.ria.com/auto_ford_focus_26941719.html" :title "Ford Focus 2015 в Херсоне" :target "_self"} [:span.blue.bold "Ford Focus Business"] "2015"]]]
     [:div.price-ticket {:data-main-currency "USD" :data-main-price "11800"} [:span.size15 [:span.bold.green.size22 {:data-currency "USD"} "11 800"] "&nbsp;" [:span.bold.green.size22 "$"] "&nbsp;" [:span.point "•"] "&nbsp;" [:span.i-block [:span {:data-currency "UAH"} "318 482"] "&nbsp;грн"]]]
     [:div.definition-data [:ul.unstyle.characteristic [:li.item-char [:i.icon-mileage {:title "Пробег"}] "47 тыс. км"]
                            [:li.item-char.view-location [:i.icon-location] "Херсон"]
                            [:li.item-char [:i.icon-fuel {:title "Тип топлива"}] "Бензин, 1 л."]
                            [:li.item-char [:i.icon-akp {:title "Тип коробки передач"}] "Автомат"]]
      [:div.base_information [:span.label-vin [:svg {:fill "none" :height "11" :viewBox "0 0 16 11" :width "16" :xmlns "http://www.w3.org/2000/svg"} [:path {:d "M0.5 3H2L5 0H11L11.75 0.75L8 4.5L6 2.5L4.5 4L8 7.5L13.25 2.25L14 3H15.5C15.7761 3 16 3.22386 16 3.5C16 3.77614 15.7761 4 15.5 4H15V10C15 10.5523 14.5523 11 14 11H13C12.4477 11 12 10.5523 12 10V9H4V10C4 10.5523 3.55228 11 3 11H2C1.44772 11 1 10.5523 1 10V4H0.5C0.223858 4 0 3.77614 0 3.5C0 3.22386 0.223858 3 0.5 3Z" :fill "white"}]] [:span "WF05XXGC" [:span.blue "х"] "5F" [:span.blue "х"] [:span.blue "х"] [:span.blue "х"] [:span.blue "х"] "69"] [:span.popup.__larr.financeua "AUTO.RIA проверил VIN-код и сравнил информацию от продавца с данными реестров МВД." [:a {:href "https://auto.ria.com/check_selection/26941719/" :target "_blank" :onclick "_gaq.push(['_trackEvent', 'Search', 'clickon_vin', 'check_history'])"} "Проверить всю историю авто"]]]]
      [:input#RadioType-26941719.check-desc {:type "checkbox" :value ""}]
      [:p.descriptions-ticket.show-desc [:span "Продам форд фокус 3 рестайлинг, официальная машина не пригонка, с салона выехала в 16 году. Максимальная комплектация business, (двухзонный климат, диодные ходовые огни, круиз контроль, подогревы сиденей и лобового стекла, блютуз телефония sync 2, удаленный запуск двигателя, старт-стоп, складывающиеся зеркала). экономичный турбированный мотор с расходом 7л в городе. Надежная коробка обычный 6ст. автомат, не робот! Полная история обслуживания, ничего делать не нужно, сел и поехал, все записи, подтвержденный пробег, не бит, не путать с Америкой!"] [:label {:aria-label "Подробнее" :title "Подробнее" :for "RadioType-26941719"} "..."]]
      ]
     [:div.footer_ticket
      [:span {:data-add-date "2020-06-11 11:38:09" :data-update-date "2020-07-06 14:10:10" :title "Объявление добавлено 11.06.2020"} [:i.icon-time-grey] [:span. "11.06.2020"]]
      [:div.area.favorite-footer [:a.item.unlink.mhide {:js-compare "" :data-autoid "26941719" :href "javascript:void(0)" :title "Нажмите, чтобы добавить к сравнению автомобиль"} [:i.icon-compare-m]]
       [:a.item.unlink {:js-addtonotepad "" :data-autoid "26941719" :href "javascript:void(0)" :title "Нажмите, чтобы перейти к управлению объявлением в блокноте"} [:i.icon-favorite-head]]]]]
    ]]

  #_(let [

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



;;

(defn css-steps [params]
  {
   ; :css/hide-listings [:css-rule ".cnt { display: none; }"]
   ;:css/css-1 [:css-rule ".woof-custom-listing-ui { font-family: 'DejaVu Sans Mono'; font-size: 7pt; }" ]
   ;:css/css-2 [:css-rule ".woof-listing-hide { opacity: 0.25;}" ]
   ;:css/css-3 [:css-rule ".woof-listing-show { outline: 3px solid crimson;  }" ]


   :css/ria-3 [:css-rule "#searchResults .ticket-item { outline: 1px solid crimson;  }" ]


;;   :css/scraping-ui [:css-rules* [".search-item" "outline: 1px solid red"]]
;;   :css/scraping-01 [:css-rules* [".search-item > .col-md-1" "display: none;"]]
;;   :css/scraping-02 [:css-rules* [".search-item .house-photo" "display: flex;"]]
 ;;  :css/scraping-03 [:css-rules* [".search-item .house-photo img" "max-height: 1  00px"]]

 ;;  :css/hide-map [:css-rules* ["#map_canvas" "display: none"]]
   }
  )

(defn parse-steps [params]
  {

   :ria/__listing-els* [:query-selector-all "#searchResults .ticket-item"]
   :ria/parsed-listings* [:process* :ria/__listing-els*]

   :ria/listings [:collect :ria/parsed-listings*]

   :ws/send-scraping-session [:ws-send! [:ws/socket :ws/data-msg]]
     :ws/data-msg [:session-msg :ria/listings]

   :wf/wait [:wait-rest [:ws/socket :ws/send-scraping-session]]
   :ws/close [:ws-close! :wf/wait]
   ;;:ria/log  [:log :ws/data-msg]



   ;; find listing els for further parsing
;   :blago/__listing-els* [:query-selector-all ".search-item"]

;   :blago/parsed-listings* [:process* :blago/__listing-els*]


 ;  ::listings [:collect :blago/parsed-listings*]

 ;  :ws/data-msg [:session-msg ::listings]

 ;  :blago/log [:log :ws/data-msg]
   ;; todo: uncomment this
   ;; :ws/send-scraping-session [:ws-send! [:ws/socket :ws/data-msg]]



   ;; expose listing els for parser, after we've got list of already processed listings from server
   ;:domik/listing-els* [:wait-rest [:domik/__listing-els*
   ;                                 :ws/already-processed-ids]]


   }
  )

(defn ui-steps [params]
  {

            ;; ::new-ui [:listing-ui* :domik/LISTINGS]


            ;; so they can be copy pasted
            ;; :ui/print_results [:prn :domik/LISTINGS]

            ; :clipboard/copy-results [:copy-to-clipboard :domik/LISTINGS]

            ;; ::ui-progress [:ui-progress :domik/LISTINGS]
            ;; ::post-process [:post-process ::RESULT]
            }

  )

(defn evt-loop-init [params]
  (let [chan-factory (base/&chan-factory params)]
    (evt-loop/build-evt-loop-init-map (base/make-chan chan-factory (base/rand-sid "evt-")))
    )
  )

(defn scraper-init [params]
  ;; todo: extract evt-loop as separate init fn
  (let [ws? (&ws? params)
        chan-factory (base/&chan-factory params)
        ]
    (if ws?
      {
       :ws/chan-fn (fn []
                     (let [ws-chan (base/make-chan chan-factory
                                                   (base/rand-sid "ws-"))]

                          ws-chan)
                     )


       :ws/gen-msg-handler (fn []
                             (fn [msg-envelope]
                               (let [{ws-id :ws-id
                                      [t msg] :msg} msg-envelope]

                                    ;(.log js/console ::->WS t msg)

                                    (cond
                                      (= :scraping/session t)
                                      (let [summary (get msg :summary {})
                                            evt-chan (evt-loop/&evt-loop params)]
                                        (swap! *STATE assoc ::ids (into #{} (keys summary)))

                                        (swap! *STATE assoc ::summary summary)

                                        (async/put! evt-chan

                                                    (merge
                                                      ;;{(base/rand-sid) [:log msg]}

                                                      (parse-steps params)
                                                      (css-steps params)
                                                      (ui-steps params)
                                                      )


                                                    )



                                        ;; start parsing
                                        ;; need to have evt loop
                                        )
                                      )
                                    )
                               )

                             )

       ;; what is a good way of sending message to socket
       ;; via separate channel
       ;; or via socket directly

       ;                  :ws/msg-handler (fn [msg]
       ;                                    (.log js/console "[WS]" msg))
       }
      )

    )
  )



(defn expand-limited [step-id n ]
  {
   :fn (fn [els]
         (reduce (fn [a e] (assoc a (base/rand-sid) [step-id e]))
                 {}
                 (take n els)))
   :expands? true
   }
  )


(defn session-ctx [params]
  {
   :new-session-msg {:fn (fn[_]
                           [:scraping/session
                            {
                             ;;:host (.-location .-host  js/window)
                             :host (.. js/window -location -host)
                             ; :url (str (.-location js/window))
                             }
                            ]
                           )}

   :session-msg {
                 :fn (fn [data]


                       ;; todo: filter out already processed data

                       (let [summary (::summary @*STATE)
                             filtered-data (filter (fn [d]
                                                     (not (get summary (:id d)))
                                                     ) data)
                             new-summary (reduce
                                           (fn [a d]
                                             (assoc a
                                                    (:id d)
                                                    (select-keys d [:mainPrice
                                                                    :updateDate
                                                                    ])
                                                    )
                                             ) {} filtered-data
                                           )
                             ]

                            (when-not (empty? filtered-data)
                              [:scraping/data {
                                               ;;:host (.-location .-host  js/window)
                                               :host (.. js/window -location -host)
                                               :url (str (.-location js/window))

                                               :data filtered-data
                                               :summary new-summary

                                               }]
                              )



                                 )

                       )
                 }

   :ws-close! {:fn (fn [socket]
                     (.close socket)

                     (u/now)
                     )
               :collect? true}

   :ws-send! {:fn (fn [[socket msg]]
                    (if (or (= :nil msg) (nil? msg))
                        (.log js/console "not sending an empty msg")
                        (ws/send-transit! socket msg)
                        )


                    (u/now)
                    )
              :collect? true}

   :wait-rest      {
                    :fn       (fn [[v & rest]]
                                v)
                    :collect? true
                    }
   }

  )

(defn scraper-ctx [params]
  (merge
    evt-loop/EVT-LOOP-CTX-MAP
    (session-ctx params)
    {

     ;;;;;;;;;;;;;;;

     ;; splits sid-list into
     ;; :process*           (expand-limited :process 1)
     :process*           (base/expand-into :process)

     :process            {
                          :fn (fn [el]
                                (try
                                  (parse-listing el)
                                  (catch js/Error e
                                      (.error js/console e el))
                                  )


                                )
                          }

     ;;;

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
  )




(defn scraper-steps [params]
  (let [
        ws-steps {

                   ;; websocket
                  :ws/init-scraping-session [:ws-send! [:ws/socket :ws/init-session-msg]]
                      :ws/socket               [:ws-socket "ws://localhost:8081/scraper-ws"]
                      :ws/init-session-msg     [:new-session-msg nil]

                  }

        evt-loop-steps {
                        ::evt-loop [:evt-loop (evt-loop/&evt-loop params)]
                        }
        ]

    (if (&ws? params)
      (merge
        evt-loop-steps
        ws-steps
        )
      ;; else - no ws

      (merge
        evt-loop-steps


        (parse-steps params)
        ;(if skip-processed? filter-results-steps
        ;                    no-filter-results-steps)

        (ui-steps params)
        (css-steps params)
        )

      )


    )

  )
