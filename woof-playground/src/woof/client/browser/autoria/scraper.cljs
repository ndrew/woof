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
