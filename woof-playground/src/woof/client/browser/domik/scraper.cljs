(ns ^:figwheel-hooks woof.client.browser.domik.scraper
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


;; domik.net scrapping

;;
;; mapping decisions for wf


(defn &ws? [params] (get params :ws? false))


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

(defn partition-listing [summary model]
  (group-by (fn [[model el]]
              (let [g (group-listing summary model el)]
                (listing-ui! el g model)
                g
                ))
            model)
  )


;;
;;;;

(defn lineriaze-loop [in-chan]
  (go-loop []
           (when-let [[handler out-chan] (async/<! in-chan)]
             (let [ready-chan (handler)
                   val (async/<! ready-chan)]

               (async/put! out-chan val)
               )
             (recur)
             )
           )
  )


(defn queue! [worker-chan handler out-channel]
  (async/put! worker-chan [handler out-channel])
  )


(defn &worker-chan [params]
  (::worker-chan params)
  )


(defn scraper-init [params]
  (let [chan-factory (base/&chan-factory params)
        in-chan (base/make-chan chan-factory (base/rand-sid))]


    (lineriaze-loop in-chan)

    {::worker-chan in-chan}
    )
  )

(defn scraper-ctx [params]
  {

   :scraping-ui        {:fn (fn [_]
                              (sui/scraping-ui-impl!))}





   :process-sync       {
                        :fn (fn [el]
                              (safe-parse-listing el))
                        }

   :process-sync*           (base/expand-into :process-sync)


   :process-async      {
                        :fn (fn [el]

                              (let [
                                    make-chan (fn [] (base/make-chan (base/&chan-factory params) (base/rand-sid)))
                                    worker-chan (&worker-chan params)
                                    t (u/now)

                                    outbound-chan (make-chan)

                                    handler-fn (fn []
                                                 ;; (.log js/console "WORKER" el t (- (u/now) t))

                                                 (let [c (make-chan)]
                                                   (async-parse-listing c el)
                                                   c))
                                    ]

                                ;; (.log js/console "STEP-HANDLER" el t)

                                (let [outbound-chan (make-chan)]
                                  (queue! worker-chan
                                          handler-fn
                                          outbound-chan)

                                  outbound-chan
                                  )
                                )

                              )
                        }

   :process-async*           (base/expand-into :process-async)


   :*process-all         {:fn       (fn [els]
                                      (reduce
                                        (fn [a el]
                                          (assoc a (base/rand-sid)
                                                   [:v (safe-parse-listing el)])
                                          )
                                        (array-map) els)

                                      )
                          :expands? true
                          :collect? true
                          }



   ;; todo: convenience wrapper for working with collection with single
   :partition-listings {
                        :fn       (fn [[summary model]]
                                    (partition-listing summary model))
                        :collect? true
                        }

   :listings-to-send   {
                        :fn (fn [listing-map]
                              (map (fn [[listing _]]
                                     listing
                                     )
                                   (concat [] (get listing-map :new [])
                                           (get listing-map :updated [])))
                              )
                        }

   ;; todo: confirmation of the resulting data

   :post-process       {
                        :fn (fn [listings]
                              (sort-by
                                :uah
                                (map #(get % :price) listings)
                                )

                              )
                        }

   }
  )




(defn scraper-steps [params]

  #_(if (&ws? params)
    (.log js/console "will be using ws")
    )


  ;; parse steps
  (merge
    ;; PARSE: GLUE
    ;; PARSE: IN - :listings/SUMMARY
    {
     :listings/SUMMARY [:identity {

                                   }]
     }
    ;; PARSE: IMPL
    {

     ;; find listing els for further parsing
     :domik/els* [:query-selector-all* ".cnt .objava"]


     ;; 3 ways of processing found dom elements

     ;; a) async: (base/expand-into :process) - each element is being parsed separately. fastest option
     ;:domik/listings* [:process-async* :domik/els*]

     ;; b) sync: (base/expand-into :process-sync) - one element at a time is being processed. TODO: linearized impl can be used to debug if parsing is correct.
     :domik/listings* [:process-sync* :domik/els*]

     ;; c) non-expand step — all elements are being processed at once
     ;;
     ;:domik/listings* [:*process-all :domik/els*]


     ;;
     ;; mem
     :mem/zip-listings+els* [:mem-zip* [:mem/listings* :mem/els*]]
        :mem/listings* [:mem-k* :domik/listings*]
        :mem/els* [:mem-k* :domik/els*]

     ;;
     ;; collect LISTINGS
     :domik/listings+els [:collect :mem/zip-listings+els*]

     ;;
     ;; filter already processed listings
     :listings/LISTINGS-MAP [:partition-listings [:listings/SUMMARY :domik/listings+els]]

     ;; todo: implement check if step handler is waiting too long for argument


     :listings/to-be-sent [:listings-to-send :listings/LISTINGS-MAP]

     ;; todo: actually send listings
     ::hello [:prn-seq :listings/to-be-sent]

     }
    {

     :ui/scraping-session [:scraping-ui nil]

     :css/scraping-ui-1 [:css-rule ".woof-listing-model { font-family: 'DejaVu Sans Mono'; font-size: 7pt; }" ]
     :css/scraping-ui-2 [:css-rule "details.woof-custom-listing-ui { background-color: #dfdfff; font-size: 16pt; }" ]
     :css/scraping-ui-3 [:css-rules* ["summary:focus" "outline-style: none;"]]


     :css/duplicate-listing   [:css-rule ".woof-listing-duplicate { opacity: 0.4; }"]
     :css/duplicate-listing-1 [:css-rule ".woof-listing-duplicate:before { content: \"DUPLICATE\"; }"]

     :css/processed-listing-1 [:css-rule ".woof-listing-processed:before { content: \"👍\"; }"]

     :css/updated-listing-1   [:css-rule ".woof-listing-updated { background-color: rgba(0,255,128,.233); }"]
     :css/updated-listing-2   [:css-rule ".woof-listing-updated:before { content: \"UPDATED!\"; }"]

     :css/ad-listing     [:css-rules* [".woof-listing-ad" "text-decoration: line-through; \n opacity: 0.4;"]]

     }
    )

  )
