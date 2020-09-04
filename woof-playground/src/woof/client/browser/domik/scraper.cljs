(ns woof.client.browser.domik.scraper
  (:require

    [cljs.core.async :as async :refer  [go go-loop]]
    [clojure.string :as str]

    [woof.base :as base]
    [woof.client.ws :as ws]
    [woof.client.dom :as woof-dom]
    [woof.data :as d]
    [woof.utils :as u]

    [woof.client.browser.scraper.scraping-ui :as sui]

    [woof.client.browser.domik.listings :as listings]
    [woof.client.browser.domik.houses :as h]
    ))


;; domik.net scrapping

(defn meta-init-fn [params]
  {
   :ws?                   true
   ;; custom on-done
   :wf/display-results-fn (fn [wf-results]
                            (.groupCollapsed js/console "RESULTS")
                            (.log js/console (d/pretty! wf-results))
                            (.groupEnd js/console)

                            wf-results
                            )

   ::debug? false
   })


;; todo: async parsing with confirming via ui button


(defn &ws? [params] (get params :ws? false))
(defn &debug? [params] (get params :debug? false))
(defn &worker-chan [params] (::worker-chan params))


(defn lineriaze-loop [in-chan]
  (go-loop []
           (when-let [[handler out-chan] (async/<! in-chan)]
             (let [ready-chan (handler)
                   val (async/<! ready-chan)]

               (async/put! out-chan val)
               )
             (recur))))



(defn scraper-init [params]
  (let [chan-factory (base/&chan-factory params)
        in-chan (base/make-chan chan-factory (base/rand-sid))]

    (lineriaze-loop in-chan)

    {::worker-chan in-chan}))


(defn scraper-ctx [params]
  {

   :scraping-ui        {:fn (fn [_]
                              ;; todo: provide meta-info here
                              (sui/scraping-ui-impl! {}))}


   :process-sync       {
                        :fn (fn [el]
                              (listings/safe-parse-listing el))
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
                                                   (listings/async-parse-listing c el)
                                                   c))
                                    ]

                                ;; (.log js/console "STEP-HANDLER" el t)

                                (let [outbound-chan (make-chan)]
                                  (async/put! worker-chan [handler-fn outbound-chan])

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
                                                   [:v (listings/safe-parse-listing el)])
                                          )
                                        (array-map) els)

                                      )
                          :expands? true
                          :collect? true
                          }



   ;; todo: convenience wrapper for working with collection with single
   :partition-listings {
                        :fn       (fn [[summary model]]
                                    (listings/partition-listing summary model))
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


   :parse-house {
                 :fn (fn [el]
                       (h/parse-house-details el)
                       )
                 }

   }
  )



(defn parse-listings-steps [params]
   (if (&ws? params)
    (.log js/console "will be using ws")
    )

  ;; parse steps
  (merge
    ;; PARSE: GLUE
    ;; PARSE: IN - :listings/SUMMARY
    {
     :listings/SUMMARY [:identity {}]
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


;;
;; steps to parse house reviews and additional info
(defn parse-house-steps [params]
  {

   :house/$details [:query-selector ".album_housepage_body_wrap"]

   :house/model [:parse-house :house/$details]
   ::hello [:prn :house/model]

   }
  )