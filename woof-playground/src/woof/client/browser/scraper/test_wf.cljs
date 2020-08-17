(ns woof.client.browser.scraper.test-wf
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
    [woof.client.dom :as woof-dom]
    [woof.client.ws :as ws]
    [woof.client.browser.scraper.session :as ss]
    [woof.client.browser.scraper.scraping-ui :as sui]

    [woof.wfs.evt-loop :as evt-loop]
    ))


;; scraping session tester


;;
;; web-socket part

(defn &ws? [params] (get params :ws? false))
(defn &summary-chan [params] (get params :ws/summary-chan))



;;
;; parsing part

(defn parse-listing [el]
  (let [headerEl (.querySelector el "header")
        header (dom/getTextContent headerEl)]

    ;; parse prices also
    (merge
      {
       :header header
       :id     (.getAttribute el "id")
       }
      (if (str/starts-with? header "AD:") {:ad? true} {})
      )
    )
  )

(defn parse-ctx [params]
  {
   ;; parsing listings handlers
   :process*           (base/expand-into :process)
   :process            {
                        :fn (fn [el]
                              (try
                                (parse-listing el)
                                (catch js/Error e
                                  (do (.error js/console e el)
                                      {
                                       :ad? true            ;; for now, treat parsed with error as ads
                                       })
                                  )
                                )
                              )
                        }

   ;; filtering
   ;; a) partition results to
   ;; {
   ;; :new       - listings that are not yet in summary
   ;; :duplicate - listings that are in summary, with same prices
   ;; :updated   - listings that are in summary, but with updated prices
   ;; :ad        - ads or invalid listings
   ;; }

   :partition-listings {
                        :fn       (fn [[summary model]]
                                    (group-by (fn [[model el]]
                                                (if (:ad? model)
                                                  :ad
                                                  (if-let [s (get summary (:id model))]
                                                    (do
                                                      (if (= (:header model) (:header s))
                                                        :duplicate
                                                        :updated
                                                        ))
                                                    :new)
                                                  ))
                                              model)
                                    )
                        :collect? true
                        }
   }
  )


;;
;; ui

;; todo: once-init - to add custom stylesheet via dom/add-stylesheet


(defn- on-click [btn handler]
  (goog.events.listen btn goog.events.EventType.CLICK handler))


(defn- wf-ui-impl! []
  (let  [stop-wf-btn-el       (dom/createDom "button" "" "stop WF!")
         panel (dom/createDom "div" "panel")
         ]

    (on-click stop-wf-btn-el  (fn [e] (js* "woof.browser.stop_workflow();")))

    (dom/appendChild panel stop-wf-btn-el)

    (woof-dom/ui-add-el! panel)
    )
  )

(defn ui-ctx [params]
  {
   :mark-new-listing!  (woof-dom/gen-add-css-handler "woof-listing-processed")
   :mark-dup-listing!  (woof-dom/gen-add-css-handler "woof-listing-duplicate")
   :mark-ad-listing!   (woof-dom/gen-add-css-handler "woof-listing-ad")
   :mark-upd-listing!  (woof-dom/gen-add-css-handler "woof-listing-updated")

   :new-listing-ui*    {
                        :fn       (fn [partitioned-listings]
                                    (merge
                                      ;; new
                                      (reduce (fn [a [m el]] (assoc a (base/rand-sid "ui-nu-")  [:mark-new-listing! el])) {} (get partitioned-listings :new []))
                                      ;; duplicate
                                      (reduce (fn [a [m el]] (assoc a (base/rand-sid "ui-dup-") [:mark-dup-listing! el])) {} (get partitioned-listings :duplicate []))
                                      ;; updated
                                      (reduce (fn [a [m el]] (assoc a (base/rand-sid "ui-upd-") [:mark-upd-listing! el])) {} (get partitioned-listings :updated []))
                                      ;; ad or invalid
                                      (reduce (fn [a [m el]] (assoc a (base/rand-sid "ui-ad-")  [:mark-ad-listing! el])) {} (get partitioned-listings :ad []))
                                      {}
                                      ))
                        :expands? true
                        }


   :scraping-ui        {:fn (fn [_]
                              (sui/scraping-ui-impl!)
                              (wf-ui-impl!)
                              )}
   }
  )


;;
;; ws

(defn ws-ctx [params]
  {

   ;;
   ;; ws
   :scraping-msg {
                  :fn (fn [[summary partitioned-listings]]
                        ;; todo: form new summary? or should it be done on the backend side?
                        (let [scraped-data (vec
                                             (map first (concat (get partitioned-listings :new [])
                                                                (get partitioned-listings :updated []))))

                              nu-summary (reduce (fn [a d]
                                                   (assoc a
                                                     (:id d)
                                                     (dissoc d :id))
                                                   )
                                                 (if (empty? summary) {} summary)
                                                 scraped-data)]

                          (ss/scraping-data-msg scraped-data nu-summary))
                        )
                  :collect? true
                  }

   }

  )


;;
;; CTX


(defn scraper-ctx [params]  ;; custom step handlers for current workflow

  (merge
    (parse-ctx params)
    (ui-ctx params)
    (ws-ctx params)
    )
  )

;; todo: use 2 factor state - first have internal state and then merge it with global one



;;
;; STEPS
;;

(defn- evt-loop-steps [params] { ::evt-loop [:evt-loop (evt-loop/&evt-loop params)] })

(defn- css-steps [params]
  {
   :css/id-listing          [:css-rule ".listing { outline: 1px solid gray; }"]

   :css/duplicate-listing   [:css-rule ".woof-listing-duplicate { opacity: 0.4; }"]
   :css/duplicate-listing-1 [:css-rule ".woof-listing-duplicate:before { content: \"DUPLICATE\"; }"]
   :css/processed-listing-1 [:css-rule ".woof-listing-processed:before { content: \"👍\"; }"]

   :css/updated-listing-1   [:css-rule ".woof-listing-updated { background-color: rgba(0,255,128,.233); }"]
   :css/updated-listing-2   [:css-rule ".woof-listing-updated:before { content: \"UPDATED!\"; }"]

   :css/ad-listing     [:css-rules* [".woof-listing-ad" "text-decoration: line-through; \n opacity: 0.4;"]]

   })


;; sub-workflow
(defn parsing-with-summary-steps [params summary-steps]
  (merge
    ;; IN params
    summary-steps


    {
     ;;
     ;; process LISTINGS
     :listings/els*      [:query-selector-all* ".listing"] ;; get all listing elements on the page
     :listings/listings* [:process* :listings/els*] ;; try parsing each listing element

     ;;
     ;; mem
     :mem/collected-listings* [:mem-zip* [:mem/listings* :mem/els*]]
     :mem/listings* [:mem-k* :listings/listings*]
     :mem/els* [:mem-k* :listings/els*]

     ;;
     ;; collect LISTINGS
     :listings/collected-listings* [:collect :mem/collected-listings*]

     ;;
     ;; filter already processed listings
     :listings/LISTINGS-MAP [:partition-listings [:listings/SUMMARY :listings/collected-listings*]]

     }
    )
  )



(defn ws+parse+ui-steps [params]

  (merge
    ;; WS: GLUE
    {
     #_:PARSE     #_:--->   :ws/LISTINGS-MAP [:v :listings/LISTINGS-MAP]
     }
    ;; WS: IN
    {
     :ws/URL           [:v "ws://localhost:8081/scraper-ws"]

     :ws/INITIAL-MSG   [:v (ss/init-scraping-msg)]   #_:--->   :ws/SUMMARY [:v (&summary-chan params)]

     :ws/RESULTS-MSG   [:scraping-msg [:ws/SUMMARY
                                       :ws/LISTINGS-MAP]]
     }
    ;; WS: IMPL - send :ws/INITIAL-MSG, do parsing/processing, send :ws/RESULTS-MSG
    {
     :ws/init-scraping-session [:ws-send! [:ws/socket :ws/INITIAL-MSG]]
     :ws/socket                [:ws-socket :ws/URL]

     :ws/send-scraping-session [:ws-send! [:ws/socket :ws/RESULTS-MSG]]

     ;; add timeout here, so websocket won't be closed too soon, to test /test


     :wf/wait                   [:wait-rest [:ws/socket :ws/send-scraping-session]]
     :ws/close                 [:ws-close! :wf/wait]
     }
    ;; WS: OUT

    ;; PARSE: GLUE
    ;; PARSE: IN - :listings/SUMMARY
    {
     ;;
     :listings/SUMMARY [:identity :ws/SUMMARY]
     }
    ;; PARSE: IMPL
    {
     ;;
     ;; process LISTINGS
     :listings/els*      [:query-selector-all* ".listing"] ;; get all listing elements on the page
     :listings/listings* [:process* :listings/els*] ;; try parsing each listing element

     ;;
     ;; mem
     :mem/collected-listings* [:mem-zip* [:mem/listings* :mem/els*]]
     :mem/listings* [:mem-k* :listings/listings*]
     :mem/els* [:mem-k* :listings/els*]

     ;; ::hello [:&log :mem/collected-listings*]

     ;;
     ;; collect LISTINGS
     :listings/collected-listings* [:collect :mem/collected-listings*]

     ;;
     ;; filter already processed listings
     :listings/LISTINGS-MAP [:partition-listings [:listings/SUMMARY :listings/collected-listings*]]
     }
    ;; PARSE: OUT
    ;; { :listings/LISTINGS-MAP [:v example] }


    ;; UI: IN
    {
     #_:PARSE     #_:--->   :ui/LISTINGS-MAP [:v :listings/LISTINGS-MAP]
     }
    ;; UI: IMPL
    (merge
      (css-steps params)
      {
       :ui/mark-progress [:new-listing-ui* :ui/LISTINGS-MAP]
       }
      {
       :ui/scraping-session [:scraping-ui nil]
       }
      )
    ;; UI: OUT
    ;;
    )
  )

(defn parse+ui-steps [params]

  (merge
    ;; PARSE: GLUE
    ;; PARSE: IN - :listings/SUMMARY
    {
     :listings/SUMMARY [:identity {
                                   ;"listing-2" {:header "booo"}
                                   "listing-3" {:header "Listing 3"}
                                   }]
     }
    ;; PARSE: IMPL
    {
     ;;
     ;; process LISTINGS
     :listings/els*      [:query-selector-all* ".listing"] ;; get all listing elements on the page
     :listings/listings* [:process* :listings/els*] ;; try parsing each listing element

     ;;
     ;; mem
     :mem/collected-listings* [:mem-zip* [:mem/listings* :mem/els*]]
     :mem/listings* [:mem-k* :listings/listings*]
     :mem/els* [:mem-k* :listings/els*]

     ;;
     ;; collect LISTINGS
     :listings/collected-listings* [:collect :mem/collected-listings*]

     ;;
     ;; filter already processed listings
     :listings/LISTINGS-MAP [:partition-listings [:listings/SUMMARY :listings/collected-listings*]]
     }
    ;; PARSE: OUT
    ;; { :listings/LISTINGS-MAP [:v example] }


    ;; UI: IN
    {
     #_:PARSE     #_:--->   :ui/LISTINGS-MAP [:v :listings/LISTINGS-MAP]
     }
    ;; UI: IMPL
    (merge
      (css-steps params)
      {
       :ui/mark-progress [:new-listing-ui* :ui/LISTINGS-MAP]
       }
      {
       :ui/scraping-session [:scraping-ui nil]
       }
      )
    ;; UI: OUT
    ;;
    {
     :log/logA [:log :listings/LISTINGS-MAP]
     }
    )
  )



(defn scraper-steps [params]
  ;; what is proper way of spliting the ws workflow into parts?
     ;; GLUE, IN, IMPL, OUT ?
     ;; should sub-steps be extracted to a fn?

  ;; for now don't use evt loop, so worklow can finish automatically
  ;; (evt-loop-steps params)


  (if (&ws? params)
    (ws+parse+ui-steps params)
    (parse+ui-steps params)
    )

  )

