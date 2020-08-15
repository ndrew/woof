(ns woof.client.browser.scraper.ws
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

    [woof.wfs.evt-loop :as evt-loop]


    ))


;; scraping session tester


;;
;; web-socket part

;; todo: handle meta-ws ws accessors in separate ns accessors
(defn &ws? [params] (get params :ws? false))

(defn &summary-chan [params] (get params :ws/summary-chan))


;; TODO: find

(defn init-fn [params]
  ;; injects: :ws/summary-chan - for returning summary via ws
  ;;          :ws/chan-fn
  ;;          :ws/gen-msg-handler
  (let [chan-factory (base/&chan-factory params)
        summary-chan (base/make-chan chan-factory (base/rand-sid))

        ;; should this be in ss?
        msg-fn (fn [params msg-envelope]
                 (let [{ws-id   :ws-id
                        [t msg] :msg} msg-envelope]

                   (cond
                     (= :scraping/session t)
                     (let [summary (get msg :summary {})]
                       ; (.warn js/console "GOT" msg)

                       ;; propagate summary further via separate channel
                       (async/put! summary-chan summary)

                       ;; other way is via injecting a specific key-step via event loop
                       ;; for now we don't use evt-chan
                       #_(let [evt-chan (evt-loop/&evt-loop params)]
                           (async/put! evt-chan
                                       {
                                        ;; :ws/SUMMARY [:identity summary]
                                        }))

                       ))))

        ws-init-fn (partial ws/_ws-init-fn msg-fn)
        ]

    (merge
      {:ws/summary-chan summary-chan}
      (ws-init-fn params))
    )
  )




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

;;
;; ui

;; todo: once-init - to add custom stylesheet via dom/add-stylesheet

(defn- gen-add-css-handler [class]
  {:fn (fn [el]
         (classes/add el class)

         true)
   })


(defn- scraping-ui-impl! []
  (let [clear-session-btn-el (dom/createDom "button" "" "clear session")
        get-session-btn-el (dom/createDom "button" "" "get session")

        stop-wf-btn-el (dom/createDom "button" "" "stop WF!")
        ]

    (goog.events.listen get-session-btn-el goog.events.EventType.CLICK
                        (fn [e]
                          (ws/GET "http://localhost:8081/scraping-session"
                                  (fn [raw-edn]
                                    (.log js/console raw-edn)
                                    )
                                  )
                          ))

    (goog.events.listen clear-session-btn-el goog.events.EventType.CLICK
                        (fn [e]
                          (ws/GET "http://localhost:8081/clear-scraping-session"
                                  (fn [raw-edn]
                                    (.log js/console raw-edn)
                                    )
                                  )
                          ))
    (woof-dom/ui-add-el! get-session-btn-el)
    (woof-dom/ui-add-el! clear-session-btn-el)

    (goog.events.listen stop-wf-btn-el
                        goog.events.EventType.CLICK
                        (fn [e]
                          (js* "woof.browser.stop_workflow();")
                          ))

    (woof-dom/ui-add-el! stop-wf-btn-el)

    ;(.focus btn-el)
    )
  )


;;
;; CTX

(defn scraper-ctx [params]                                  ;; custom step handlers for current workflow

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

   :mark-new-listing!  (gen-add-css-handler "woof-listing-processed")
   :mark-dup-listing!  (gen-add-css-handler "woof-listing-duplicate")
   :mark-ad-listing!   (gen-add-css-handler "woof-listing-ad")
   :mark-upd-listing!  (gen-add-css-handler "woof-listing-updated")

   :new-listing-ui*    {
                        :fn       (fn [partitioned-listings]
                                    (merge
                                      ;; new
                                      (reduce (fn [a [m el]] (assoc a (base/rand-sid "ui-nu-") [:mark-new-listing! el])) {} (get partitioned-listings :new []))
                                      ;; duplicate
                                      (reduce (fn [a [m el]] (assoc a (base/rand-sid "ui-dup-") [:mark-dup-listing! el])) {} (get partitioned-listings :duplicate []))
                                      ;; updated
                                      (reduce (fn [a [m el]] (assoc a (base/rand-sid "ui-upd-") [:mark-upd-listing! el])) {} (get partitioned-listings :updated []))
                                      ;; ad or invalid
                                      (reduce (fn [a [m el]] (assoc a (base/rand-sid "ui-ad-") [:mark-ad-listing! el])) {} (get partitioned-listings :ad []))
                                      {}
                                      ))
                        :expands? true
                        }


   :scraping-ui        {:fn (fn [_] (scraping-ui-impl!))}

   ;;
   ;; ws
   :scraping-msg       {
                        :fn       (fn [[summary partitioned-listings]]
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

                                      (ss/scraping-data-msg scraped-data nu-summary)
                                      )
                                    )
                        :collect? true
                        }

   }
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

