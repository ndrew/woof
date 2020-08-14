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

    [woof.wfs.evt-loop :as evt-loop]
    ))


;; scraping session tester


;;
;; web-socket part

;; todo: handle meta-ws ws accessors in separate ns accessors
(defn &ws? [params] (get params :ws? false))

(defn &summary-chan [params] (get params :summary/chan))


(defn init-fn [params]
  ;; pass _ws-init here?


  (let [chan-factory (base/&chan-factory params)
        summary-chan (base/make-chan chan-factory (base/rand-sid))

        ws-init-fn (partial ws/_ws-init-fn
                            (fn [params msg-envelope]
                              (let [{ws-id   :ws-id
                                     [t msg] :msg} msg-envelope]

                                (cond
                                  (= :scraping/session t)
                                  (let [summary (get msg :summary {})
                                        evt-chan (evt-loop/&evt-loop params)]

                                    ; (.warn js/console msg)

                                    (async/put! summary-chan summary)

                                    ;; for not not use evt-chan
                                    #_(async/put! evt-chan
                                                {
                                                 ;; is not working
                                                 ;; :session/INITIAL-SUMMARY [:identity summary]

                                                 :log/test   [:log "GOT SCRAPING SESSION"]
                                                 :log/test-1 [:log :session/INITIAL-SUMMARY]

                                                 })

                                    ;; what if we don't emit :session/INITIAL-SUMMARY — wf will hang?
                                    )
                                  )
                                )
                              ))

        ]

    (merge
      {
       :summary/chan summary-chan
       }
      (ws-init-fn params)
      )
    )
  )




(defn scraping-data-msg [data summary]
  [:scraping/data
   {
    :data    data
    :summary summary

    :host    (.. js/window -location -host)
    :url     (str (.-location js/window))
    }
   ]
  )

(defn scraping-session-start-msg []
  [:scraping/session
   {
    :host (.. js/window -location -host)
    :url (str (.-location js/window))
    }])


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

                                      (scraping-data-msg scraped-data nu-summary)
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
(defn scraper-steps-parsing [params summary-steps]
  (merge
    ;; IN params
    summary-steps

    (css-steps params)
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
     :listings/LISTINGS-MAP [:partition-listings [:session/INITIAL-SUMMARY :listings/collected-listings*]]
     :ui/mark-progress [:new-listing-ui* :listings/LISTINGS-MAP]

     ;; join :new and :updated listings
     ;; :listings/partitioned-listings

     ;;
     ;; :log/k [:log :listings/LISTINGS-MAP]
     }
    )
  )



(defn scraper-steps [params]

  ;; examaple of conditional (affected by meta data) steps

  ;; steps are being composed by merging smaller steps
  ;; some of these will have a 'keyframe' steps - with defiined name (usually in uppercase)

  ;;      evt loop
  ;; ws?  ws-receive-summary | dummy-summary => :session/INITIAL-SUMMARY
  ;;      ui
  ;;      parsing
  ;; ws?  ws-send-scraped | {}
  ;;      result

  ;; factor these without

  (merge
    ;; (evt-loop-steps params)        ;; for now don't use evt loop, so worklow can finish automatically
    (scraper-steps-parsing params
      (if (&ws? params)
        {
         ;; init scraping session
         :ws/init-scraping-session [:ws-send! [:ws/socket :session/init-session-msg]]
         :session/init-session-msg [:identity (scraping-session-start-msg)]
         :ws/socket                [:ws-socket "ws://localhost:8081/scraper-ws"]

         :session/INITIAL-SUMMARY [:identity (&summary-chan params)]
         ; :session/INITIAL-SUMMARY [:identity {}]
         }
        {
         :session/INITIAL-SUMMARY [:identity {
                                     ;"listing-2" {:header "booo"}
                                     ;"listing-3" {:header "Listing 3"}
                                  }]
         }
        ))
    {
     :ui/scraping-session [:scraping-ui nil]
       ;; :log/log-summary [:log :session/INITIAL-SUMMARY]
     }
    (if (&ws? params)
      {
       ;;
       :session/scraping-data [:scraping-msg [:session/INITIAL-SUMMARY
                                              :listings/LISTINGS-MAP ]]

       ;; send scraping session and close
       :ws/send-scraping-session [:ws-send! [:ws/socket :session/scraping-data]]
       :wf/wait                  [:wait-rest [:ws/socket :ws/send-scraping-session]]
       :ws/close                 [:ws-close! :wf/wait]


       ;; :log/result [:log :session/scraping-data]
       }
      {

       })

    ;;{:log/result [:log :session/SCRAPED-DATA]}

    )

  )

