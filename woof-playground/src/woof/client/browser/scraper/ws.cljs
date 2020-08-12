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


;; todo: handle meta-ws ws accessors in separate ns
;; accessors
(defn &ws? [params] (get params :ws? false))


;; should this be generified?

(defn process-ws-msg [params msg-envelope]
  (let [{ws-id   :ws-id
         [t msg] :msg} msg-envelope]

    (cond
      (= :scraping/session t)
      (let [_summary (get msg :summary {})
            evt-chan (evt-loop/&evt-loop params)

            summary (merge
                      {}
                      _summary)
            ]

        (.warn js/console msg)

        ;; we emit the summary as hardcoded key
        (async/put! evt-chan
                    {:session/INITIAL-SUMMARY [:identity summary]})

        ;; what if we don't emit :session/INITIAL-SUMMARY — wf will hang?
        )
      )
    )
  )

(defn scraping-data-msg [data summary]
  [:scraping/data
   {
    ;;:host (.-location .-host  js/window)
    :host    (.. js/window -location -host)
    :url     (str (.-location js/window))

    :data    data
    :summary summary
    }
   ]
  )

(defn scraping-session-start-msg []
  [:scraping/session
   {
    :host (.. js/window -location -host)
    :url (str (.-location js/window))
    }
   ])



;;
;; CTX
;;

(defn parse-listing [el]
  ; (.log js/console el)
  (let [a 1
        headerEl (.querySelector el "header")
        ]

    {
     :header (dom/getTextContent headerEl)
     :id (.getAttribute el "id")
     }
    )
  )

(defn custom-listing-ui [summary listing]

  (when-let [el (. js/document (getElementById (:id listing)))]

    (.log js/console (:id listing) (get summary (:id listing)))
    (if-let [processed-el (get summary (:id listing))]
      (do
        (classes/add el "woof-listing-duplicate")
        )
      (classes/add el "woof-listing-processed")
      )

    ;;


    )

  ;; todo: indicate that listing had been parsed  via

  ;; <?>: now we get the parsed data, maybe try to have el + parsed data

  ;; (.log js/console listing)

  :ok
  )

;; todo: once-init - to add custom stylesheet via dom/add-stylesheet

(defn scraper-ctx [params]
  ;; custom step handlers for current workflow
  {

   ;; parsing listings handlers


   :process*    (base/expand-into :process)
   :process     {
                 :fn (fn [el]
                       (try
                         (parse-listing el)
                         (catch js/Error e
                           (.error js/console e el))
                         )

                       )
                 }

   ;; todo: filtering already processed

   ;; custom listing ui
   ;; todo: how to pass :session/INITIAL-SUMMARY here
   :listing-ui* {
                 :fn       (fn [[summary els]]
                                ;; for now pass a hardcoded id of summary here
                             (reduce (fn [a e] (assoc a (base/rand-sid) [:listing-ui [:session/INITIAL-SUMMARY e]])) {} els)
                             )
                 :expands? true
                 :collect? true
                 }

   :listing-ui  {:fn (fn [[summary listing]]
                       (custom-listing-ui summary listing)

                       "ok"
                       )
                 :collect? true
                 }

   :scraping-ui {
                 :fn (fn [_]
                       (let [clear-session-btn-el (dom/createDom "button" "" "clear session")
                             get-session-btn-el (dom/createDom "button" "" "get session")
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

                         ;(.focus btn-el)
                         )
                       )
                 }

   :scraping-msg {
                  :fn       (fn [[summary scraped-data]]
                              (.log js/console summary)
                              ;; todo: form new summary? or should it be done on the backend side?
                              ;; todo: why summary is ()
                              (let [nu-summary (reduce (fn [a d]
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




; (if-let [ui-el (.querySelector existing-el ".woof-custom-listing-ui")]
;      ; update custom ui
;      (dom/setTextContent ui-el (listing-text-ui listing))
;      (let [inner-ui-el (dom/createDom "pre" "woof-custom-listing-ui"
;                                       (listing-text-ui listing))]
;
;        (dom/insertChildAt existing-el inner-ui-el 0)
;        )
;      )


;;
;; STEPS
;;

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

  (merge
    {
     ::evt-loop [:evt-loop (evt-loop/&evt-loop params)]
     }

    ;; ws part 1
    (if (&ws? params)
      {
     ;; init scraping session
       :ws/init-scraping-session [:ws-send! [:ws/socket :session/init-session-msg]]
         :session/init-session-msg [:identity (scraping-session-start-msg)]
         :ws/socket                [:ws-socket "ws://localhost:8081/scraper-ws"]
       ;; => these should add   :session/INITIAL-SUMMARY [:identity summary]
       }
      {;; proceed with empty summary
       :session/INITIAL-SUMMARY [:identity {
                                            ;; fixme: why this is getting converted to ()
                                            ;; :test :id
                                            }]
       })

    (let [css-steps {
                     ;; todo: maybe add css reset?
                     :css/hide-ads [:css-rules* [".ad-listing" "text-decoration: line-through;
                                                                opacity: 0.4;" ]]

                     :css/id-listing [:css-rule ".listing { outline: 1px solid crimson; }"]

                     :css/duplicate-listing [:css-rule ".woof-listing-duplicate { opacity: 0.4; }"]
                     :css/duplicate-listing-1 [:css-rule ".woof-listing-duplicate:before { content: \"DUPLICATE\"; }"]

                     :css/processed-listing-1 [:css-rule ".woof-listing-processed:before { content: \"👍\"; }"]


                     }

          ui-steps {
                    :ui/scraping-session [:scraping-ui nil]
                    }

          parse-steps {
                       ;; try parsing all the listings (even ad-listing or already processed)

                       :ex/__listing-els* [:query-selector-all ".listing"]
                       :ex/parsed-listings* [:process* :ex/__listing-els*]
                       :ex/listings [:collect :ex/parsed-listings*]

                       ;; todo: implemement filtering out ads/invalid listings, or already processed data
                      ;;


                       ;;
                       :ui/listings [:listing-ui* [:session/INITIAL-SUMMARY :ex/parsed-listings*]]


                       ;;


                       :session/SCRAPED-DATA [:scraping-msg [:session/INITIAL-SUMMARY
                                                             :ex/listings ]]

                       }

          ;; todo: implement ui to show scraping session
          ;; todo: add custom ui example

          NORMAL-STEPS (merge
                         css-steps
                         ui-steps
                         parse-steps

                         ;; (ui-steps params)
                         {
                          ;; print out summary for now
                          :log/summary                [:log :session/INITIAL-SUMMARY]

                          ;; todo: get some actual data
                          ;:session/SCRAPED-DATA [:identity (scraping-data-msg [{:id (u/now)}]
                          ;                                                    {:new-summary (u/now)})]

                          }
                         )]
      (if (&ws? params)
        { ;; expand normal steps only after waiting for a key-step :session/INITIAL-SUMMARY

         ::conditional-steps                [:wait-steps [;; expand
                                                          ::steps-after-got-scraping-summary
                                                          ;; wait for
                                                          :session/INITIAL-SUMMARY]]
         ::steps-after-got-scraping-summary [:identity NORMAL-STEPS]
         }
        NORMAL-STEPS)
      )

    (if (&ws? params)
      {
       ;; send scraping session and close
       :ws/send-scraping-session [:ws-send! [:ws/socket :session/SCRAPED-DATA]]
       :wf/wait                  [:wait-rest [:ws/socket :ws/send-scraping-session]]
       :ws/close                 [:ws-close! :wf/wait]
       }
      {})

    {:log/result [:log :session/SCRAPED-DATA]}
    )

  )
