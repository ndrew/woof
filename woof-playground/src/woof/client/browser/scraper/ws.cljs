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
                      {:summary :from-server}
                      _summary)
            ]

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

(defn scraper-ctx [params]
  ;; custom step handlers for current workflow
  {

   ;; parsing listings handlers
   :process* (base/expand-into :process)

   :process  {
              :fn (fn [el]
                    (try
                      (parse-listing el)
                      (catch js/Error e
                        (.error js/console e el))
                      )

                    )
              }

   :scraping-msg {
                  :fn (fn [[summary scraped-data]]
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
                     }

          parse-steps {
                       ;; try parsing all the listings (even ad-listing or already processed)

                       :ex/__listing-els* [:query-selector-all ".listing"]
                       :ex/parsed-listings* [:process* :ex/__listing-els*]

                       :ex/listings [:collect :ex/parsed-listings*]


                       ;; todo: implemement filtering out ads/invalid listings, or already processed data

                       :session/SCRAPED-DATA [:scraping-msg [:session/INITIAL-SUMMARY
                                                             :ex/listings ]]

                       }

          ;; todo: implement ui to show scraping session
          ;; todo: add custom ui example

          NORMAL-STEPS (merge
                         css-steps

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
