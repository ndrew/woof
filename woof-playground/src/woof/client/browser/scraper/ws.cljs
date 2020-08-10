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


(defn css-steps [params]
  {
   :css/scraping-test-01 [:css-rule "h1 { text-decoration: underline; }" ]
   ;;   :css/scraping-02 [:css-rules* [".search-item .house-photo" "display: flex;"]]
   ;;  :css/scraping-03 [:css-rules* [".search-item .house-photo img" "max-height: 1  00px"]]
   ;;  :css/hide-map [:css-rules* ["#map_canvas" "display: none"]]
   }
  )

(defn ui-steps [params]
  {

   ;; todo: handle ui update steps here

   }

  )









(defn ws-init [params]

  (let [chan-factory (base/&chan-factory params)]
    {
     ;; or we need just :ws/msg-handler

     :ws/gen-msg-handler (fn []
                           (fn [msg-envelope]
                             ;; got msg via WS
                             (.log js/console (d/pretty! msg-envelope))

                             (try
                               (let [{ws-id :ws-id
                                      [t msg] :msg} msg-envelope]

                                 (cond
                                   (= :scraping/session t)
                                   (let [summary (get msg :summary {})
                                         evt-chan (evt-loop/&evt-loop params)]


                                     ;; we add the hardcoded key
                                     (async/put! evt-chan
                                                 {:session/INITIAL-SUMMARY
                                                  [:identity
                                                   (merge
                                                    {:summary :from-server}
                                                    summary)]}
                                                 )

                                     ;; FIXME: <!> <?> how inject steps further
                                     #_(async/put! evt-chan

                                            ;; here
                                                 (merge
                                                   {(base/rand-sid) [:test msg]}
                                                   ;(parse-steps params)
                                                   ;(css-steps params)
                                                   ;(ui-steps params)
                                                   )


                                                 )



                                     ;; start parsing
                                     ;; need to have evt loop
                                     )
                                   )
                                 )
                               (catch js/Error e
                                 (.error js/console ":ws/gen-msg-handler error:" e)
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

(defn scraper-init [params]
  (let [ws? (&ws? params)]
    (if ws?
      (ws-init params)
      {})))


(defn ws-scraping-session-ctx [params]
  {

   :new-session-msg {:fn (fn[_]
                           [:scraping/session
                            {
                             :host (.. js/window -location -host)
                             ; :url (str (.-location js/window))
                             }
                            ]
                           )}

   :session-msg {
                 :fn (fn [data]

                       [:scraping/data {
                                        ;;:host (.-location .-host  js/window)
                                        :host (.. js/window -location -host)
                                        :url (str (.-location js/window))

                                        :data    [::DUMMY-DATA]
                                        :summary {::DUMMY-SUMMARY (u/now)}

                                        }]
                       )
                 }

   ;;
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

   }

  )


;;
;; CTX
;;
(defn scraper-ctx [params]

  (merge
    evt-loop/EVT-LOOP-CTX-MAP
    (ws-scraping-session-ctx params)

    {
     :test {:fn (fn [v]
                  (.log js/console v)
                  v
                  )

            }
     }
    )
  )

;;
;; STEPS
;;
;; todo: how to handle waiting for scraping session data

;;

(defn scraper-steps [params]


  (let [
        normal-steps (merge
                       (css-steps params)
                       (ui-steps params)
                       {
                        ::test [:test :session/summary]

                        :session/SCRAPED-DATA [:identity [:scraping/data
                                                          {
                                                           ;;:host (.-location .-host  js/window)
                                                           :host (.. js/window -location -host)
                                                           :url (str (.-location js/window))

                                                           :data [{:id (u/now)}]
                                                           :summary {:new-summary (u/now)}
                                                           }
                                                          ]]
                        }
                       )

        ws-steps {

                  ;; init scraping session
                  :ws/init-scraping-session [:ws-send! [:ws/socket :session/init-session-msg]]
        ;; this should add :session/INITIAL-SUMMARY under the hood
                    :ws/socket            [:ws-socket "ws://localhost:8081/scraper-ws"]
                    :session/init-session-msg  [:new-session-msg nil]

                  ;; send scraping session and close
                  :ws/send-scraping-session [:ws-send! [:ws/socket :session/SCRAPED-DATA]]

                  :wf/wait [:wait-rest [:ws/socket :ws/send-scraping-session]]
                  :ws/close [:ws-close! :wf/wait]
                  }


        ]
    (merge
      {
       ::evt-loop [:evt-loop (evt-loop/&evt-loop params)]
       }
      (if (&ws? params) ws-steps
                        {
                         :session/INITIAL-SUMMARY [:identity {}]
                         })

      ;; conditional expand
      {
       ::steps-after-got-scraping-summary [:identity normal-steps]
       ::conditional-steps [:wait-steps [::steps-after-got-scraping-summary
                                        ; and wait for
                                        :session/INITIAL-SUMMARY]
                                        ]


       }

      )
  )
  )
