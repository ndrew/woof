(ns woof.client.browser.blago.scraper
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

    [woof.client.browser.blago.listings :as listings]

    [woof.client.browser.scraper.scraping-ui :as sui]

    ))


;; accessors
(defn &ws? [params] (get params :ws? false))
(defn &skip-processed? [params] (get params :ws/skip-processed? false))


;; todo: move meta-init-fn here
;; todo: properly handle ws communication
;; todo: remove *STATE from here


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




;; use global state for now
(def *STATE (atom {
                   ::ids #{}
                   ::socket nil
                   }))


(defn scraper-init [params]
  (let [ws? (&ws? params)]
    (if ws?
      {
       :ws/chan-fn (fn []
                     (let [ws-chan (base/make-chan (base/&chan-factory params)
                                                   (base/rand-sid "ws-"))]

                          ws-chan)
                     )


       :ws/gen-msg-handler (fn []
                             (fn [msg]
                               (.log js/console "!!!-[WS]" msg))
                             )

       ;; what is a good way of sending message to socket
       ;; via separate channel
       ;; or via socket directly

       ;                  :ws/msg-handler (fn [msg]
       ;                                    (.log js/console "[WS]" msg))
       }
      ;{:start-chan (async/chan)}
      {})
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

(defn scraper-ctx [params]
  {

   :scraping-ui        {:fn (fn [_]
                              ;; todo: provide meta-info here
                              (sui/scraping-ui-impl! {}))}

   :scraping-url {:fn (fn[_]
                        [:scraping/session
                         (str (.-location js/window))
                         ]
                        )}

   :ws-send! {:fn (fn [[socket msg]]
                    (ws/send-transit! socket msg)

                    (u/now)
                    )
              :collect? true}
   ;;;;;;;;;;;;;;;

   ;; splits sid-list into
   ;; :process*           (base/expand-into :process)
   :process*           (expand-limited :process 1)

   :process            {
                        :fn (fn [el]
                              (listings/parse-listing el))
                        }

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




(defn scraper-steps [params]
  (let [
        css-steps {
                   ; :css/hide-listings [:css-rule ".cnt { display: none; }"]
                   ;:css/css-1 [:css-rule ".woof-custom-listing-ui { font-family: 'DejaVu Sans Mono'; font-size: 7pt; }" ]
                   ;:css/css-2 [:css-rule ".woof-listing-hide { opacity: 0.25;}" ]
                   ;:css/css-3 [:css-rule ".woof-listing-show { outline: 3px solid crimson;  }" ]

                   :css/scraping-ui [:css-rules* [".search-item" "outline: 1px solid red"]]
                   :css/scraping-01 [:css-rules* [".search-item > .col-md-1" "display: none;"]]
                   :css/scraping-02 [:css-rules* [".search-item .house-photo" "display: flex;"]]
                   :css/scraping-03 [:css-rules* [".search-item .house-photo img" "max-height: 1  00px"]]
                   }


        parse-steps {
                     ;; find listing els for further parsing
                     :blago/__listing-els* [:query-selector-all ".search-item"]

                     :blago/parsed-listings* [:process* :blago/__listing-els*]


                     ;; expose listing els for parser, after we've got list of already processed listings from server
                     ;:domik/listing-els* [:wait-rest [:domik/__listing-els*
                     ;                                 :ws/already-processed-ids]]


                     }

        ui-steps {

                  :ui/scraping-session [:scraping-ui nil]

                  ;; ::new-ui [:listing-ui* :domik/LISTINGS]


                  ;; so they can be copy pasted
                  ;; :ui/print_results [:prn :domik/LISTINGS]

                  ; :clipboard/copy-results [:copy-to-clipboard :domik/LISTINGS]

                  ;; ::ui-progress [:ui-progress :domik/LISTINGS]
                  ;; ::post-process [:post-process ::RESULT]
                  }

        ws-steps {

                  ;; websocket
                  ::ws-socket [:ws-socket "ws://localhost:8081/scraper-ws"]

                  ::current-url [:scraping-url nil]

                  :ws/init-scraping-session [:ws-send! [::ws-socket ::current-url]]
                  ;;::log [:log ::ws]
                  }

        ]

    (merge
      parse-steps
      ;(if skip-processed? filter-results-steps
      ;                    no-filter-results-steps)

      ui-steps
      css-steps

      ws-steps
      )

    )

  )
