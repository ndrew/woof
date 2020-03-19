(ns ^:figwheel-hooks ^:figwheel-always
  woof.browser
  (:require
    [goog.object]
    [goog.dom :as dom]
    [goog.object]
    [goog.dom.query :as query]
    [goog.dom.classes :as classes]

    [woof.data :as d]

    [woof.base :as base]

    [woof.client.ws :as ws]
    [woof.utils :as u]

    [woof.scraper :as scraper]
    [clojure.core.async :as async]

    ))


;; ns for doing in-browser scraping

;; for now, build as :browser-min and inject in the host page as
;   (function() { var $script = document.createElement('script'); $script.setAttribute("type","text/javascript"); $script.setAttribute("src", "http://localhost:9500/cljs-out/browser-main.js"); document.body.appendChild($script); })()

;; or copy page contents into browser.html


(enable-console-print!)


;; use global state for now
(def *STATE (atom {
                   ::ids #{}
                   ::socket nil
                   }))



(defn simple-ctx [params]
  {
   :identity {:fn identity }

   :log {:fn (fn[v]
               ;(prn v)
               (.log js/console v)
               v)}
   :export-edn {:fn (fn[v]
                      (prn v)
               "")}

   }
  )



(defn simple-opt[params]
  {
   :op-handlers-map {
                     :done  (fn [result]

                              (.log js/console result)

                              (.log js/console
                                    "RESULT"
                                    (::RESULT result))


                              )

                     :error (fn [result]
                              (.error js/console result))

                     }

   })


(defn scraper-ctx [params]
    {

     ;; gets html elements
     :query-selector-all {
                          :fn (fn [selector]
                                (array-seq (.querySelectorAll (.-body js/document) selector)))
                          }

     ;; splits elements to a separate step
     :expand*            (base/expand-into :identity)

     :collect            {
                          :fn       (fn [xs]
                                      ; (.warn js/console xs)
                                      xs)
                          :collect? true
                          }

     ;; splits sid-list into
     :process*           (base/expand-into :process)

     :process            {
                          :fn (fn [el]
                                (scraper/parse-listing el))
                          }

     :listing-ui*        (base/expand-into :listing-ui)
     :listing-ui         {
                          :fn (fn [listing]
                                (scraper/custom-ui listing)

                                "ok"
                                )
                          }
     :add-listing-css    {
                          :fn (fn [rule]
                                (let [style-el (.createElement js/document "style")]

                                     (.appendChild (.-head js/document) style-el)

                                     (let [sheet (.-sheet style-el)]
                                       (.insertRule sheet rule)
                                       )
                                     )
                                true
                                )

                          }


     :mem-k*             {
                          :fn       (fn [o]
                                      {(base/rand-sid "mem-k-") [:identity {:k o}]})
                          :expands? true
                          }

     ;; kv zipping - joins keys with values
     :*kv-zip            {
                          :fn       (fn [[[k] vs]]
                                      (let [ks (:k k)]
                                           (apply assoc {} (interleave ks vs))
                                           ))
                          :collect? true
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

(defn ws-ctx-fn [params]
  ;; "ws:localhost:8081/ws"
  (let [*state (atom {
                      ::socket nil
                      ::ids #{}
                      ::listings {}
                      })

        ]
    {
     :init-socket    {
                      :fn (fn [url]

                            (let [ch (async/chan)
                                  first-ids (volatile! false)
                                  socket (ws/connect url
                                                     :on-open (fn []
                                                                (.log js/console "opened")
                                                                ;; strange, but working
                                                                (async/put! ch (::socket @*state))
                                                                )
                                                     :on-message (fn [payload]

                                                                   (let [[t body] (:msg payload)]
                                                                        (when (= t :ids)
                                                                          (swap! *state assoc :ids body)
                                                                          (when-not @first-ids
                                                                            (async/put! (:start-chan params) true)
                                                                            (vswap! first-ids not)
                                                                            )

                                                                          )

                                                                        (.log js/console "PAYLOAD" payload)
                                                                        )

                                                                   )
                                                     )]

                                 (swap! *state assoc ::socket socket)
                                 ;{:socket socket}
                                 ch
                                 )
                            )
                      }


     :wait-rest      {
                      :fn       (fn [[v & rest]]
                                  v)
                      :collect? true
                      }

     :send-msg!      {
                      :fn (fn [msg]
                            (ws/send! (:socket msg) (:msg msg))
                            ::sent
                            )
                      }

     :store-listings {
                      :fn       (fn [[socket listings]]

                                  (let [ids (::ids @*state)
                                        kv-listings (reduce (fn [a o]
                                                              (if-not (ids (:id o))
                                                                      (assoc a (:id o) o)
                                                                      a)
                                                              ) {} listings)

                                        ]
                                       {
                                        (base/rand-sid "store-") [:send-msg! {:socket socket
                                                                              :msg    [:listings kv-listings]
                                                                              }
                                                                  ]
                                        })
                                  )
                      :collect? true
                      :expands? true
                      }

     }
    )

  )

;; avoiding duplicates:
;; a) not returning via expand*
;; b) not including during kv-zipping


(defn scraper-steps [params]
  {
   ::ws [:init-socket "ws:localhost:8081/ws"]


   ::got-ids [:identity (:start-chan params)]

   ::selector [:query-selector-all ".cnt .objava"]
   ::all [:wait-rest [::selector ::got-ids]]

   ;::expand-id [:expand* ::all]

   ;::processed-elements [:process* ::expand-id]
   ::processed-elements [:process* ::all]
   ::RESULT [:collect ::processed-elements]

   ;;
;   ::css-1 [:add-listing-css ".objava { background: #fff; }" ]
   ::css-1 [:add-listing-css ".woof-custom-listing-ui { font-family: 'DejaVu Sans Mono'; font-size: 7pt; }" ]
   ::css-2 [:add-listing-css ".woof-listing-hide { opacity: 0.25;}" ]
   ::css-3 [:add-listing-css ".woof-listing-show { outline: 3px solid crimson;  }" ]




   ;; hacky way to pass the key as a value

   ::k [:mem-k* ::processed-elements]
   ::KV [:*kv-zip [::k ::processed-elements]]

   ::new-listings [:filter-scraped ::KV]

   ::new-ui [:listing-ui* ::new-listings]

   ::save-results [:store-listings [::ws ::new-listings]]

   ;; ::export2console [:export-edn ::new-listings]

   ;; ::post-process [:post-process ::RESULT]

   }
  )



(def init-fns   [(fn [params]
                   {:start-chan (async/chan)}
                   )])
(def ctx-fns    [simple-ctx scraper-ctx ws-ctx-fn])
(def steps-fns  [
                 scraper-steps
                 ])

(def opt-fns    [simple-opt])




(defn ^:export run_workflow []
  ;; this will start the wf
  (let [wf-impl
        (base/parametrized-wf!
          (base/combine-init-fns init-fns)
          identity ; wf-params-fn
          identity ; opt-params-fn
          (base/combine-fns opt-fns :merge-results base/merge-opts-maps)
          (base/combine-fns ctx-fns)
          (base/combine-fns steps-fns))
        ]
    (base/run-wf! wf-impl identity)
    )
  )







;; todo: add autoscroll




(when-not (goog.object/get js/window "PLAYGROUND")

  (.clear js/console)
  (run_workflow)

  )
