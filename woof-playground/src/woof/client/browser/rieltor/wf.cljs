(ns woof.client.browser.rieltor.wf
  (:require

    [woof.base :as base]
    [woof.client.dom :as wdom]
    [woof.client.dbg :as dbg :refer [__log]]
    [woof.data :as d]
    [woof.utils :as u]

    [goog.dom :as dom]
    [goog.dom.classes :as classes]

    [goog.dom.dataset :as dataset]
    ;; common wf

    [woof.wfs.evt-loop :as evt-loop]
    [woof.wfs.watcher :as watcher]

    [woof.utils :as u]

    [cljs.core.async :refer [go go-loop] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]
    [clojure.string :as str]

    [rum.core :as rum]
    ))


;; css-... classes seems to be too generic


;; second idea - all tweets are article elements, but inview does not capture them all, so we may try to emulate scroll
;; and to iterate through all article elements manually


;;
;; parsing implementation


(defonce *SCRAPED-DATA (atom []))     ;; todo: should global atom be used for this?
(defonce *PROCESSING-MAP (atom {}))   ;; todo: should global atom be used for this?


(defonce *FAILED (atom []))

(def SCRAPE-SELECTOR ".index-list-container > .catalog-item")


(defn safe-href [el selector]
  (if-let [sub-el (wdom/q el selector)]
    (wdom/attr sub-el "href")
    (do
      (.log js/console "can't find element for selector: " selector "parent-el" el)
      (classes/add el "parsed-error")
      ""
      )
    )
  )

(defn safe-txt [el selector]
  (if-let [sub-el (wdom/q el selector)]

    ;; don't trim for now
    (wdom/txt sub-el)
    (do
      (.log js/console "can't find element for selector: " selector "parent-el" el)
      (classes/add el "parsed-error")
      ""
      )
    )

  )






(defn mark![el parse-id]
  (when el
    (classes/set el "DDD")
    (dataset/set el "parseId" parse-id)
    ))


;; element scraping function
(defn scrape-element [el]
  ; (.warn js/console el (d/pretty! (wdom/dataset el)))

  ;; (swap! *SCRAPED-DATA conj (. (. el -parentElement) -innerHTML))

  ;; saving parsing status in dom
  (when (classes/has el "parsed")
    (.warn js/console "PARSE WAS CALLED TWICE")
    (classes/add el "parsed-twice"))

  (classes/add el "parsed")

  ;; *PROCESSING-MAP

  ;; DIV:nth-child(2) -> catalog-item__info

  ;; classes => label label_no_commission
  ;; classes => label label_new-building
  ;; paid


  ;; .catalog-item__info .catalog-item__title -> .catalog-item__title

  ;; label_new-building
  ".catalog-item__info .catalog-item__title > DIV:nth-child(2) > SPAN:nth-child(2)" "commission",
  ".catalog-item__info .catalog-item__title > DIV:nth-child(2) > SPAN:nth-child(3)" "commission"
  ".catalog-item__info .catalog-item__title > A:nth-child(1)" "address",

  ".catalog-item__info > DIV:nth-child(1) > DIV:nth-child(1) > DIV:nth-child(2)" "house-params",  ;; catalog-item_info-item-row

  ".catalog-item__info .catalog-item_info-item-row > STRONG:nth-child(1)" "price-usd",
  ".catalog-item__info .catalog-item_info-item-row > DIV:nth-child(2)" "price-per-m",

  ".catalog-item__info > DIV:nth-child(2) > EM:nth-child(1)" "listing-date", ;; catalog-item__additional-info

  ".catalog-item__info > DIV:nth-child(3) > DIV:nth-child(1) > A" "agent-url",
  ".catalog-item__info > DIV:nth-child(3) > DIV:nth-child(2) > DIV:nth-child(1) > A",

  ;; "DIV:nth-child(1)" -> catalog-item__img-col [paid]
  ".catalog-item__img-col > DIV > A:nth-child(1) > IMG" "listing-image",
  ".catalog-item__img-col > DIV > A:nth-child(1)" "listing-url"



  (if-let [id (safe-href el ".catalog-item__img-col > DIV > A:nth-child(1)")]
    (do


      (mark! (wdom/q ".catalog-item__img-col > DIV > A:nth-child(1)") "ID")

      (mark! (wdom/q ".catalog-item__info .catalog-item__title") "TITLE-BLOCK")

      (mark! (wdom/q ".catalog-item__info .catalog-item_info-item-row") "PRICE-BLOCK" )

      ;;

      (swap! *SCRAPED-DATA conj
             (merge {:id id})
             )


      ;
      )
    )

  ;; :ok


  #_(let [


        ]

    (if-let [id (safe-href el id_$)]
      (do

        (swap! *SCRAPED-DATA conj
               (merge

                 {
                  :id id

                  ;; todo: exctract WL index from the URL

                  ;:channel (safe-href el channel_$)
                  ;:title (safe-txt el title_$)


                  ;; :el-map (map #(dissoc % :el) (wdom/el-map el :top-selector-fn (fn [base el] { :nth-child (:i base)})))
                  }

                 (if-let [n (wdom/q el "YTD-CHANNEL-NAME YT-FORMATTED-STRING > A")]
                   {:channel-href (wdom/attr n "href")  }
                   {"YTD-CHANNEL-NAME YT-FORMATTED-STRING > A" false
                    :html (.-outerHTML el)
                    })

                 (if-let [n (wdom/q el "A YT-IMG-SHADOW:nth-child(1) IMG")]
                   {:img-src (wdom/attr n "src")  }
                   {"A YT-IMG-SHADOW:nth-child(1) IMG" false
                    :html (.-outerHTML el)
                    })

                 (if-let [n (wdom/q el "#channel-name #text a")]
                   {:channel-title (.-innerText n)  }
                   {"#channel-name #text a" false
                    :html (.-outerHTML el)
                    })

                 (if-let [n (wdom/q el "DIV:nth-child(3)  H3:nth-child(1)  SPAN:nth-child(2)")]
                   {:title (.-innerText n)}
                   {
                    "DIV:nth-child(3)  H3:nth-child(1)  SPAN:nth-child(2)" false
                    :html (.-outerHTML el)
                    })

                 (if-let [n (wdom/q el "YTD-THUMBNAIL-OVERLAY-TIME-STATUS-RENDERER > SPAN:nth-child(2)")]
                   {:duration (.-innerText n)}
                   {
                    "YTD-THUMBNAIL-OVERLAY-TIME-STATUS-RENDERER > SPAN:nth-child(2)" false
                    :html (.-outerHTML el)
                    })




                 #_(if-let [n (wdom/q el "DIV:nth-child(3) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1) > YTD-CHANNEL-NAME:nth-child(1) > DIV:nth-child(1) > DIV:nth-child(1) > YT-FORMATTED-STRING > A")]
                     {:innerText (.-innerText n)}
                     {"DIV:nth-child(3) > H3:nth-child(1) > SPAN:nth-child(2)" false})


                 )
               )
        )

      (do
        (.warn js/console "cannot find the id for element " el)
        )
      )


    )


  )

(defn is-scraped? [el]
  (dataset/has el "woof_id"))

(defn mark-scraped! [el]
  (let [sid (base/rand-sid)]
    ;; mark element as processed
    (dataset/set el "woof_id" sid)
    ))





;;
;; common

(defn _trigger-event [params steps]
  (let [evt-loop (evt-loop/&evt-loop params)]
    (async/put! evt-loop steps)
    )
  )

(defn _tick [params [t max-num]]
  (let [chan-factory (base/&chan-factory params)
        chan (base/make-chan chan-factory (base/rand-sid))]

    (async/go-loop [i 0]
      (async/>! chan (u/now))
      (async/<! (u/timeout t))

      (if (< i max-num)
        (recur (inc i))))

    chan))


(defn wf-clean-up-css []
  ; remove previosly added styles
  (wdom/remove-added-css [
                          ;; for now styles added will persist
                          "parsed-twice"
                          "parsed-error"
                          ; "parsed" "parsed-red" "parsed-magenta" "parsed-brown"
                          ])
  (classes/addRemove
    (.-body js/document) "woof-el" "")

  )



;;
;; brute WF - tries scrolling down until all data are being scraped




(defn wf! [*wf-state meta-info]

  (wf-clean-up-css)

  ;; for now go with local scope, instead of init fn
  (let [;; brute-force scraping, pass all parameters and retrieve, filter els in it
        _simple-brute-force (fn [is-scraped? mark-scraped! process-step selector]
                              (.log js/console "simple scrape: A")

                              ;; try to find elements to be processed, but skip already processed
                              (let [els (filter (fn [el] (not (is-scraped? el)))
                                                (wdom/q* selector))]
                                (.log js/console "els" els (wdom/q* selector))

                                (reduce (fn [a el]
                                          (let [_sid (mark-scraped! el)
                                                sid (if (qualified-keyword? _sid)
                                                      _sid
                                                      (base/rand-sid "el-"))]
                                            (assoc a sid [process-step el])
                                            )
                                          ) {} els)

                                ))

        ;; brute-force scraping via separate find items step (incl. filtering) and separate expand step generation step

        _expander! (fn [collection-expand item-expand els]
                     (reduce (fn [a el]
                               (merge a (item-expand el)))
                             (collection-expand els) els))



        item-expand! (fn [el]
                       (let [_sid (mark-scraped! el)
                             sid (if (qualified-keyword? _sid)
                                   _sid
                                   (base/rand-sid "el-"))]
                         {sid [:scrape-el el]}))


        _infinite-scroll! (fn [params timeout-fn f max-num ]
                            (let [chan-factory (base/&chan-factory params)
                                  chan (base/make-chan chan-factory (base/rand-sid))
                                  t (volatile! (u/now))]

                              (go-loop [i 0]
                                (vreset! t (u/now))

                                (f)
                                (async/<! (u/timeout (timeout-fn)))

                                (if (< i max-num)
                                  (recur (inc i))))

                              chan))


        *brute-force-counter (atom 0)

        ;; re-curring expand steps
        recurring-scrape-expand! (fn [els]
                                   (let [
                                         k_items (base/rand-sid)

                                         k_selector (base/rand-sid)

                                         k_log (base/rand-sid)

                                         k_scroll-wait-time (base/rand-sid)
                                         k_scroll-amount (base/rand-sid)

                                         k_!selector (base/rand-sid)

                                         wait-time (if (empty? els)
                                                     (* 1000 (swap! *brute-force-counter inc))
                                                     (do
                                                       (reset! *brute-force-counter 0)
                                                       5000)
                                                     )
                                         ]


                                     (if (> wait-time (* 15 1000))
                                       (do
                                         {k_log [:log (str "stopping attempts to scrape")]}
                                         )
                                       ;; no more els to scrape - scroll and brute force again
                                       {
                                        k_selector         [:v SCRAPE-SELECTOR]

                                        k_log              [:log (str "recurring scraper scheduled in " wait-time " ms")]

                                        ;; if scroll needed it can be also done
                                        ;; k_scroll-amount    [:scroll (rand-nth [1 2 3])]

                                        k_scroll-wait-time [:v (u/timeout wait-time)]

                                        k_!selector        [:wait-rest [k_selector k_scroll-wait-time]]
                                        k_items            [:find-els k_!selector]

                                        ;;
                                        (base/rand-sid)    [:brute-recurring k_items]
                                        }
                                       )
                                     )
                                   )
        ]

    ;; (.clear js/console)
    (wf-clean-up-css)
    {
     :init  []

     :ctx   [(fn [params]
               {
                :tick               {:fn       (partial _tick params)
                                     :infinite true
                                     }
                :rnd-scroll         {:fn (fn [_]
                                           (rand-nth [1 2 3]))}

                :8-scroll {:fn (partial _infinite-scroll! params
                                        (fn [] + 1000 (int (rand 1000)))
                                        (fn []
                                          (.scrollBy js/window 0  (* (.-innerHeight js/window)
                                                                     (rand-nth [1 2 3])))
                                          )
                                        )
                           :infinite true
                           }


                :scrape-el          {:fn (fn [el]
                                           (try
                                             (scrape-element el)
                                             (catch js/Error e
                                               (do
                                                 (.error js/console e)

                                                 (swap! *FAILED conj
                                                        (wdom/el-map el)
                                                        )

                                                 )
                                               )
                                             )

                                           )}

                ;;
                ;; conditional expand

                ;; brute force approach A

                :brute-force-simple {
                                     :fn       (partial _simple-brute-force
                                                        is-scraped?
                                                        mark-scraped!
                                                        :scrape-el)
                                     :expands? true
                                     :collect? true
                                     }

                ;; brute force approach B

                :find-els           {:fn (fn [selector]
                                           (filter (fn [el] (not (is-scraped? el))) (wdom/q* selector))
                                           #_(take 10 (filter (fn [el] (not (is-scraped? el))) (wdom/q* selector)))

                                           )
                                     }

                :brute-1            {:fn       (partial _expander!
                                                        (fn [] {})
                                                        item-expand!)
                                     :collect? true
                                     :expands? true}

                :brute-recurring    {:fn       (partial _expander!
                                                        recurring-scrape-expand!
                                                        item-expand!)
                                     :collect? true
                                     :expands? true}
                }
               )
             ]
     :steps [
             {

              :css/a2   [:css-rule (str SCRAPE-SELECTOR " { outline: 1px solid red; }")]


              :css/c4   [:css-rule ".parsed { opacity: .6; }"]
              :css/c4_1 [:css-rule ".parsed-twice { background-color: red; }"]
              :css/c4_2 [:css-rule ".parsed-error { background-color: red; outline: 5px solid crimson; }"]
              :css/c5   [:css-rule ".parsed-red { outline: 5px solid red; }"]
              :css/c6   [:css-rule ".parsed-magenta { outline: 5px solid magenta; }"]
              :css/c7   [:css-rule ".parsed-brown { outline: 5px solid brown; }"]


              ;; :css/attr-0 [:css-rule ".DDD { outline: 5px solid blue; }"]



              :css/attr-0 [:css-rules* [".DDD:hover" "outline: 1px solid crimson;"]]
              :css/attr-1 [:css-rules* [".DDD:before" "content: attr(data-parse-id); b \n display: flex; \n background-color: red; \n font-weight: bolder; \n color: white; \n height: 20px; \n outline: 1px solid crimson;"]]

              }

             {
              ::hello [:prn "scraping started"]
              }

             ]

     :opts  [
             (base/build-opt-on-done (fn [params result]
                                       (.warn js/console result)
                                       ))
             ]


     :api   (let [trigger-event (fn [steps] (_trigger-event (get @*wf-state :WF/params {}) steps))]
              (array-map

                "A) trigger scraping: simple" #(trigger-event {(base/rand-sid) [:brute-force-simple SCRAPE-SELECTOR]})
                "B) trigger scraping: brute"  #(trigger-event
                                                (let [k (base/rand-sid)]
                                                  {
                                                    k [:find-els SCRAPE-SELECTOR]
                                                    (base/rand-sid) [:brute-1 k]
                                                   }))

                "C) trigger scraping: recurring" #(trigger-event
                                                  (let [k (base/rand-sid)]
                                                    {
                                                      k [:find-els SCRAPE-SELECTOR]
                                                      (base/rand-sid) [:brute-recurring k]
                                                     }))

                "infinite scroll" (fn []
                                    ;; todo make it togglable
                                    (let [params (get @*wf-state :WF/params {})
                                          evt-loop (evt-loop/&evt-loop params)]
                                      (async/put! evt-loop {
                                                            (base/rand-sid) [:8-scroll 100]
                                                            })
                                      )
                                    )
                "📋FAILED" (fn [] (wdom/copy-to-clipboard @*FAILED))

                "📋RESULT" (fn []
                             (let [data @*SCRAPED-DATA]
                               (.log js/console data)
                               ;(wdom/copy-to-clipboard data)
                               ;(wdom/save-edn (str "rieltor-" (u/now) ".edn") data)
                               ))




                )
              )

     :on-stop (fn [state]
                (__log "ON STOP")
                (.log js/console state)

                ;; can return channel
                )


     }
    )

  )