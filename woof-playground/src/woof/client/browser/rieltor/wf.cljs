(ns woof.client.browser.rieltor.wf
  (:require
    [goog.dom :as dom]
    [goog.dom.classes :as classes]
    [goog.dom.dataset :as dataset]

    [cljs.core.async :refer [go go-loop] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]
    [clojure.string :as str]

    [woof.base :as base]
    [woof.data :as d]

    [woof.client.dom :as wdom]
    [woof.client.dbg :as dbg :refer [__log]]
    [woof.client.ws :as ws]

    [woof.utils :as u]

    [woof.wfs.evt-loop :as evt-loop]
    [woof.wfs.watcher :as watcher]

    [cljs-time.core :as time]
    [cljs-time.format :as time-fmt]

    ;; todo:
    [woof.client.browser.scraper.scraping-ui :as sui]

    [woof.client.browser.rieltor.ui :as wf-ui]
    [woof.client.browser.rieltor.parser :as parser]

    [rum.core :as rum]
    ))


;; css-... classes seems to be too generic

;; second idea - all tweets are article elements, but inview does not capture them all, so we may try to emulate scroll
;; and to iterate through all article elements manually


(def ALLOW-DOUBLE-PARSE true)
(def SCRAPE-SELECTOR ".index-list-container > .catalog-item")

;;
;; parsing implementation

(defonce *SCRAPED-DATA (atom []))     ;; todo: should global atom be used for this?
(defonce *PROCESSING-MAP (atom {}))   ;; todo: should global atom be used for this?

(defonce *FAILED (atom []))


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


(defn ->price [$PRICE $PRICE-M2]
  ;; "250 000 $" or hrn
  (let [price-text (-> $PRICE
                       (wdom/txt)
                       (str/replace " " ""))
        price-m2-text (-> $PRICE-M2
                          (wdom/txt)
                          (str/replace " " "")
                          ) ;; "2 326 $/м²"

        xchange-price-text (wdom/attr $PRICE "title")  ;; " По курсу НБУ - 3 291 123 грн / 65 977 грн/м²"
        [xchange-price xchange-price-m2] (-> xchange-price-text
                                             (str/trim)
                                             (str/replace "По курсу НБУ - " "")
                                             (str/replace " " "")
                                             (str/split "/"))

        price->curr (partial re-find #"^(\d+)(.+)$")
        ]

    (merge
      (let [[_ p c] (price->curr price-text)]
        {(if (= "$" c) :USD :UAH) (d/to-primitive p)})
      (let [[_ p c] (price->curr xchange-price)]
        {(if (= "$" c) :USD :UAH) (d/to-primitive p)})
      (let [[_ p c] (price->curr price-m2-text)]
        {(if (= "$/м²" c) :USD_M2 :UAH_M2) (d/to-primitive p)})
      (let [[_ p c] (price->curr xchange-price-m2)]
        {(if (= "$/м²" c) :USD_M2 :UAH_M2) (d/to-primitive p)})
      )
    )

  )

(defn ->house [$HOUSE]
  (let [t (wdom/txt $HOUSE)
        [_rooms _area] (str/split t "·")

        [_total _living _kitchen] (map d/to-primitive (-> _area
            (str/replace " " "")
            (str/replace "м²" "")
            (str/split "/")))


        [_room_n _floor _material] (str/split _rooms ",")

        [floor floor-total] (map d/to-primitive (re-seq #"\d+" _floor))

        rooms-n (d/to-primitive (re-find #"\d+" _room_n))

        walls ((fnil str/trim "") _material)
        ]

    {
     :house_walls walls

     :area_total   _total
     :area_living  _living
     :area_kitchen _kitchen

     :floor       floor
     :floor_total floor-total

     :rooms rooms-n

     ;; " 66.7 / 32 / 10 м² " total bedrooms kitchen
     :_floor _floor
     :_rooms _room_n
     :_area _area
     }
    )
  )


(def custom-formatter (time-fmt/formatter "yyyy.MM.dd"))

(defn- ua->date [s]
  (let [t (cond
            (= "сьогодні" s) (time/today)
            (= "вчора" s) (time/yesterday)

            ;;
            :else (let [[_ _n unit] (re-find #"(\d+)(.+)тому" s)
                        n (d/to-primitive _n)]
                    (time/minus
                      (time/today) (cond
                                     (#{"дні" "днів"} unit) (time/days n)
                                     (= "тиж." unit) (time/weeks n)
                                     (= "міс." unit) (time/months n)))
                    )
            )]

    (time-fmt/unparse custom-formatter t)
    )

  )

(defn ->upd [$UPD]
  (let [t (-> $UPD
              (wdom/txt)
              (js/decodeURI)
              (str/replace " " "")
              )
        [_ _upd _added] (re-find #"^Онов:(.+)\sДод:(.+)$" t)
        ]

    {
     :upd   (ua->date (str/trim _upd))
     :added (ua->date (str/trim _added))
     }
    )
  )


(defn ->agent [$AGENT]
  (let [n (wdom/txt $AGENT)
        tel (-> (wdom/attr $AGENT "href"))

        [_ _tel] (re-find #"(\d+)\.rieltor.ua" tel)
        ]
    {
      :agent-id _tel
      :agent-name n
     }
    )


  )

(defn ->addr [$ADDR]

  (let [t (-> $ADDR
              (wdom/txt)
              ;(js/decodeURI)
              ;(str/replace " " "")
              )
        [_str _house-n _district] (str/split t ",")

        ;;[_ _upd _added] (re-find #"^Онов:(.+)\sДод:(.+)$" t)
        ]
    {
     :_ADDR t

     :addr_street (str/trim _str)
     :addr_house (str/trim _house-n)
     :addr_district (str/trim _district)
     }
    )
  )


(defn ->img [$IMG $IMG-NUM]

  (let [src (wdom/attr $IMG "src")
        src-set (wdom/attr $IMG "srcset")
        alt (wdom/attr $IMG "alt")

        img-n (-> $IMG-NUM
            (wdom/txt-only)
            (str/trim)
            (d/to-primitive)
            )
        ]

    {
     :imgs [src
            ; src-set
            ]
     :img-n img-n
     :img-alt alt
     }
    )
  )


;; element scraping function
(defn scrape-element [el]
  ; (.warn js/console el (d/pretty! (wdom/dataset el)))

  ;; (swap! *SCRAPED-DATA conj (. (. el -parentElement) -innerHTML))

  ;; saving parsing status in dom

  (if-not ALLOW-DOUBLE-PARSE
    (when (classes/has el "parsed")
      (.warn js/console "PARSE WAS CALLED TWICE")
      (classes/add el "parsed-twice"))
    )

  (classes/add el "parsed")

  ;; *PROCESSING-MAP
  (if-let [id (parser/safe-href el ".catalog-item__img A")]
    (let [$ID (wdom/q el ".catalog-item__img A")     ; "DIV:nth-child(1) > DIV > A:nth-child(1)" -> ".catalog-item__img A"

          $IMG (wdom/q el ".catalog-item__img IMG")  ; "DIV:nth-child(1) > DIV > A:nth-child(1) > IMG"
          $IMG-NUM (wdom/q el ".catalog-item__img .catalog-item__img-num")


          $ADDR (wdom/q el ".catalog-item__general-info > H2:nth-child(1) > A:nth-child(1)") ; "DIV:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1) > H2:nth-child(1) > A:nth-child(1)"
          $HOUSE (wdom/q el ".catalog-item__general-info .catalog-item_info-item-row")       ; "DIV:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1) > DIV:nth-child(2)"

          $LABELS (wdom/q* el ".catalog-item__general-info > H2:nth-child(1) > DIV .label")

          $DESCR (wdom/q el ".catalog-item__info .catalog-item_info-description") ;;

          $PRICE (wdom/q el ".catalog-item__price-column .catalog-item__price")   ; "DIV:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(2) > STRONG:nth-child(1)"
          $PRICE-M2 (wdom/q el ".catalog-item__price-column .catalog-item__price-per-sqm")

          $UPD (wdom/q el ".catalog-item__additional-info-container .catalog-item__additional-info")

          $AGENT (wdom/q el ".ov-author__info .ov-author__name A")
          ]



      ;(.log js/console (map #(wdom/attr % "class") $LABELS) $LABELS)

      ;; ID
      ; (mark! $ID "ID")
      ;; image
      ;; (mark! $IMG "IMG")
      ;; addr
      ;; (mark! $ADDR "ADDR")
      ;; all labels

        ;; label_location
        ;; label_no_commission
        ;; label_location_subway


      ;"DIV:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1) > H2:nth-child(1) > DIV:nth-child(2) > A:nth-child(1)"

      ;;(mark! (wdom/q el ".catalog-item__general-info H2 DIV A.label") "LBL")



      ;; house info
      ;
      ;(mark! $HOUSE "HOUSE-DETAILS")

      ;; descr
      ;; (mark! $DESCR "DESCR")


      ;; price

      ;(mark! $PRICE "PRICE USD")
      ;(mark! $PRICE-M2 "PRICE M^2")

      ;; upd
      ;;(mark! $UPD "UPD")

      ;; rieltor

      ;;(mark! $AGENT "RIELTOR")
      ;

      ;; $LABELS

      (swap! *SCRAPED-DATA conj
             (merge {
                     :source :riel
                     :id id
                    }

                    ;; meta info block
                    (reduce (fn [m $lbl]
                              (let [classes' (wdom/attr $lbl "class")
                                    classes (into #{} (str/split classes' #"\s"))
                                    ]

                                ;(.log js/console classes $lbl)
                                (merge
                                  m
                                  (if (classes "label_no_commission") {:no_commission true})

                                  (if (classes "label_attention")
                                    {
                                     ;:paid true
                                     :paid_info (str/trim (wdom/txt $lbl))
                                     })

                                  (if (or (classes "label_location")
                                          (classes "label_location_subway"))
                                    (let [distr-url (wdom/attr $lbl "href")
                                          distr   (wdom/txt $lbl)]
                                      (merge
                                        {
                                          :district_1 distr
                                          :district_1_url distr-url
                                        }
                                        (if (classes "label_location_subway")
                                          {:subway distr}))))
                                  (if (classes "label_new-building") {:house_new true})

                                  )
                                )
                              )
                            (if (wdom/q el ".paid")
                                  {:paid true}
                                  {})
                            $LABELS)

                    (->addr $ADDR)
                    (->price $PRICE $PRICE-M2)

                    ;; todo: commision
                    (->img $IMG $IMG-NUM)

                    (->house $HOUSE)

                    (->agent $AGENT)
                    (->upd   $UPD)
                    {:descr (-> $DESCR
                        (wdom/txt)
                        ;; (js/decodeURI)
                        (str/replace "... далі" "…")
                        (str/trim)
                        )}

                    ))


      ;
      )
    )
  )



(defn is-scraped? [el]
  (dataset/has el "woof_id")
  )

(defn mark-scraped! [el]
  (let [sid (base/rand-sid)]
    ;; mark element as processed
    (if-not ALLOW-DOUBLE-PARSE
      (dataset/set el "woof_id" sid)
      )
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





;; todo: move the scraping stuff to a separate ns
(defn make-ctx-fn []
  (let [
        ;; brute-force scraping, pass all parameters and retrieve, filter els in it
        _simple-brute-force (fn  [is-scraped? mark-scraped! process-step selector]
                              (.log js/console "simple scrape: A")

                              ;; try to find elements to be processed, but skip already processed
                              (let [els (filter (fn [el] (not (is-scraped? el)))
                                                (wdom/q* selector))]

                                ;;(.log js/console "els" els (wdom/q* selector))

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

        item-expand! (fn [el]
                       (let [_sid (mark-scraped! el)
                             sid (if (qualified-keyword? _sid)
                                   _sid
                                   (base/rand-sid "el-"))]
                         {sid [:scrape-el el]}))

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

    (fn [params]
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
                                        (classes/add el "parsed-error")

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
    )
  )



(defn wf! [*wf-state meta-info]


  ;; <?> should this initialization be here or in wf?

  ;; (.clear js/console)
  (reset! *SCRAPED-DATA [])
  (wf-clean-up-css)


  ;; for now go with local scope, instead of init fn
  (let [WATCHER-ID ::ui
        *WF-UI (rum/cursor-in *wf-state [:wf/UI])

        ;; todo: move keyboard handing to browser
        klog (fn [e]
               (let [chord (into {} (for [[key attr] {:shift "shiftKey" :ctrl "ctrlKey" :alt "altKey" :meta "metaKey"
                                                      :code  "keyCode"}]
                                      [key (aget e attr)]))]

                 (cond
                   (= chord {:shift true :ctrl false :alt false :meta true :code  38})
                   (do
                     (.log js/console "increase size!")
                     (wdom/scraping-ui__inc 50)
                     )

                   (= chord {:shift true :ctrl false :alt false :meta true :code  40})
                   (wdom/scraping-ui__inc -50)

                   :else (do)
                   )
                 (.log js/console chord)
                 )
               )
        ]
    {
     :init  [
             ;; pass UI atom to wf
             (fn [params]
               (merge
                 {
                  :wf/*UI *WF-UI
                  }
                 (watcher/_watcher-cf-init-cb
                   WATCHER-ID *WF-UI
                   (fn [*state state]
                     (.log js/console "UI: upd" state (= state @*state))
                     (wf-ui/<rum-ui> *state state))
                   params)))

             ;; add key logger
             (fn [params]
               (js/addEventListener "keydown" klog false)

               {}
               )
             ]


     :ctx   [watcher/watcher-ctx

             (fn [params]
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
                                                 (classes/add el "parsed-error")

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

        ;; :css/c4   [:css-rule ".parsed { opacity: .6; }"]

        :css/c4_1 [:css-rule ".parsed-twice { background-color: red; }"]
        :css/c4_2 [:css-rule ".parsed-error { background-color: red; outline: 5px solid crimson; }"]
        :css/c5   [:css-rule ".parsed-red { outline: 5px solid red; }"]
        :css/c6   [:css-rule ".parsed-magenta { outline: 5px solid magenta; }"]
        :css/c7   [:css-rule ".parsed-brown { outline: 5px solid brown; }"]
        :css/c8   [:css-rule ".parsed-error { outline: 5px solid brown; }"]


        ;; :css/attr-0 [:css-rule ".DDD { outline: 5px solid blue; }"]

        :css/attr-0 [:css-rules* [".DDD:hover" "outline: 5px solid crimson; \n background-color: rgba(255,0,0,.5);"]]
        :css/attr-1 [:css-rules* [".DDD > *" "z-index: 100;"]]
        :css/attr-2 [:css-rules* [".DDD:after" "z-index: 1; \n content: \"↑\" attr(data-parse-id) \"↑\"; b \n display: flex; \n background-color: red; \n font-weight: bolder; \n color: white; \n height: 20px; \n outline: 1px solid crimson;"]]
        :css/attr-3 [:css-rules* [".DDD:before" "content: \"↓\" attr(data-parse-id) \"↓\"; b \n display: flex; \n background-color: red; \n font-weight: bolder; \n color: white; \n height: 20px; \n outline: 1px solid crimson;"]]

        }
       {
        ::hello [:prn "scraping started!!!"]
        }
     ]

     :opts  [
             watcher/watcher-opts
             (base/build-opt-on-done (fn [params result]
                                       ; (.warn js/console result)
                                       (js/removeEventListener "keydown" klog false)))
             ]


     :api   (let [trigger-event (fn [steps] (_trigger-event (get @*wf-state :WF/params {}) steps))]
              [
               ;; toggles 'debug' mode on/off
               ["debug" (fn [] (classes/toggle (.-body js/document) "woof-debug"))]
               []
               ["save HTML" (fn []
                              (let [els (wdom/q* SCRAPE-SELECTOR)
                                    html (reduce (fn [s el]
                                                   (str s (. el -outerHTML))) "" els)]
                                (ws/POST "http://localhost:8081/kv/put" (fn [])
                                         {:k :html
                                          :v html})))]

               ["save EDN" (fn []
                             (let [data @*SCRAPED-DATA]
                               (ws/POST "http://localhost:8081/kv/append" (fn [])
                                        {:k :listings
                                         :v data})))]
               []
               ;; different modes of scraping

               ;; scrape once everything on page
               ["simple scrape" #(trigger-event {(base/rand-sid) [:brute-force-simple SCRAPE-SELECTOR]})]                ;;
               ["brute scrape"  #(trigger-event
                                    (let [k (base/rand-sid)]
                                      {
                                        k [:find-els SCRAPE-SELECTOR]
                                        (base/rand-sid) [:brute-1 k]
                                       }))]

               ["recurring scrape" #(trigger-event
                                      (let [k (base/rand-sid)]
                                        {
                                          k [:find-els SCRAPE-SELECTOR]
                                          (base/rand-sid) [:brute-recurring k]
                                         }))]

               []
               ["♾️ scroll" (fn []
                                    ;; todo make it togglable
                                    (let [params (get @*wf-state :WF/params {})
                                          evt-loop (evt-loop/&evt-loop params)]
                                      (async/put! evt-loop {
                                                            (base/rand-sid) [:8-scroll 100]
                                                            })))]

               []
               ["📋FAILED" (fn [] (wdom/copy-to-clipboard @*FAILED))]  ;; copy failed elements to buffer
               ["📋RESULT" (fn []
                             (let [data @*SCRAPED-DATA]
                               (.log js/console data)
                               ;(wdom/copy-to-clipboard data)
                               ;(wdom/save-edn (str "rieltor-" (u/now) ".edn") data)
                               ))]
              ])

     :on-stop (fn [state]
                (__log "ON STOP")
                ;; for now do not return channel
                nil)


     }
    )

  )
