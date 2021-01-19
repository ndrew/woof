(ns woof.client.browser.yt.nu-wf
  (:require
       [clojure.string :as str]

       [cljs.core.async :as async :refer [go go-loop]]


       [goog.dom.classes :as classes]
       [goog.dom.dataset :as dataset]

       ;; woof core
       [woof.base :as base :refer [rand-sid sid
                                   &chan-factory make-chan own-chan]]
       [woof.data :as d]
       [woof.utils :as u]

       ;; client utils
       [woof.client.dom :as woof-dom :refer [q q*]]
       [woof.client.dbg :as dbg :refer [__log]]

       ;; wf helpers -
       [woof.client.ws :as ws]

       ; helpers from base
       [woof.wfs.alpha :as alpha]
       [woof.wfs.evt-loop :as evt-loop]

       ;; ui
       [rum.core :as rum]

       [woof.client.playground.ui :as ui]
       [woof.client.browser.scraper.rum-ui :as rum-wf]
       [woof.client.browser.scraper.actions :as api-wf :refer [chord-action]]
       [woof.client.browser.scraper.scrape :as scrape]

       ;; scraping wf impl
       [woof.client.browser.yt.parser :as parser]
       [woof.client.browser.yt.nu-wf-ui :as yt-ui]

       )
  (:require-macros
    [woof.utils-macros :refer [inline--fn inline--fn1]]
    )
  )

;; scraping wf example
;; - get watch history






;;
;; implementation of scraping
;;
;;
(defn scraping-sub-wf [*wf-state *WF-UI]
  (let [; helpers
        trigger-event (fn [steps] (evt-loop/_emit-steps (get @*wf-state :WF/params {}) steps))

        action (fn [title steps-fn]
                   [title (fn []
                            (let [steps (steps-fn)]
                                (evt-loop/_emit-steps (get @*wf-state :WF/params {}) steps)))])


        save!         (fn [k v] (ws/POST "http://localhost:8081/kv/put"
                                         (fn [] (.log js/console (str k " saved")))
                                         {:k k :v v}))


        ; meta
        LINEARIZE? true

        ; data
        SEQ-ID ::seq
        SCRAPE-SELECTOR (parser/history-day-selector)

        recurring-parse! (fn [LINEARIZE? ]
                          (let [k (sid)]
                            (if LINEARIZE?
                              { ;; linearized version
                               k     [:find-els SCRAPE-SELECTOR]
                               (sid) [:linear-brute-recurring k]
                               }
                              {;; normal version
                               k     [:find-els SCRAPE-SELECTOR]
                               (sid) [:brute-recurring k]
                               }))
                          )

        ]
    {; _SCRAPE_

     :api   [
             ;; toggle details
             (chord-action (woof-dom/chord 49 :shift true)                    ;; shift+!
                           "👀" woof-dom/scraping-ui__togle-details)
             ["debug!" (fn []
                         (classes/toggle (.. js/document -body) "ZZZ"))]

             []

             ;; pass only selector
             (action ":batch-scrape-1!" #(do {(sid) [:batch-scrape-1! SCRAPE-SELECTOR]}))
             (action ":batch-scrape-2!" #(do {(sid) [:batch-scrape-2! SCRAPE-SELECTOR]}))

             (action ":scrape!!!" #(let [r (sid)]
                                     {
                                      r     [:scrape!!! {:$ SCRAPE-SELECTOR}]
                                      (sid) [:warn r]
                                      }))

             ;;
             (action "parse(expand)" #(do {(sid) [:brute-force-simple SCRAPE-SELECTOR]}))

             (action "parse(recurring expand)" recurring-parse!)

             []
             (action "SCROLL" #(let [CLOCK (sid "t-")
                                     SCROLLER (sid "scr-")]
                                 {
                                  CLOCK        [:tick [3000 3]]
                                  SCROLLER     [:rnd-scroll CLOCK]
                                  (sid "scr-") [:scroll SCROLLER]
                                  }))
             (action "♾️ scroll" #(do {(sid) [:8-scroll 10]}))
             []
             ["ANALYZE DOM!" (fn []
                               ;; find sections
                               (let [sections (woof-dom/q* SCRAPE-SELECTOR)]

                                 (swap! *WF-UI assoc :DOM (map #(woof-dom/nu-el-map % :MAX-LEVEL 3) sections))
                                 #_(.log js/console (woof-dom/nu-el-map (first sections)))))]
             []
             ["SAVE HTML" #(save! :html (woof-dom/outer-html (woof-dom/q* SCRAPE-SELECTOR)))]
             ["SAVE DATA" #(save! :RESULTS (:RESULTS @*WF-UI))]

             #_(chord-action (woof-dom/chord 49 :shift true)                    ;; shift+!
                           "SAVE DATA" #(save! :RESULTS (:RESULTS @*WF-UI)))



             ]

     ;; linearize parsing
     :init  [(fn [params] (alpha/_idle-seq-worker-init SEQ-ID params))]

     :steps [{
              ::hello [:prn "scraping wf running"]
              }]

     :ctx   [
             (fn [params]
               ;; todo: kinda generic way of parsing elements on page
               ;;
               ;; builds scraping ctx with common scraping stuff
               ;;
               ;;
               (let [

                     ;;
                     ;; marks element as parsed
                     scrape-start! (fn [el]


                                     ;; todo: use dataset for prod (as won't trigger css refresh)
                                     ;(dataset/set el "woof_scraped" "PROCESSING")

                                     (classes/add el "WOOF-WIP")

                                     ;(parser/mark-scraped! el)

                                     ;; return sid or unique element id
                                     (sid)
                                     )
                     ;;
                     ;; whether element had been scraped
                     is-scraping? (fn [el]
                                    ;;(dataset/has el "woof_scraped")
                                    (classes/has el "WOOF-WIP")

                                    ;;(parser/is-scraped? el)
                                   )

                     ctx-fn (scrape/make-ctx-fn
                              scrape-start!
                              is-scraping?

                              ;; indicates which elements to parse
                              SCRAPE-SELECTOR

                              (partial parser/_history-scrape-fn params)

                              ;; NORMAL PARSING
                              ; (partial parser/_history-day-scrape-async params)
                              ; (partial parser/_history-day-scrape params)
                              )

                     ;; trick: wrapping step handler from s-handler
                     ctx (ctx-fn params)]

                 ;;
                 ;; very ugly - pass parsing impl to ui state, so it can be used to parse via UI
                 (swap! *WF-UI merge {
                                      :SCRAPE-FN       (get-in ctx [:brute! :fn])
                                      :SCRAPE-SELECTOR SCRAPE-SELECTOR
                                      })

                 ;; override step handlers with linearized versions of step handlers
                 (merge
                   {
                    ;;=================================
                    ;; scrape via non-expand s-handler
                    ;; -
                    :batch-scrape-2! {
                                      :fn (fn [selector]

                                            ;; try to find elements to be processed, but skip already processed
                                            (let [els (filter #(not (is-scraping? %))
                                                              (q* selector))

                                                  ;; mark element for scraping first
                                                  els2scrape (loop [els els
                                                                    ;; key map ordered
                                                                    els-map (array-map)]
                                                               (if (seq els)
                                                                 (let [el (first els)
                                                                       _id (scrape-start! el)
                                                                       id (if (qualified-keyword? _id) _id (sid))]

                                                                   (recur (rest els)
                                                                          (assoc els-map id el))
                                                                   )
                                                                 ;; return
                                                                 els-map
                                                                 )
                                                               )
                                                  ]

                                              ;;(.log js/console "els" els (woof-dom/q* selector))


                                              (doseq [[id el] els2scrape]
                                                ;;=================
                                                ;;=================
                                                ;; TODO: CONTINUE HERE
                                                ;;=================
                                                ;;=================

                                                (.log js/console id el)
                                                ;(SCRAPE-FN el)
                                                )

                                              :ok
                                              )
                                            )
                                      }
                          :scrape!!! {
                                      :fn (fn [cfg]
                                            (let [out-chan (make-chan (&chan-factory params) (sid "OUT/"))
                                                  el-chan (make-chan (&chan-factory params) (sid "EL/"))

                                                  *result (volatile! [])

                                                  cont? (fn [el]
                                                          (.log js/console "EL" el)

                                                          (if (= :tick el)
                                                            (vswap! *result conj (u/now))
                                                            (vswap! *result conj el)
                                                            )
                                                          (> 20 (count @*result))
                                                          )]


                                              (let [els (filter #(not (is-scraping? %))
                                                                (q* SCRAPE-SELECTOR))

                                                    ;; mark element for scraping first
                                                    els2scrape (loop [els els
                                                                      ;; key map ordered
                                                                      els-map (array-map)]
                                                                 (if (seq els)
                                                                   (let [el (first els)
                                                                         _id (scrape-start! el)
                                                                         id (if (qualified-keyword? _id) _id (sid))]

                                                                     (recur (rest els)
                                                                            (assoc els-map id el))
                                                                     )
                                                                   ;; return
                                                                   els-map
                                                                   )
                                                                 )]

                                                (doseq [el els]
                                                  (async/put! el-chan el))
                                                ;(async/into els el-chan)
                                                )


                                              (go-loop []
                                                ;; how to extract this?
                                                (let [el (async/alt!
                                                           el-chan ([_el] _el)
                                                           (u/timeout 1000) :tick)]

                                                  (if (cont? el)
                                                    (recur)
                                                    (do
                                                      (async/>! out-chan @*result)
                                                      )
                                                    )
                                                  )
                                                )
                                              out-chan
                                              )

                                            )

                                      }

                    }
                   ctx
                   (if LINEARIZE?
                     (let [s-handler (get ctx :scrape-el)
                           f (:fn s-handler)]
                       {
                        :scrape-el              (assoc s-handler
                                                  :fn (fn [v]
                                                        (alpha/_seq-worker-handler SEQ-ID f params v)))

                        ;;
                        :linear-brute-recurring (assoc s-handler
                                                  :fn (fn [col]
                                                        (alpha/_seq-worker-expander SEQ-ID f params col)))

                        })
                     {})
                   )
                 )
               )
             ]
     }
    )
  )


(defn runner-sub-wf [& {:keys [execute-mode
                               t ; imeout
                               n ; chunk size
                               ] :as params
                        :or {execute-mode :idle-w-timeout
                             t 100
                             n 30}}]
  {
   :opts [(fn [params]
            ;; todo: find out the most efficient way of parsing
            (condp = execute-mode
              ;; process wf msgs only when page is idle
              :on-idle         {:execute alpha/execute-on-idle}
              ;; process wf msgs only when page is idle,
              ;; if page is busy then process messages after timeout t
              :idle-w-timeout  {:execute (partial alpha/_execute-on-idle-w-timeout t)}
              ;; process wf msgs on certain time interval t
              :timed-execute   {:execute (partial base/_timed-execute-fn t)}
              ;; process wf msg :process updates in chunks of size n
              :chunked-execute {:execute (partial base/_chunked-execute-fn n)}
              ;; use default execute - may be greedy
              {}
              )
            )]
   })



(def <rum-ui> (rum-wf/gen-rum-ui yt-ui/<scraping-ui>))

(defn _ui-process-fn__extract-results [*WF-UI result]

  (let [results* (select-keys result
                              (filter #(str/starts-with? (str %) ":/SCRAPE__") (keys result)))
        *errors (volatile! {})
        ready-results (reduce
                        (fn [a [k v]]
                          (if-not (u/channel? v)
                            (if (:error v)
                              (do
                                (vswap! *errors assoc k v)
                                a
                                )
                              (assoc a (:d v) (count (:videos v)))
                              )

                            a))
                        {} results*
                        )
        ]

    ;;(.warn js/console ready-results)
    (swap! *WF-UI assoc :SCRAPE/READY ready-results)
    (swap! *WF-UI assoc :SCRAPE/ERROR @*errors)
    )
  )


(defn ui-sub-wf [*WF-UI]
  (assoc
    (rum-wf/ui-impl! *WF-UI <rum-ui>)
    :steps [(fn [params]
              {
               :CSS/minimal-styles    [:css-file "http://localhost:9500/css/r.css"]
               :CSS/playground-styles [:css-file "http://localhost:9500/css/playground.css"]
               })]
    :opts [(fn [params]
             {:op-handlers-map {
                                :process (fn [result]
                                           ;; just take the results by key prefix
                                           (_ui-process-fn__extract-results *WF-UI result)

                                           result
                                           )}
              }
             )]
    )
  )


;;;;;
;;
;; WF
;;
(defn wf! [*wf-state meta-info]
  (let [;; state
        *WF-UI (rum/cursor-in *wf-state [:wf/UI])


        _RUN_ (runner-sub-wf
                  :execute-mode :idle-w-timeout
                  :t 10)

        _UI_ (ui-sub-wf *WF-UI)


        _SCRAPE_ (scraping-sub-wf *wf-state *WF-UI)

        API (get _SCRAPE_ :api [])

        _API_ (api-wf/actions-impl!
                API api-wf/default-on-chord)


        ]

    {
     :init            (concat
                        (get _UI_  :init [])
                        (get _API_ :init [])
                        (get _SCRAPE_ :init [])
                        )

     :ctx             (concat
                        (get _UI_     :ctx [])
                        (get _API_ :init [])
                        (get _SCRAPE_ :ctx [])
                        )

     :steps           (concat
                        (get _UI_ :steps [])
                        (get _API_ :steps [])
                        (get _SCRAPE_ :steps [])
                        )

     :opts            (concat
                        (get _UI_ :opts [])
                        (get _API_ :opts [])
                        (get _SCRAPE_ :opts [])
                        (get _RUN_ :opts [])
                        )

     ;; expose some wf API
     :api             API

     :on-stop         (fn [state]
                        (__log "ON STOP")

                        ;; clean up added css
                        (woof-dom/remove-added-css [
                                                    "WOOF-WIP"

                                                    "PROCESSED-VIDEO"
                                                    "DOUBLE-PROCESSED-VIDEO"
                                                    ]
                                                   )

                        ;; for now do not return channel
                        nil)

     ;; for now provide custom on-run handler - as older wf APIs are a map
     :scraper/on-run! (partial rum-wf/_on-run! *WF-UI)
     }
    )
  )
