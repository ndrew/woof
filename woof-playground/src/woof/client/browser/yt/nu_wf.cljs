(ns woof.client.browser.yt.nu-wf
  (:require


    [cljs.core.async :as async :refer [go go-loop]]
    [goog.dom.classes :as classes]

    [rum.core :as rum]


    [woof.base :as base :refer [rand-sid sid]]

    [woof.client.dom :as woof-dom]

    [woof.client.playground.ui :as ui]

    [woof.client.dbg :as dbg :refer [__log]]

    [woof.client.browser.scraper.rum-ui :as rum-wf]
    [woof.client.browser.scraper.actions :as api-wf]

    [woof.client.browser.scraper.scrape :as scrape]

    [woof.client.browser.yt.parser :as parser]
    [woof.client.browser.yt.nu-wf-ui :as yt-ui]

    [woof.wfs.alpha :as alpha]
    [woof.wfs.evt-loop :as evt-loop]
    [woof.client.ws :as ws]
    [woof.data :as d]
    [woof.utils :as u]))

;; scraping wf example
;; - get watch history



(def <rum-ui> (rum-wf/gen-rum-ui yt-ui/<scraping-ui>))


;; WORKFLOW ACTIONS (API)
;;
(defn wf-api [*wf-state *WF-UI]

  (let [SCRAPE-SELECTOR (parser/history-day-selector)
        trigger-event (fn [steps] (evt-loop/_emit-steps (get @*wf-state :WF/params {}) steps))

        LINEARIZE? true
        ]
    [#_(api-wf/chord-action (woof-dom/chord 49 :shift true ) "SCRAPE"
       (fn []
         (trigger-event {(base/rand-sid) [:brute-force-simple SCRAPE-SELECTOR]})))
     ["RECURRING PARSE"
      (fn []
        ;:brute-recurring
        (trigger-event
          (let [k (sid)]
            (if LINEARIZE?
              ;; linearized version
              {
               k     [:find-els SCRAPE-SELECTOR]
               (sid) [:linear-brute-recurring k]
               }
              ;; normal version
              {
               k     [:find-els SCRAPE-SELECTOR]
               (sid) [:brute-recurring k]
               }))))
      ]

     ["PARSE"
      (fn []
        (trigger-event {(sid) [:brute-force-simple SCRAPE-SELECTOR]}))]

     ;;
     ;; scroll until
     ["SCROLL"
      (fn []

        ; simplest scroll
        ;(trigger-event {(base/rand-sid) [:scroll 1]})

        ;; scroll with timeout
        (trigger-event
          (let [CLOCK (sid "t-")
                SCROLLER (sid "scr-")]
            {
             CLOCK        [:tick [3000 3]]
             SCROLLER     [:rnd-scroll CLOCK]
             (sid "scr-") [:scroll SCROLLER]
             })
          )
        )]

     ["♾️ scroll"
      (fn []
        (trigger-event {(sid) [:8-scroll 10]}))]


     ["ANALYZE DOM!"
      (fn []
        ;; find sections
        (let [sections (woof-dom/q* SCRAPE-SELECTOR)]

            (swap! *WF-UI assoc :DOM (map #(woof-dom/nu-el-map % :MAX-LEVEL 3) sections))
            #_(.log js/console (woof-dom/nu-el-map (first sections))))
        )]

     ["SAVE HTML"
      (fn []
        (let [els (woof-dom/q* SCRAPE-SELECTOR)
                html (reduce (fn [s el] (str s (. el -outerHTML))) "" els)]

            (ws/POST "http://localhost:8081/kv/put" (fn [])
                     {:k :html
                      :v html})
            )
        )
      ]

     ;;
     (api-wf/chord-action (woof-dom/chord 49 :shift true )
        "SAVE DATA"
        (fn []
          ;; find sections
          (ws/POST "http://localhost:8081/kv/put" (fn [])
                   {:k :RESULTS
                    :v (:RESULTS @*WF-UI)})
          (.log js/console "sent results")
          ;; todo: maybe indicate via woof ui
          ))
     ]
  )
)





(defn wf! [*wf-state meta-info]
  (let [*WF-UI (rum/cursor-in *wf-state [:wf/UI])
        _UI_ (assoc
               (rum-wf/ui-impl! *WF-UI <rum-ui>)
               :steps [(fn [params]
                         {
                          :CSS/minimal-styles    [:css-file "http://localhost:9500/css/r.css"]
                          :CSS/playground-styles [:css-file "http://localhost:9500/css/playground.css"]
                       })]
               )

        API (wf-api *wf-state *WF-UI)
        _API_ (api-wf/actions-impl! API api-wf/default-on-chord)


        _RUN_ {
               :opts [(fn [params]
                        (let [execute-mode :idle-w-timeout]

                          ;; todo: find out the most efficient way of parsing
                          (condp = execute-mode

                            :on-idle {
                                      :execute alpha/execute-on-idle
                                      }
                            :idle-w-timeout {
                                             :execute (partial alpha/_execute-on-idle-w-timeout 50)
                                             }
                            :timed-execute {
                                            :execute  (partial base/_timed-execute-fn 1000)
                                            }

                            :chunked-execute {
                                              :execute (partial base/_chunked-execute-fn 30)
                                              }

                            ;; no execute wf
                            {}
                            )
                          )
                        )]
               }

        SEQ-ID ::seq

        _SCRAPE_ {
                  ;; linearize parsing
                  :init [(fn [params] (alpha/_idle-seq-worker-init SEQ-ID params))]

                  :steps [{
                          ::hello [:prn "scraping started!!!"]
                          }]
                  :ctx [
                        (fn [params]
                          ;; todo: kinda generic way of parsing elements on page
                          (let [ctx-fn (scrape/make-ctx-fn
                                         parser/mark-scraped!
                                         parser/is-scraped?
                                         (parser/history-day-selector)
                                         ; (partial parser/_history-day-scrape-async params)
                                         (partial parser/_history-day-scrape params))
                                ctx (ctx-fn params)

                                s-handler (get ctx :scrape-el)
                                f (:fn s-handler)
                                ]

                            ;;
                            ;; very ugly - pass parsing impl to ui state, so it can be used to parse via UI
                            (swap! *WF-UI merge {
                                                 :SCRAPE-FN (get-in ctx [:brute! :fn])
                                                 :SCRAPE-SELECTOR (parser/history-day-selector)
                                                 })

                            (merge
                              ctx
                              {

                               ;; linearize existing step handler
                               :scrape-el (assoc s-handler
                                            :fn (fn [v]
                                                  (alpha/_seq-worker-handler SEQ-ID f params v)))

                               ;;
                               :linear-brute-recurring (assoc s-handler
                                                           :fn (fn [col]
                                                                 (alpha/_seq-worker-expander SEQ-ID f params col)))

                               }
                              )
                            )
                          )
                        ]
                  }
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
                        (get _RUN_ :opts [])
                        )

     ;; expose some wf API
     :api             API

     :on-stop         (fn [state]
                        (__log "ON STOP")
                        ;; for now do not return channel
                        nil)

     ;; for now provide custom on-run handler - as older wf APIs are a map
     :scraper/on-run! (partial rum-wf/_on-run! *WF-UI)
     }
    )
  )
