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
    [woof.client.browser.scraper.actions :as api-wf :refer [chord-action]]

    [woof.client.browser.scraper.scrape :as scrape]

    [woof.client.browser.yt.parser :as parser]
    [woof.client.browser.yt.nu-wf-ui :as yt-ui]

    [woof.wfs.alpha :as alpha]
    [woof.wfs.evt-loop :as evt-loop]
    [woof.client.ws :as ws]
    [woof.data :as d]
    [woof.utils :as u])
  )

;; scraping wf example
;; - get watch history



(def <rum-ui> (rum-wf/gen-rum-ui yt-ui/<scraping-ui>))



;;
;; implementation of scraping
;;
;;
(defn scraping-sub-wf [*wf-state *WF-UI]
  (let [; helpers
        trigger-event (fn [steps] (evt-loop/_emit-steps (get @*wf-state :WF/params {}) steps))
        save!         (fn [k v] (ws/POST "http://localhost:8081/kv/put"
                                         (fn [] (.log js/console (str k " saved")))
                                         {:k k :v v}))


        ; meta
        LINEARIZE? true

        ; data
        SEQ-ID ::seq
        SCRAPE-SELECTOR (parser/history-day-selector)

        ]
    { ; _SCRAPE_

     :api [#_(chord-action (woof-dom/chord 49 :shift true ) "SCRAPE"
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
            (fn [] (trigger-event {(sid) [:brute-force-simple SCRAPE-SELECTOR]}))]

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


           ["SAVE HTML" #(save! :html (woof-dom/outer-html (woof-dom/q* SCRAPE-SELECTOR)))]

           ;;shift+1
           (chord-action (woof-dom/chord 49 :shift true)
                         "SAVE DATA"
                         #(save! :RESULTS (:RESULTS @*WF-UI))
                         )
           ]

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


(defn ui-sub-wf [*WF-UI]
  (assoc
    (rum-wf/ui-impl! *WF-UI <rum-ui>)
    :steps [(fn [params]
              {
               :CSS/minimal-styles    [:css-file "http://localhost:9500/css/r.css"]
               :CSS/playground-styles [:css-file "http://localhost:9500/css/playground.css"]
               })]
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
                  :execute-mode :idle-w-timeout)

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
