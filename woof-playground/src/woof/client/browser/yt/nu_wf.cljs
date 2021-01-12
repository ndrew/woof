(ns woof.client.browser.yt.nu-wf
  (:require


    [cljs.core.async :as async :refer [go go-loop]]
    [goog.dom.classes :as classes]

    [rum.core :as rum]

    [woof.base :as base :refer [rand-sid]]

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
    [woof.core.protocols :as protocols]
    [woof.data :as d]
    [woof.utils :as u]))

;; scraping wf example
;; - get watch history



(def <rum-ui> (rum-wf/gen-rum-ui yt-ui/<scraping-ui>))


;; WORKFLOW ACTIONS (API)
;;
(defn wf-api [*wf-state *WF-UI]

  (let [SCRAPE-SELECTOR (parser/history-day-selector)
        trigger-event (fn [steps] (evt-loop/_emit-steps (get @*wf-state :WF/params {}) steps))]

    #_{
     ::clock              [:tick [3000 3]]

     ;;::scroller [:rnd-scroll ::clock]
     ;; :infinite/scroll [:scroll ::scroller]

     }

    [
     #_(api-wf/chord-action (woof-dom/chord 49 :shift true )
       "SCRAPE"
       (fn []
         (trigger-event {(base/rand-sid) [:brute-force-simple SCRAPE-SELECTOR]})))


     [
      "RECURRING PARSE"
      (fn []
        ;:brute-recurring
        (trigger-event
          (let [k (base/rand-sid)]
            {
             k               [:find-els SCRAPE-SELECTOR]

             ;; normal version
             (base/rand-sid) [:brute-recurring k]

             ;; linearized version
             ;; (base/rand-sid) [:linear-brute-recurring k]
             }
            )
          )
        )
      ]

     ["PARSE"
      (fn []
        (trigger-event {(base/rand-sid) [:brute-force-simple SCRAPE-SELECTOR]}))]


     ;; scroll until

     ["SCROLL"
      (fn []
        (trigger-event {(base/rand-sid) [:scroll 1]}))]

     ;["♾️ scroll"
     ; (fn []
     ;   (trigger-event {(base/rand-sid) [:8-scroll 100]}))]


     ;; todo: clean processed
     #_["CLEAN PROCESSED"
      (fn []
        (let [$els (woof-dom/q* ".PROCESSED-SECTION #contents")]
          (doseq [$el $els]
            (woof-dom/html! $el "")
            )
          )
        )
      ]

     #_["CLEAN PROCESSED (smart)"
      (fn []
        (trigger-event
          (let [k (base/rand-sid)]
            {
             ;k               [:find-els SCRAPE-SELECTOR]
             ;; normal version
             ;(base/rand-sid) [:brute-recurring k]

             ;; linearized version
             ;; (base/rand-sid) [:linear-brute-recurring k]
             }
            )
          ))
      ]

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


;; idle executors:
;; ---------------
;; maybe should be moved to wfs.alpa

; execute on idle
(defn execute-on-idle [executor]
  (let [from (protocols/execute! executor)
        to (async/chan 1)]

    (go-loop []
      (async/<! (alpha/request-idle-callback-chan!))
      (let [v (async/<! from)]
        (if (nil? v)
          (async/close! to)
          (when (async/>! to v)
            (recur)))))

    to)
  )


; idle or timeout
(defn _execute-on-idle-w-timeout [t executor]
  (let [from (protocols/execute! executor)
        to (async/chan 1)]

    (go-loop []
      (async/alt!
        (alpha/request-idle-callback-chan!) ([]
                                             ;(.log js/console "idle")
                                             )
        (u/timeout t) ([]
                       ;(.log js/console "t")
                       )
        )
      ;(async/<! (alpha/request-idle-callback-chan!))
      (let [v (async/<! from)]
        (if (nil? v)
          (async/close! to)
          (when (async/>! to v)
            (recur)))))
    to)
  )



(defn wf! [*wf-state meta-info]
  (let [
        *WF-UI (rum/cursor-in *wf-state [:wf/UI])

        _UI_ (rum-wf/ui-impl!
               *WF-UI
               <rum-ui>)

        API (wf-api *wf-state *WF-UI)

        _API_ (api-wf/actions-impl! API api-wf/default-on-chord)

        ;; *WF-scrape-data (rum/cursor-in *WF-UI [:scraped])

        SEQ-ID ::seq

        ]
    {
     :init            (concat

                        (get _UI_ :init [])

                        ;; linearize parsing
                        [
                         (fn [params] (alpha/_idle-seq-worker-init SEQ-ID params))
                         ]

                        (get _API_ :init [])
                        )


     :ctx             (concat
                        (get _UI_ :ctx [])

                        ;; scrape ctx
                        [
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
                                ;:linear-brute-recurring (assoc s-handler
                                ;                            :fn (fn [col]
                                ;                                  (alpha/_seq-worker-expander SEQ-ID f params col)))

                                }

                               )

                             )

                           ; (partial alpha/_seq-worker-expander SEQ-ID f)
                           ;         :linear-brute-recurring    {:fn       (partial _expander!
                           ;                                                       recurring-scrape-expand!
                           ;                                                      item-expand!)
                           ;                                  :collect? true
                           ;                                 :expands? true}


                           ;; meh - ugly

                           )
                         ]
                        )

     :steps           [
                       ;; UI
                       {
                        :CSS/minimal-styles    [:css-file "http://localhost:9500/css/r.css"]
                        :CSS/playground-styles [:css-file "http://localhost:9500/css/playground.css"]

                        }
                       ;;
                       {
                        ::hello [:prn "scraping started!!!"]
                        }
                       ]

     :opts            (concat
                        (get _UI_ :opts [])
                        (get _API_ :opts [])
                        [(fn [params]
                           (let [execute-mode :idle-w-timeout]

                             ;; todo: find out the most efficient way of parsing
                             (condp = execute-mode

                               :on-idle {
                                         :execute execute-on-idle
                                         }
                               :idle-w-timeout {
                                                :execute (partial _execute-on-idle-w-timeout 50)
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
                        )

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
