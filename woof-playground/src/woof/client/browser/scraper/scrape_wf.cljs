(ns woof.client.browser.scraper.scrape-wf
  (:require
    ;; core
    [cljs.core.async :as async :refer [go go-loop]]
    [clojure.string :as str]

    ;; woof core
    [woof.base :as base :refer [rand-sid sid
                                &chan-factory make-chan own-chan]]
    [woof.data :as d]
    [woof.utils :as u]

    ;; client utils
    [woof.client.dom :as woof-dom :refer [q q* txt dataset]]

    ;; wf helpers -
    [woof.client.ws :as ws]

    ; helpers from base
    [woof.wfs.alpha :as alpha]
    [woof.wfs.evt-loop :as evt-loop]

    ;; ui
    [goog.dom.classes :as classes]
    [goog.dom :as dom]

    [rum.core :as rum]

    [woof.client.browser.scraper.rum-ui :as rum-wf]
    [woof.client.browser.scraper.actions :as api-wf :refer [chord-action]]

    ;;
    [woof.client.browser.scraper.listings :as l]
    ))



(defn quick-analysis-scrape! [params el data]

  ;; todo: add more rules
  (if (re-find #"Набережно" (:addr_street data))
    (classes/add el "WOOF-GIVNO"))

  nil
  )

(defn user-confirm-scrape! [params el result]
  (let [*IDS (:*IDS params)

        cf (&chan-factory params)
        chan (make-chan cf (base/sid))]

    (let [ids @*IDS]
      ; (.warn js/console (:id result) ids)
      (if-not (get ids (:id result))
        (let [btn (dom/createDom "button" "ok-btn WOOF-DOM" "✅OK")]
          (swap! *IDS conj (:id result))
          (dom/appendChild el btn)
          (woof-dom/on-click btn (fn [e]
                               (async/put! chan result)))
          chan)
        (do
          (classes/add el "WOOF-SEEN")
          (classes/add el "WOOF-SKIP")
          ;;
          nil
          )
        )
      )
    )
  )


(defn build-scrape-fn [src]

  (let [parse-listing-fn (l/get-parse-fn src)

        post-scrape-fn (fn [params el result]
                         (quick-analysis-scrape! params el result)
                         (user-confirm-scrape! params el result))]

    (fn [params el]
      (let [result (parse-listing-fn el)]
        (post-scrape-fn params el result))))
  )


;; results processing aspect
(defn _results-init [*WF-UI]
  ;; result of scraping
  (swap! *WF-UI assoc :scraped [])

  ;; ids
  (swap! *WF-UI assoc :ids #{})
  (swap! *WF-UI assoc :sent-ids #{})
  )


(defn _results-add [*WF-UI r]
  (swap! *WF-UI update :scraped into [r]))

(defn _results-read [*WF-UI]
  (get @*WF-UI :scraped))


;;
;; scraping implementation
(defn scraping-sub-wf [*wf-state *WF-UI]

  ;; els -> candidates -> result

  (let [url (.. js/document -location -href)

        SRC (l/get-source url)

        RESULTS_INIT (partial _results-init *WF-UI)
        RESULTS_ADD  (partial _results-add *WF-UI)
        RESULTS_READ (partial _results-read *WF-UI)

        SCRAPE-CONTAINER-SELECTOR (l/get-container-selector SRC)
        SCRAPE-SELECTOR (l/get-scrape-selector SRC)


        ;; pass *IDS
        *IDS (rum/cursor-in *WF-UI [:ids])
        *SENT-RESULT-IDS (rum/cursor-in *WF-UI [:sent-ids])
        SCRAPE! (build-scrape-fn SRC)

        ]

    ;;
    ;; BEFORE WF

    ;; clean up added css
    (woof-dom/remove-added-css [
                                "WOOF-WIP"
                                "WOOF-DONE"
                                "WOOF-ASYNC"
                                "WOOF-SEEN"
                                "WOOF-SKIP"
                                "WOOF-GIVNO"

                                "WOOF-ERROR"
                                "WOOF-PARSE-ERROR"
                                ;; debug
                                "DDD"
                                ])

    ;; remove added dom by woof
    ;(.warn js/console "REMOVING" (q* ".WOOF-DOM"))
    (doseq [el (q* ".WOOF-DOM")]
      (if-let [parent (.-parentElement el)]
        (.removeChild parent el)))

    ;; init state to hold dom els queue
    (swap! *WF-UI assoc :el-queue #queue [])
    (swap! *WF-UI assoc :process-queue #queue [])

    (RESULTS_INIT)

    ;;
    ;;

    (let [<API> (fn [title steps-fn]
                  [title (fn []
                           (let [steps (steps-fn)]
                             (evt-loop/_emit-steps (get @*wf-state :WF/params {}) steps)))])
          ;;
          UPD-CHAN (async/chan)
          *ticker-ready? (atom false)

          ;; add element to the processing queue
          queue! (fn [el]
                   (.log js/console "QUEUED" (woof-dom/txt el))
                   (classes/add el "WOOF-WIP")
                   (swap! *WF-UI update :el-queue conj el)
                   )

          ;;
          ;;
          mutator (js/MutationObserver. (fn [mut] (async/put! UPD-CHAN :mutation)))
          ;; <!cfg>
          mutation-cfg #js {:characterData true
                            :childList true
                            :subtree true}
          mutations-observe! (fn []
                               (let [el (q SCRAPE-CONTAINER-SELECTOR)]
                                 (.observe mutator el mutation-cfg)))

          ;;
          ;; <!cfg>
          intersect-cfg #js {
                             :threshold 0 ;[0 1]
                             :rootMargin "0px 0px 0px 0px" ; (top, right, bottom, left)
                             }

          intersector (js/IntersectionObserver.
                        (fn [entries observer]
                          ;(.group js/console "intersect")
                          (doseq [entry (array-seq entries)]
                            (let [el (.-target entry)
                                  use-el? (.-isIntersecting entry)]
                              (if use-el?
                                (do
                                  ;(.log js/console frac (woof-dom/txt el))
                                  (queue! el)
                                  (.unobserve observer el))
                                )))
                          ;(.groupEnd js/console)
                          )
                        intersect-cfg)

          intersect-observe! (fn [] (doseq [el (q* SCRAPE-SELECTOR)]
                                      (.observe intersector el)))


          start-parse-fn (fn []
            (intersect-observe!)
            (mutations-observe!)

            ;; todo: wait for ids to be loaded
            ;; todo: emit only after certain step is done

            (let [steps {
                         :EVT/tick                 [:ticker UPD-CHAN]
                         ;:DOM/intersect-observer [:queue-in-view! :EVT/tick]
                         ;:PROCESS/process [:process-queue! :EVT/tick]


                         :TICK/el-queue            [:queue-els-ticker :EVT/tick]
                         :SCROLL/register-handlers [:queue-in-view! :TICK/el-queue]

                         :TICK/el-process          [:process-els-ticker :EVT/tick]
                         :DOM/process              [:process-queue! :TICK/el-process]

                         }]
              (evt-loop/_emit-steps (get @*wf-state :WF/params {}) steps)))


          save-data! (fn []
                       ;; todo: store which results had been already send
                       (let [ROWS (RESULTS_READ)

                             sent-ids @*SENT-RESULT-IDS

                             r (filter (fn [x]
                                         (not (contains? sent-ids (:id x)))) ROWS)
                             IDS @*IDS]

                         (.log js/console "TRYING TO SAVE")
                         (when-not (empty? IDS)
                           (.group js/console "SAVE:")
                           (.warn js/console "APPEND IDS - 1 - " IDS)

                           ;; chain saving results
                           (ws/POST "http://localhost:8081/kv/append-set"
                                    (fn []
                                      (.log js/console "IDS...saved!")
                                      (if (> (count r) 0)
                                        (do
                                          (.log js/console "sending rows...")
                                          (ws/POST "http://localhost:8081/kv/append"
                                                   (fn []
                                                     (.log js/console "ROWS...saved!")
                                                     (swap! *SENT-RESULT-IDS into (map :id r))
                                                     (.log js/console @*SENT-RESULT-IDS)

                                                     (.groupEnd js/console))
                                                   {:k :rows :v r})
                                          )
                                        (do
                                          (.groupEnd js/console))
                                        )
                                      )
                                    {:k SRC :v IDS})
                           )
                         )
                       )

          ;; TODO: this is not working without service worker, so manually need to
          SAVE-DATA-ON-LEAVE? false

          PERIODIC-SAVE 5000;; 0 to disable
          *periodic-save-id (atom nil)
          ]
      {

       :init  [(fn [params]
                 ; register the update channel
                 (let [cf (&chan-factory params)]
                   (own-chan cf :UPD-CHANNEL UPD-CHAN))
                 {})

               (fn [_]

                 ;; makes no sense - as two requests won't be send
                 (if SAVE-DATA-ON-LEAVE?
                   (js/addEventListener "beforeunload" save-data! false))

                 (if (> PERIODIC-SAVE 0)
                   (reset! *periodic-save-id (js/setInterval save-data! PERIODIC-SAVE)))

                 {:*IDS *IDS})
               ]

       ;;
       ;; ticker implementation
       ;;
       :ctx   [(fn [params]
                 ;; split main tick to a sub-ticks
                 (let [_cond-ticker-fn (fn [pred? ticker]
                                         (let [ch (make-chan (&chan-factory params) (sid))]
                                           (if (pred? ticker)
                                             (async/put! ch ticker))
                                           ch))

                       ; ticker configuration/back-pressure
                       *run-on-idle (atom false)
                       *tick-t (atom 200)
                       *t (atom (u/now))

                       ticker-fn (fn [chan]
                                   (.log js/console "TICKER STARTED")
                                   ;; maybe some xf here?

                                   (go-loop []
                                     (let [tick-chan (u/timeout @*tick-t)
                                           run-on-idle? @*run-on-idle

                                           idle-cb-chan (if run-on-idle? (alpha/request-idle-callback-chan!) nil)
                                           ports (into
                                                   (if @*run-on-idle
                                                     [idle-cb-chan] [])
                                                   [
                                                    UPD-CHAN tick-chan
                                                    ])

                                           [_msg port] (async/alts! ports)

                                           msg (cond
                                                 (= tick-chan port) :tick
                                                 (and run-on-idle?
                                                      (= idle-cb-chan port)) :tick
                                                 :else _msg)

                                           ;; get status of queues
                                           {
                                            prq :process-queue
                                            elq :el-queue
                                            } @*WF-UI

                                           has-process-queued? (not (empty? prq))
                                           has-els-queued? (not (empty? elq))

                                           has-work? (or
                                                       has-process-queued?
                                                       has-els-queued?)
                                           ]

                                       ; (.log js/console "MSG" msg)
                                       (cond
                                         ;; if we got update from UPD-CHAN - then send ELS event
                                         (= tick-chan port) ;; (= :mutation msg)
                                         (do
                                           ;; enable idle channel as we have now work
                                           (reset! *run-on-idle true)
                                           (async/put! chan :ELS/mutation-tick))

                                         ;;
                                         (and (= :tick msg)
                                              has-work?)
                                         (do
                                           ;; enable idle channel as we have now work
                                           (reset! *run-on-idle true)
                                           (async/put! chan :PROCESS/process-tick))

                                         :else (do
                                                 ; no work, disable idle-thread
                                                 (reset! *run-on-idle false)
                                                 ;; todo: adjusting timeout if we come from main ticker
                                                 ;; (if (= tick-chan port) ... )
                                                 ))

                                       (if-not @*ticker-ready?
                                         (do

                                           ;; updating tick period
                                           #_(let [t @*t
                                                   now (u/now)]
                                               (.log js/console "UPD:" (- now t) "ms")
                                               (reset! *t now)
                                               )

                                           ;(.log js/console "(RECUR)")
                                           (recur))
                                         )
                                       )
                                     )
                                   chan
                                   )

                       ]
                   {
                    ; main ticker for wf updates
                    :ticker
                                        {:fn       ticker-fn
                                         :infinite true}


                    ;; push ticker
                    :queue-els-ticker   {:fn       (partial _cond-ticker-fn #(= "ELS" (namespace %)))
                                         :infinite true}


                    ;; pop ticker
                    :process-els-ticker {:fn       (partial _cond-ticker-fn #(= "PROCESS" (namespace %)))
                                         :infinite true}
                    }
                   )

                 )
               (fn [params]
                 {

                  :scroll-parse  {:fn (fn [_]
                                        (start-parse-fn)
                                        :started
                                        )}

                  :load-ids       {:fn (fn [SRC]
                                         (let [ch (make-chan (&chan-factory params) (rand-sid))]

                                           ;; todo:

                                           (ws/GET (str "http://localhost:8081/kv/get/" SRC)
                                                   (fn [_data]
                                                     (let [backend-ids (d/to-primitive _data)]
                                                       (if (nil? backend-ids)
                                                         (do
                                                           (.log js/console "loading ids from FS")
                                                           (ws/GET (str "http://localhost:9500/s/drv/" (name SRC) "_ids.edn")
                                                                   (fn [raw-edn]
                                                                     (let [ids (into #{} (d/to-primitive raw-edn))]
                                                                       (.warn js/console "APPEND IDS" ids)


                                                                       (swap! *WF-UI assoc :ids ids)

                                                                       (ws/POST "http://localhost:8081/kv/append-set"
                                                                                (fn []
                                                                                  (async/put! ch ids)
                                                                                  ) {:k SRC :v ids})
                                                                       )
                                                                     )
                                                                   )
                                                           #_(ws/POST "http://localhost:8081/kv/put"
                                                                      (fn []
                                                                        (async/put! ch #{}))
                                                                      {:k SRC :v #{}})
                                                           )
                                                         (let [set-ids (into #{} backend-ids)]
                                                           (.warn js/console "loaded ids" set-ids
                                                                  ; (pr-str (count backend-ids))
                                                                  )
                                                           (async/put! ch set-ids)
                                                           (swap! *WF-UI assoc :ids set-ids)
                                                           )
                                                         )
                                                       )
                                                     ))
                                           ch
                                           )
                                         )}

                  ;; attaches scroll observers on scraped elements
                  :queue-in-view! {:fn (fn [ticker]
                                         ;(.log js/console ":observe!" ticker)
                                         (intersect-observe!)
                                         ;;
                                         (u/now))}

                  ;; adds all found elements to element queue (without scrolling)
                  :queue-all!     {:fn (fn [ticker]
                                         (.log js/console ":queue!")
                                         (let [selector SCRAPE-SELECTOR
                                               els (q* selector)]

                                           (doseq [el els]
                                             (classes/add el "WOOF-WIP"))
                                           (swap! *WF-UI update :el-queue into els)

                                           (u/now)))}

                  ;; process
                  :process-queue! {:fn (fn [ticker]
                                         ;(.log js/console "processing!!!" ticker)

                                         (let [p-queue (get-in @*WF-UI [:process-queue])
                                               ;; enrich ids

                                               ]
                                           (if (empty p-queue)
                                             (do

                                               ;; copy the values from el-queue to process-queue
                                               (swap! *WF-UI update :process-queue into
                                                      (get-in @*WF-UI [:el-queue] []))
                                               ;; clean-up the el-queue - todo: maybe copy only chunk of elements
                                               (swap! *WF-UI assoc :el-queue #queue [])


                                               (when-let [el (peek (get-in @*WF-UI [:process-queue]))]
                                                 ;; remove element from queue immediately
                                                 (swap! *WF-UI update :process-queue pop)

                                                 (.log js/console "processing" el)
                                                 ;; process - res/nil/exception?
                                                 (try
                                                   (if-let [result (SCRAPE! params el)]
                                                     (if (u/channel? result)
                                                       (do
                                                         ;; handling if channel is returned
                                                         (classes/add el "WOOF-ASYNC") ; mark element as in progress
                                                         (go
                                                           (let [r (async/<! result)] ; wait for result, todo: alts
                                                             (classes/add el "WOOF-DONE")
                                                             (RESULTS_ADD r)
                                                             ;(swap! *WF-UI update :results into [r])

                                                             )))
                                                       (do
                                                         ;; handling value
                                                         (classes/add el "WOOF-DONE")
                                                         (RESULTS_ADD result)
                                                         ))
                                                     (do
                                                       ;; nil - skip node

                                                       ;;
                                                       ;; FIXME: HANDLE PROPERTY SKIP
                                                       ;;

                                                       ; can't process further, re-adding el back to the queue
                                                       ;(swap! *WF-UI update :el-queue conj el)

                                                       ;; add back to process queue?
                                                       ;; maybe add with some delay?

                                                       (when-not (classes/has el "WOOF-SKIP")
                                                         (.log js/console "RE-PROCESSING EL AGAIN")
                                                         (swap! *WF-UI update :process-queue conj el)
                                                         )

                                                       )
                                                     )
                                                   (catch js/Error e
                                                     (.error js/console e)
                                                     ; can't process further, re-adding el back to the queue
                                                     (classes/add el "WOOF-ERROR")
                                                     ;; (swap! *WF-UI update :el-queue conj el)
                                                     )
                                                   )
                                                 )
                                               )
                                             )
                                           (do
                                             (.log js/console "PROCESSING IN PROGRESS"))
                                           )
                                         )}
                  }
                 )
               ]

       :opts  [(base/build-opt-on-done (fn [_]
                                         (if SAVE-DATA-ON-LEAVE?
                                           (js/removeEventListener "beforeunload" save-data! false))

                                         (if (> PERIODIC-SAVE 0)
                                           (js/clearInterval @*periodic-save-id))

                                         ;; stop the ticker
                                         (reset! *ticker-ready? true)
                                         ;; disconnect observers
                                         (.disconnect intersector)
                                         (.disconnect mutator)))
               ]

       :steps [
               {
                ;; comment out to disable auto start
                 :WS/LOAD-IDS [:load-ids SRC]
                 :scraping/start [:scroll-parse :WS/LOAD-IDS]

                ;; debug css
                :css/attr-0  [:css-rules* [".DDD:hover" "outline: 5px solid crimson; \n background-color: rgba(255,0,0,.5);"]]
                :css/attr-1  [:css-rules* [".DDD > *" "z-index: 100;"]]
                :css/attr-2  [:css-rules* [".DDD:after" "z-index: 1; \n content: \"↑\" attr(data-parse-id) \"↑\"; b \n display: flex; \n background-color: red; \n font-weight: bolder; \n color: white; \n height: 20px; \n outline: 1px solid crimson;"]]
                :css/attr-3  [:css-rules* [".DDD:before" "content: \"↓\" attr(data-parse-id) \"↓\"; b \n display: flex; \n background-color: red; \n font-weight: bolder; \n color: white; \n height: 20px; \n outline: 1px solid crimson;"]]

                }
               ]

       :api   [

               ;; TODO: keep this in sync with :steps
               (<API> "START" #(do {
                                    :WS/LOAD-IDS [:load-ids SRC]
                                    :zzz [:log :WS/LOAD-IDS]
                                    :scraping/start [:scroll-parse :WS/LOAD-IDS]
                                    }))

               ["START(scroll)" start-parse-fn]


               ;; <?> does greedy scraping makes sense?

               #_(<API> "START(greedy)"
                      #(do

                         ;; emulate
                         ;(async/put! UPD-CHAN :mutation)

                         (mutations-observe!)

                         ;; for now - update on tick
                         (go
                           ;; ugly, but this is needed to trigger the greedy scraping
                           (async/<! (u/timeout 0))
                           (async/put! UPD-CHAN :mutation))

                         {
                          :EVT/tick        [:ticker UPD-CHAN]
                          :TICK/el-queue   [:queue-els-ticker :EVT/tick]

                          :GREEDY/queuing  [:queue-all! :TICK/el-queue]

                          :PROCESS/process [:process-queue! :EVT/tick]
                          :DOM/process     [:process-queue! :TICK/el-process]
                          }))

               []

               ; (<API> "manual: queue" #(do {(sid) [:queue-all! (u/now)]}))
               ; (<API> "manual: process!" #(do {(sid) [:process-queue! (u/now)]}))
               []
               ;; extract all html so
               ["KV: 👨🏻‍🔬 html" (fn []
                                     (let [html (woof-dom/outer-html (q SCRAPE-CONTAINER-SELECTOR))]
                                       (.log js/console html)
                                       (ws/send-html-for-analysis html)))]

               []
               []

               ;;
               ;;
               #_["LOAD IDS FROM FS"
                (fn []
                  (ws/GET (str "http://localhost:9500/s/drv/" (name SRC) "_ids.edn")
                          (fn [raw-edn]
                            (let [ids (d/to-primitive raw-edn)]
                              (ws/POST "http://localhost:8081/kv/append"
                                       (fn []) {:k SRC :v ids})
                              )
                            )
                          )

                  )
                ]
               []
               (chord-action
                 (woof-dom/chord 49 :shift true :meta true) ;; shift+cmd+!
                 "IDS: save"
                 (fn [] (save-data!))
                 )

               ]
       }
      )
    )
  )



(defn ui-sub-wf [*WF-UI API]
  (assoc
    ;; cfg: debounce interval
    (rum-wf/ui-impl! *WF-UI rum-wf/<rum-ui>)
    :steps
    [(fn [params]
       {
        :CSS/scraper-styles   [:css-file "http://localhost:9500/css/scraper.css"]
        })]
    )
  )


;;;;;
;;
;; WF
;;
(defn wf! [*WF-STATE meta-info]
  (let [
        ;; state
        *WF-UI (rum/cursor-in *WF-STATE [:wf/UI])

        ;_RUN_ (runner-sub-wf :execute-mode :idle-w-timeout :t 10)

        ;; todo: extract all configuration to here, so there will be single place of configuring wf

        _SCRAPE_ (scraping-sub-wf *WF-STATE *WF-UI)
        api (get _SCRAPE_ :api [])

        _UI_ (ui-sub-wf *WF-UI api)
        _API_ (api-wf/actions-impl! api api-wf/default-on-chord)

        ]

    {
     :init            (concat
                        (get _UI_  :init [])
                        (get _API_ :init [])
                        (get _SCRAPE_ :init [])
                        )

     :ctx             (concat
                        (get _UI_     :ctx [])
                        (get _API_    :ctx [])
                        (get _SCRAPE_ :ctx [])
                        )

     :steps           (concat
                        (get _UI_  :steps [])
                        (get _API_ :steps [])
                        (get _SCRAPE_ :steps [])
                        )

     :opts            (concat
                        (get _UI_  :opts [])
                        (get _API_ :opts [])
                        (get _SCRAPE_ :opts [])
                        ;(get _RUN_ :opts [])
                        )

     ;; expose some wf API
     :api             api


     :on-stop         (fn [state]
                        ;;

                        ;; for now do not return channel
                        nil)

     ;; fixme:
     ;; for now provide custom on-run handler to handle the API in [["action" (fn [] ...)]], instead of older wf APIs
     ;; that are a map
     :scraper/on-run! (partial rum-wf/_on-run! *WF-UI)
     }
    )
  )