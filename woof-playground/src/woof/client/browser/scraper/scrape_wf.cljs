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

    [rum.core :as rum]

    [woof.client.browser.scraper.rum-ui :as rum-wf]
    [woof.client.browser.scraper.actions :as api-wf :refer [chord-action]]

    ;; riel
    [woof.client.browser.rieltor.ui :as wf-ui]
    [woof.client.browser.rieltor.parser :as riel-parser]

    ;; domik
    [woof.client.browser.domik.parser :as domik-parser]
    ))


;;
;;
(defn- riel? [url]
  (or (str/starts-with? url "http://localhost:9500/r.html")
      (str/starts-with? url "https://rieltor.ua/")))

(defn- domik? [url]
  (or (str/starts-with? url "http://localhost:9500/d.html")
      (str/starts-with? url "http://domik.ua/")))


(defn get-source [url]
  (cond
    (riel? url) :riel
    (domik? url) :domik))


(defn get-container-selector [src]
  (cond
    (= :riel src) ".index-list-container"
    (= :domik src) "#divListObjects"
    ))


(defn get-scrape-selector [src]
  ;; :not(.WOOF-WIP) is very important
  (let [exclude-processed ":not(.WOOF-WIP):not(.WOOF-ERROR)"]
    (cond
      (= :riel src)  (str ".index-list-container > .catalog-item" exclude-processed)
      (= :domik src) (str "#divListObjects .objava" exclude-processed)
      )
    )
  )


(defn get-scrape-fn [src]
  (cond
    (= :riel src) riel-parser/scrape-element
    (= :domik src) domik-parser/scrape-element
    )
  )


;; results processing aspect
(defn _results-init [*WF-UI]
  (swap! *WF-UI assoc :scraped [])

  ;;

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

        SRC (get-source url)

        RESULTS_INIT (partial _results-init *WF-UI)
        RESULTS_ADD  (partial _results-add *WF-UI)
        RESULTS_READ (partial _results-read *WF-UI)

        SCRAPE-CONTAINER-SELECTOR (get-container-selector SRC)
        SCRAPE-SELECTOR (get-scrape-selector SRC)


        ;; pass *IDS
        *IDS (rum/cursor-in *WF-UI [:ids])
        SCRAPE! (get-scrape-fn SRC)
        ]



    ;; clean up added css
    (woof-dom/remove-added-css [
                                "WOOF-WIP"
                                "WOOF-DONE"
                                "WOOF-ASYNC"
                                "WOOF-SEEN"
                                "WOOF-SKIP"

                                "WOOF-ERROR"
                                "WOOF-PARSE-ERROR"
                                ;; debug
                                "DDD"
                                ])

    ;; remove added dom by woof
    ;(.warn js/console "REMOVING" (q* ".WOOF-DOM"))
    (doseq [el (q* ".WOOF-DOM")]
      (if-let [parent (.-parentElement el)]
        (.removeChild parent el)
        (do
          (.warn js/console "!!!" el)
          )
        )
      )



    ;; init state to hold dom els queue
    (swap! *WF-UI assoc :el-queue #queue [])
    (swap! *WF-UI assoc :process-queue #queue [])

    (swap! *WF-UI assoc :ids #{})
    (RESULTS_INIT)

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

          ]
      {

       :init  [(fn [params]
                 ; register the update channel
                 (let [cf (&chan-factory params)]
                   (own-chan cf :UPD-CHANNEL UPD-CHAN))
                 {})
               (fn [_]
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

                  :load-ids       {:fn (fn [SRC]
                                         (let [ch (make-chan (&chan-factory params) (rand-sid))]

                                           (ws/GET (str "http://localhost:8081/kv/get/" SRC)
                                                   (fn [_data]
                                                     (let [backend-ids (d/to-primitive _data)]
                                                       (if (nil? backend-ids)
                                                         (ws/POST "http://localhost:8081/kv/put"
                                                                  (fn []
                                                                    (async/put! ch #{}))
                                                                  {:k SRC :v #{}})
                                                         (do
                                                           (.log js/console "loaded ids" (pr-str backend-ids))
                                                           (async/put! ch backend-ids)
                                                           (swap! *WF-UI assoc :ids backend-ids)
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
                                         ;; stop the ticker
                                         (reset! *ticker-ready? true)

                                         ;; disconnect observers
                                         (.disconnect intersector)
                                         (.disconnect mutator)))]

       :steps [
               {
                :WS/LOAD-IDS    [:load-ids SRC]
                ;; :log/loaded-ids [:log :WS/LOAD-IDS]


                ;; debug css
                :css/attr-0     [:css-rules* [".DDD:hover" "outline: 5px solid crimson; \n background-color: rgba(255,0,0,.5);"]]
                :css/attr-1     [:css-rules* [".DDD > *" "z-index: 100;"]]
                :css/attr-2     [:css-rules* [".DDD:after" "z-index: 1; \n content: \"↑\" attr(data-parse-id) \"↑\"; b \n display: flex; \n background-color: red; \n font-weight: bolder; \n color: white; \n height: 20px; \n outline: 1px solid crimson;"]]
                :css/attr-3     [:css-rules* [".DDD:before" "content: \"↓\" attr(data-parse-id) \"↓\"; b \n display: flex; \n background-color: red; \n font-weight: bolder; \n color: white; \n height: 20px; \n outline: 1px solid crimson;"]]

                }
               ]

       :api   [
               ;;
               (chord-action (woof-dom/chord 49 :shift true :meta true) ;; shift+cmd+!
                             "START(scroll)"
                             (fn []

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
                                 (evt-loop/_emit-steps (get @*wf-state :WF/params {}) steps))))

               (<API> "START(greedy)"
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
               ["debug-info"
                (fn []
                  ;; why there are some elements that are .WOOF-WIP, but taken from :el-queue?
                  (let [els (q* ".WOOF-WIP:not(.WOOF-DONE):not(.WOOF-ASYNC)")
                        queued (into [] (:el-queue @*WF-UI))
                        process-q (into [] (:process-queue @*WF-UI))
                        ]
                    (.log js/console "els:" els
                          "queue:" queued
                          "process" process-q)
                    )
                  )]
               []

               (<API> "manual: queue" #(do {(sid) [:queue-all! (u/now)]}))
               (<API> "manual: process!" #(do {(sid) [:process-queue! (u/now)]}))
               []
               ;; extract all html so
               ["KV: 👨🏻‍🔬 html" (fn []
                                     (let [html (woof-dom/outer-html (q SCRAPE-CONTAINER-SELECTOR))]
                                       (.log js/console html)
                                       (ws/send-html-for-analysis html)))]

               []
               []
               []
               ["IDS: save"
                (fn []
                  (let [ROWS (RESULTS_READ)
                        IDS @*IDS]
                    (.group js/console "SAVE:")
                    ;; chain saving results
                    (ws/POST "http://localhost:8081/kv/append"
                             (fn []
                               (.log js/console "IDS...saved!")
                               (ws/POST "http://localhost:8081/kv/append"
                                        (fn []
                                          (.log js/console "ROWS...saved!")
                                          (.groupEnd js/console))
                                        {:k :rows :v ROWS}))
                             {:k SRC :v IDS})
                    )

                  )
                ]

               ]
       }
      )
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

        _UI_ (wf-ui/ui-sub-wf *WF-UI api)
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

     ;; for now provide custom on-run handler - as older wf APIs are a map
     :scraper/on-run! (partial rum-wf/_on-run! *WF-UI)
     }
    )
  )