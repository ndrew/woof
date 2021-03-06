(ns woof.client.browser.example.seq-wf-a
  (:require
    ;; core
    [cljs.core.async :as async :refer [go go-loop]]


    ;; woof core
    [woof.base :as base :refer [rand-sid sid
                                &chan-factory make-chan own-chan]]
    [woof.data :as d]
    [woof.utils :as u]

    ;; client utils
    [woof.client.dom :as woof-dom :refer [q q* txt dataset ]]
    [woof.client.dbg :as dbg :refer [__log]]

    ;; wf helpers -
    [woof.client.ws :as ws]

    ; helpers from base
    [woof.wfs.alpha :as alpha]
    [woof.wfs.evt-loop :as evt-loop]

    ;; ui
    [goog.dom.classes :as classes]
    [goog.dom.dataset :as dataset]

    [rum.core :as rum]

    [woof.client.playground.ui :as ui]
    [woof.client.browser.scraper.rum-ui :as rum-wf]
    [woof.client.browser.scraper.actions :as api-wf :refer [chord-action]]

    ; another scrape impl
    ; [woof.client.browser.scraper.scrape :as scrape]
    )
  )

;;
;; another take on scraping workflow
;; - use queue for elements to be processed
;; - use queue for processing elements

;; todo: maybe use channel instead queue?
  ;; channel is not observable. or it is?

;; - use ticker to trigger queuing elements and processing them

;; custom css:
;; "http://localhost:9500/css/t.css"
;; "http://localhost:9500/css/r.css"



;;
;; _UI_

(rum/defc <scraping-ui> < rum/static
  [*state STATE]

  [:div#ui-impl
   ;; display api bar
   (when (seq (:api STATE))
     (ui/menubar "API" (:api STATE) :class "woof_api"))

   [:div
    [:table ;{:border "1"}
     [:thead
      [:tr
       [:th "EL Q:"]
       [:th "PROC Q:"]
       [:th "RESULTS"]
       ]
      ]
     [:tbody
      [:tr
       [:td
        (if (seq (:el-queue STATE))
          (pr-str (count (:el-queue STATE)))
          "<empty>")
        ]
       [:td
        (if (seq (:process-queue STATE))
          (pr-str (count (:process-queue STATE)))
          "<empty>"
          )
        ]
       [:td
        (when (seq (:results STATE))
          (let [data (:results STATE)]
            [:div.html
             (pr-str (count data))
             [:button {:on-click (fn[_] (.log js/console
                                              (d/pretty!
                                                (->> data
                                                     (sort (fn [a b] (compare (:id a) (:id b))))
                                                     ;; (reverse)
                                                    )

                                                )))}
              "RESULTS"
              ]
             ]
            ;(pr-str (map :id (:results STATE)))  )
          ))
        ]
       ]
      ]
     ]
    ]
   ]
  )


(def <rum-ui> (rum-wf/gen-rum-ui <scraping-ui>))


(defn ui-sub-wf [*WF-UI API]
  (assoc
    ;; cfg: debounce interval
    (rum-wf/ui-impl! *WF-UI <rum-ui>)
    :steps
    [(fn [params]
       {
        :CSS/test-page-styles [:css-file "http://localhost:9500/css/t.css"]

        :CSS/scraper-styles   [:css-file "http://localhost:9500/css/r.css"]
        })]
    )
  )


;;
;; SCRAPE
;;



;(def SCRAPE-SELECTOR "article:not(.WOOF-WIP)")
;(def SCRAPE-CONTAINER-SELECTOR "#container")


;; tg
(def TG-SCRAPE-SELECTOR ".im_history_message_wrap:not(.WOOF-WIP)")
(def TG-SCRAPE-CONTAINER-SELECTOR ".im_history_wrap")



(defn tg-scrape!
  [el]

  (let [$id (q el ".im_message_outer_wrap")]
    (merge
      {
        :id (cljs.reader/read-string (get (dataset $id) :msgId "-1"))
       }

      (if-let [$author (q el ".im_message_author")]
        {:tg (txt $author)} {})

      (if-let [$day (q el ".im_message_date_split_text")]
        {:day (txt $day)} {})

      (if-let [$date (q el ".im_message_date_text")]
        {:time (get (dataset $date) :content)} {})

      (if-let [$text (q el ".im_message_text")]
        {:txt (txt $text)
         :_txt (woof-dom/outer-html $text)
         }
        {}
        )
      (if-let [$media (q el ".im_message_media")]
        {:media (txt $media)
         :_media (woof-dom/outer-html $media)
         }
        {}
        )
      )
    )


  #_(let [p (rand-int 100)]

    (let [r (cond
              (< p 10) (u/throw! "STOPPING")
              (< p 60) (do
                         ;(classes/add el "WOOF-DONE")
                         (woof-dom/txt el))
              (< p 80) (do
                         (let [c (async/chan)]
                           (go
                             (async/<! (u/timeout (rand-int 1500)))
                             (async/put! c (str "ASYNC:" (woof-dom/txt el)))
                             )
                           c)
                         )
              :else nil
              )]
      (.log js/console "SCRAPE" (woof-dom/txt el) r)
      r
      )
    )

  )

(def SCRAPE-SELECTOR TG-SCRAPE-SELECTOR)
(def SCRAPE-CONTAINER-SELECTOR TG-SCRAPE-CONTAINER-SELECTOR)

(defn scrape!
  [el]
  (tg-scrape! el)
  )

(comment
  ; deezer
  (def DEEZER-SCRAPE-SELECTOR ".datagrid .datagrid-row.song:not(.WOOF-WIP)")
  (def DEEZER-SCRAPE-CONTAINER-SELECTOR ".datagrid ")


  (defn deezer-scrape! [el]
    ;; .datagrid-cell .datagrid-track-number

    ;; .datagrid-cell.cell-title *[itemprop="name"]
    ;; .datagrid-cell.cell-artist *[itemprop="byArtist"]
    ;; .datagrid-cell.cell-album *[itemprop="inAlbum"]
    ;; .datagrid-cell.cell-duration span

    {
     :n (txt (q el ".datagrid-cell .datagrid-track-number"))
     :title (txt (q el ".datagrid-cell.cell-title *[itemprop=\"name\"]"))
     :artist (txt (q el ".datagrid-cell.cell-artist *[itemprop=\"byArtist\"]"))
     :album (txt (q el ".datagrid-cell.cell-album *[itemprop=\"inAlbum\"]"))
     :duration (txt (q el ".datagrid-cell.cell-duration span"))
     }
    )

  (def SCRAPE-SELECTOR DEEZER-SCRAPE-SELECTOR)
  (def SCRAPE-CONTAINER-SELECTOR DEEZER-SCRAPE-CONTAINER-SELECTOR)

  (defn scrape!
    [el]
    (deezer-scrape! el)
    )
  )

;;
;; prototype of scrape function
;; - can return scrape result (sync)
;; - can return chan: and do async procession
;; - can throw exception - ???
;; - can return nil - ???
#_(defn scrape!
  "scrapes the element"
  [el]
  (let [p (rand-int 100)]

    (let [r (cond
      (< p 10) (u/throw! "STOPPING")
      (< p 60) (do
                 ;(classes/add el "WOOF-DONE")
                 (woof-dom/txt el))
      (< p 80) (do
                 (let [c (async/chan)]
                   (go
                     (async/<! (u/timeout (rand-int 1500)))
                     (async/put! c (str "ASYNC:" (woof-dom/txt el)))
                     )
                   c)
                 )
      :else nil
      )]
      (.log js/console "SCRAPE" (woof-dom/txt el) r)
      r
      )
    )
  )



;;
;; scraping implementation
(defn scraping-sub-wf [*wf-state *WF-UI]

  ;; els -> candidates -> result

  ;; init state to hold dom els queue
  (swap! *WF-UI assoc :el-queue #queue [])
  (swap! *WF-UI assoc :process-queue #queue [])

  (swap! *WF-UI assoc :results [])

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
                           ;:root nil
                           :threshold 0 ;[0 1]
                           ; mark fixed bottom: 200px
                           ;:rootMargin "0px 0px -200px 0px" ; (top, right, bottom, left)
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
               {})]

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
                                 (.log js/console "UPD:" (- now t) "ms" )
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
             {

              ;; attaches scroll observers on scraped elements
              :queue-in-view! {:fn (fn [ticker]
                                     ;(.log js/console ":observe!" ticker)
                                     (intersect-observe!)

                                     ;;
                                     (u/now)
                                     )}

              ;; adds all found elements to element queue (without scrolling)
              :queue-all!     {:fn (fn [ticker]
                                     (.log js/console ":queue!")
                                     (let [selector SCRAPE-SELECTOR
                                           els (q* selector)]

                                       (doseq [el els]
                                         (classes/add el "WOOF-WIP"))
                                       (swap! *WF-UI update :el-queue into els)

                                       (u/now)
                                       ))}

              ;; process
              :process-queue! {:fn (fn [ticker]
                                     ;(.log js/console "processing!!!" ticker)

                                     (let [p-queue (get-in @*WF-UI [:process-queue])]
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
                                               (if-let [result (scrape! el)]
                                                 (if (u/channel? result)
                                                   (do
                                                     ;; handling if channel is returned
                                                     (classes/add el "WOOF-ASYNC") ; mark element as in progress
                                                     (go
                                                       (let [r (async/<! result)] ; wait for result, todo: alts
                                                         (classes/add el "WOOF-DONE")
                                                         (swap! *WF-UI update :results into [r]))))
                                                   (do
                                                     ;; handling value
                                                     (classes/add el "WOOF-DONE")
                                                     (swap! *WF-UI update :results into [result])
                                                     ))
                                                 (do ;; nil - skip node

                                                   ; can't process further, re-adding el back to the queue
                                                   ;(swap! *WF-UI update :el-queue conj el)

                                                   ;; add back to process queue?
                                                   (swap! *WF-UI update :process-queue conj el)
                                                   )
                                                 )
                                               (catch js/Error e
                                                 (.error js/console e)
                                                 ; can't process further, re-adding el back to the queue
                                                 (swap! *WF-UI update :el-queue conj el)
                                                 )
                                               )
                                             )
                                           )
                                         )
                                       (do
                                         (.log js/console "PROCESSING IN PROGRESS")
                                         )
                                       )
                                     )}
              }]

     :opts  [(base/build-opt-on-done (fn [_]
                                       ;; stop the ticker
                                       (reset! *ticker-ready? true)

                                       ;; disconnect observers
                                       (.disconnect intersector)
                                       (.disconnect mutator)

                                       ))]

     :steps [
             {
              ::hello [:log "hello!"]
              }
             ]

     :api   [
             ;;
             (chord-action (woof-dom/chord 49 :shift true :meta true) ;; shift+cmd+!
               "START(scroll)"
                           (fn []
                             (intersect-observe!)
                             (mutations-observe!)

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
             ;; dynamically add more
             ["emulate loading"
              (fn []
                (let [container-el (woof-dom/q "#container")]
                  (dotimes [n (rand-int 15)]
                    (.insertAdjacentHTML container-el "beforeend" (str "<article>ADDED " n "</article>"))
                    )
                  )
                )]

             ; (<API> "observe: scroll" #(do {(sid) [:queue-in-view! SCRAPE-SELECTOR]}))

             (<API> "manual: queue" #(do {(sid) [:queue-all! (u/now)]}))
             (<API> "manual: process!" #(do {(sid) [:process-queue! (u/now)]}))
             []
             ;; extract all html so
             ["KV: 👨🏻‍🔬 html" (fn []
                           (let [html (woof-dom/outer-html (q SCRAPE-CONTAINER-SELECTOR))]
                             (.log js/console html)
                             (ws/send-html-for-analysis html)))]
             ]
     }
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
                        ; (__log "ON STOP")
                        ;; clean up added css
                        (woof-dom/remove-added-css [
                                                    "WOOF-WIP"
                                                    "WOOF-DONE"
                                                    "WOOF-ASYNC"
                                                    ])

                        ;; for now do not return channel
                        nil)

     ;; for now provide custom on-run handler - as older wf APIs are a map
     :scraper/on-run! (partial rum-wf/_on-run! *WF-UI)
     }
    )
  )