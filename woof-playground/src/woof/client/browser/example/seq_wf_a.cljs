(ns woof.client.browser.example.seq-wf-a
  (:require
    ;; core
    [clojure.string :as str]
    [cljs.core.async :as async :refer [go go-loop]]


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
    [goog.dom.classes :as classes]
    [goog.dom.dataset :as dataset]

    [rum.core :as rum]

    [woof.client.playground.ui :as ui]
    [woof.client.browser.scraper.rum-ui :as rum-wf]
    [woof.client.browser.scraper.actions :as api-wf :refer [chord-action]]
    [woof.client.browser.scraper.scrape :as scrape]
    )
  )



(defn clean-up-dom-side-effects []
  ;; clean up added css
  (woof-dom/remove-added-css [
                              "WOOF-WIP"
                              "WOOF-DONE"
                              "WOOF-ASYNC"
                              ])
  )

;; _UI_

(rum/defc <scraping-ui> < rum/static
  [*state STATE]

  [:div
   ;; api
   (when (seq (:api STATE))
     (ui/menubar "API" (:api STATE)
                 :class "woof_api"))

   [:hr]

   (when (seq (:el-queue STATE))
     (pr-str (count (:el-queue STATE)))
     )

   (when (seq (:results STATE))
     (pr-str (:results STATE))
     )

   ]
  )

(def <rum-ui> (rum-wf/gen-rum-ui <scraping-ui>))


(defn ui-sub-wf [*WF-UI API]
  (assoc
    (rum-wf/ui-impl! *WF-UI <rum-ui>)
    :steps [(fn [params]
              {
               :CSS/test-styles    [:css-file "http://localhost:9500/css/t.css"]
               :CSS/-styles    [:css-file "http://localhost:9500/css/r.css"]
               })]
    )
  )


;;
;; _RUN_

;; copy-pasted
#_(defn runner-sub-wf [& {:keys [execute-mode
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

;;
;; SCRAPE
;;



(def SCRAPE-SELECTOR "article:not(.WOOF-WIP)")
(def SCRAPE-CONTAINER-SELECTOR "#container")


(defn scrape!
  "scrapes the element"
  [el]
  (let [p (rand-int 100)]

    (let [r (cond
      (< p 10) (u/throw! "STOPPING")
      (< p 80) (do
                 ;(classes/add el "WOOF-DONE")
                 (woof-dom/txt el))
      (< p 90) (do
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

  (let [api-action (fn [title steps-fn]
                     [title (fn []
                              (let [steps (steps-fn)]
                                (evt-loop/_emit-steps (get @*wf-state :WF/params {}) steps)))])

        UPD-CHAN (async/chan)
        *ticker-ready? (atom false)


        ;; add element to the processing queue
        queue! (fn [el]
                 (.log js/console "QUEUED" (woof-dom/txt el))
                 (classes/add el "WOOF-WIP")
                 (swap! *WF-UI update :el-queue conj el)

                 )

        mutator (js/MutationObserver.
                  (fn [mut] (async/put! UPD-CHAN :mutation)))

        intersector (js/IntersectionObserver.
                      (fn [entries observer]
                        (.group js/console "intersect")

                        (doseq [entry (array-seq entries)]
                          (let [el (.-target entry)

                                frac (-> (.-intersectionRatio entry)
                                         (* 100) int
                                         float (/ 100))

                                use-el? (.-isIntersecting entry)]

                            (if use-el?
                              (do
                                (.log js/console frac (woof-dom/txt el))
                                (queue! el)
                                (.unobserve observer el)
                                )
                              #_(do
                                (.log js/console (woof-dom/txt el) entry))
                              )
                            )
                          )
                        (.groupEnd js/console))
                      #js {
                           ;:root nil
                           :threshold 0 ;[0 1]

                           ; mark fixed bottom: 200px
                           ;:rootMargin "0px 0px -200px 0px" ; (top, right, bottom, left)
                           :rootMargin "0px 0px -100px 0px" ; (top, right, bottom, left)
                           })

        ]
    {

     :init  [(fn [params]

               ; register the update channel
               (let [cf (&chan-factory params)]
                 (own-chan cf :UPD-CHANNEL UPD-CHAN))

               ;; intersection observer
               (doseq [el (q* SCRAPE-SELECTOR)]
                 (.observe intersector el))

               ;; mutation observer
               (let [el (q SCRAPE-CONTAINER-SELECTOR)
                     cfg #js {:characterData true
                              :childList true
                              :subtree true}]
                 (.observe mutator el cfg)
                 )

               {}
               )]

     :ctx   [(fn [params]
               (let [_cond-ticker-fn (fn [pred? ticker]
                                      (let [ch (make-chan (&chan-factory params) (sid))]
                                        (if (pred? ticker)
                                          (async/put! ch ticker))
                                        ch))]

                 {
                  ; main ticker for wf updates
                  :ticker {:fn (fn [chan]
                                 (.log js/console "TICKER")
                                 ;; maybe some xf here?

                                 (go-loop []
                                   (let [msg (async/alt!
                                               UPD-CHAN ([v] v)

                                               (alpha/request-idle-callback-chan!) ([] :tick) ; instead of idle

                                               (u/timeout 100) ([] :tick))] ;; todo: recur0

                                     (cond
                                       (= :mutation msg) (async/put! chan msg)

                                       (and (= :tick msg)
                                            (not (empty? (:process-queue @*WF-UI))))
                                       (async/put! chan :process)

                                       (and (= :tick msg)
                                            (not (empty? (:el-queue @*WF-UI))))
                                       (async/put! chan :process)

                                       :else (do
                                               ;(.log js/console "OTHER" msg)
                                               ))

                                     (if-not @*ticker-ready?
                                       (do
                                         ;(.log js/console "(RECUR)")
                                         (recur))
                                       )
                                     )
                                   )
                                 chan
                                 )
                           :infinite true}


                  ;; push ticker
                  :queue-els-ticker {:fn (partial _cond-ticker-fn #(= :mutation %))
                                    :infinite true}


                  ;; pop ticker
                  :process-els-ticker {:fn (partial _cond-ticker-fn #(= :process %))
                                       :infinite true}
                  }
                 )

               )
             {

              :queue-in-view! {:fn (fn [ticker]
                               (.log js/console ":observe!" ticker)

                               (let [selector SCRAPE-SELECTOR]
                                 (doseq [el (q* selector)]
                                   (.observe intersector el))

                                 (str "o-" (u/now))
                                 )
                               )}

              ;;
              :queue-all! {:fn (fn [ticker]
                                        (.log js/console ":queue!")
                                        (let [selector SCRAPE-SELECTOR
                                              els (q* selector)]

                                          (doseq [el els]
                                            (classes/add el "WOOF-WIP"))
                                          (swap! *WF-UI update :el-queue into els)

                                          :ok))}


              :process-queue! {:fn (fn [ticker]
                                  (.log js/console "processing!!!" ticker)

                                   (let [p-queue (get-in @*WF-UI [:process-queue])]
                                     (if (empty p-queue)
                                       (do

                                         ;; copy the values from el-queue to process-queue
                                         (swap! *WF-UI update :process-queue into
                                                (get-in @*WF-UI [:el-queue] []))
                                         ;; clean-up the el-queue - todo: maybe copy only chunk of elements
                                         (swap! *WF-UI assoc :el-queue #queue [])


                                         (let [el (peek (get-in @*WF-UI [:process-queue]))]
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

     :opts [(base/build-opt-on-done (fn [_]
                                      ;; stop the ticker
                                      (reset! *ticker-ready? true)

                                      ;; disconnect observers
                                      (.disconnect intersector)
                                      (.disconnect mutator)
                                      ))]

     :steps [
             {
              ;; ::hello [:log "hello!"]

              :EVT/tick [:ticker UPD-CHAN]
              ;:DOM/intersect-observer [:queue-in-view! :EVT/tick]
              ;:PROCESS/process [:process-queue! :EVT/tick]


              :TICK/el-queue [:queue-els-ticker :EVT/tick]
                :SCROLL/register-handlers [:queue-in-view! :TICK/el-queue]

              :TICK/el-process [:process-els-ticker :EVT/tick]
                :DOM/process [:process-queue! :TICK/el-process]

              }
             ]

     :api   [

             ["find lost els" (fn []
                                ;; why there are some elements that are .WOOF-WIP, but taken from :el-queue?
                                (let [els (q* ".WOOF-WIP:not(.WOOF-DONE):not(.WOOF-ASYNC)")
                                      queued (into [] (:el-queue @*WF-UI))
                                      ]
                                  (.log js/console "els:" els
                                                   "queue:" queued )
                                  )
                                )]
             []
             ["emulate loading" (fn []
                                  (let [container-el (woof-dom/q "#container")]

                                    (dotimes [n (rand-int 15)]

                                      (.insertAdjacentHTML container-el "beforeend" (str "<article>ADDED " n "</article>"))
                                      )
                                    )
                                  )]

             ["log queue" (fn []
                      (.log js/console (get-in @*WF-UI [:el-queue]))
                      )]

             (api-action "observe: scroll" #(do {(sid) [:queue-in-view! SCRAPE-SELECTOR]}))

             (api-action "queue all!" #(do {(sid) [:queue-all! SCRAPE-SELECTOR]}))
             (api-action "process!" #(do {(sid) [:process-els SCRAPE-SELECTOR]}))
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
                        (clean-up-dom-side-effects)

                        ;; for now do not return channel
                        nil)

     ;; for now provide custom on-run handler - as older wf APIs are a map
     :scraper/on-run! (partial rum-wf/_on-run! *WF-UI)
     }
    )
  )