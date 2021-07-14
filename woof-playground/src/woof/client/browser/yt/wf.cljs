(ns woof.client.browser.yt.wf
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
       [woof.client.browser.yt.wf-ui :as yt-ui]

       )
  (:require-macros
    [woof.utils-macros :refer [inline--fn inline--fn1]]
    )
  )

;; scraping wf example
;; - get watch history
;; - process watch later




(defn _trigger-event [*wf-state steps] (evt-loop/_emit-steps (get @*wf-state :WF/params {}) steps))

(defn _action [*wf-state title steps-fn]
   [title (fn []
            (let [steps (steps-fn)]
                (evt-loop/_emit-steps (get @*wf-state :WF/params {}) steps)))])

;;
;; implementation of scraping sub-wf for HISTORY PAGE
;;
(defn history-scraping-sub-wf [*wf-state *WF-UI]
  (let [; helpers
        trigger-event (partial _trigger-event *wf-state)
        action (partial _action *wf-state)


        save!         (fn [k v] (ws/POST "http://localhost:8081/kv/put"
                                         (fn [] (.log js/console (str k " saved")))
                                         {:k k :v v}))


        ;; 
        ;; generic scraping parameters
        LINEARIZE? true

        ; data
        SEQ-ID ::seq  ;; sequential (linear) worker id
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
             (chord-action (woof-dom/chord 49 :shift true) "👀" woof-dom/scraping-ui__togle-details) ;; shift+!

             ["debug!" (fn []
                         (classes/toggle (.. js/document -body) "ZZZ"))]

             []

             ;; different scraping strategies

             ;; pass only selector
             (action ":batch-scrape-1!" #(do {(sid) [:loop-scrape SCRAPE-SELECTOR]}))
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


;;
;; scraper sub-wf for scraping watch later
;;


(defn playlist-parse-ctx [SCRAPE-SELECTOR]
		(let [
      ;; marks element as parsed
      scrape-start! (fn [el]
                      ;; todo: use dataset for prod (as won't trigger css refresh)
                      ;(dataset/set el "woof_scraped" "PROCESSING")

                      (classes/add el "WOOF-WIP")

                      ;; return sid or unique element id
                      (sid)
                      )
      ;;
      ;; whether element had been scraped
      is-scraping? (fn [el]
                     ;;(dataset/has el "woof_scraped")
                     (classes/has el "WOOF-WIP"))

  			 scrape-fn (fn [el]
  			 										   (.log js/console "SCRAPING" el)

  			 										   (try
  			 										   		(do 

		  			 										   		;; (classes/remove el "WOOF-WIP") 
		  			 										   		(let [r (parser/parse-playlist-video el)]
																									(classes/add el "WOOF-DONE")
																									(.log js/console "SRAPED" (d/pretty! r))
		  																					r )
  			 										   				)
  			 										   	(catch js/Error e
  			 										   					(do (.error js/console e) :err)))
               )

      ]

			(scrape/make-ctx-fn 
			;; indicates which elements to parse
               								SCRAPE-SELECTOR
               								scrape-fn
               								is-scraping?
																							scrape-start!
               							 
               							 )		
))


(defn dbg-params-fn [f]
	 (fn [params]
	 		(let [R (f params)]
	 			(.log js/console R)
	 			R)))


(rum/defc <item> < rum/static
  [a]

  [:div.foo (pr-str a)]
  )


(defn watch-later-scraping-sub-wf [*wf-state *WF-UI]
		(let [SCRAPE-SELECTOR "ytd-playlist-video-renderer:not(.WOOF-WIP)"
						 ;; generate scraping handlers 
						 SCRAPE-CTX (playlist-parse-ctx SCRAPE-SELECTOR)

							action (partial _action *wf-state)]
		{
				:api [
									 ; non-recurring scrapers
										(action ":loop-scrape" #(do {(sid) [:loop-scrape SCRAPE-SELECTOR]}))
										
										(action ":expand-scrape" #(do {(sid) [:expand-scrape SCRAPE-SELECTOR]}))

										(action ":step-scrape" #(let [k (sid)]
 																																	{k [:find-els SCRAPE-SELECTOR]
																																		(base/rand-sid) [:step-scrape k]
																																		}))

										(action ":8-step-scrape" #(let [k (sid)] 
																																	{k [:find-els SCRAPE-SELECTOR]
																																		(base/rand-sid) [:8-step-scrape k]
																																		}))

										[]

										["Add Items" (fn []
																											(let [container-el (woof-dom/q ".container")]

                                      (dotimes [n (rand-int 15)]
                                        (let [h (rum/render-static-markup (<item> (str n "__" (rand-nth ["A" "B" "C" "D" "E" "F" "G"]))))]
                                          (.insertAdjacentHTML container-el "beforeend" h))))
										)]
										; 
									]

				:init  []
				:ctx   [(dbg-params-fn SCRAPE-CTX)]
				:steps [
							{
								::hello [:prn "watch later scraper running"]
							}
				]
			}))




;; steps for simulating clicks 
(defn menu-clicker-ctx [params]
  {
    :condition {:fn (fn [pred?]
    			(let [chan-factory (base/&chan-factory params)
											 ch (base/make-chan chan-factory (base/rand-sid))]
										(async/go-loop [i 0]
            
            (if-let [v (pred?)] 
            	(async/put! ch v)
            	(do 
			            (async/<! (u/timeout (get [100 500 1000 1250 3000] i)))
			            (if (< i 5)
			              (recur (inc i))
			              ;; todo: figure out error handling, so no stuck steps will be left 
			              (.warn js/console "NO CONDITION WAS MET"))
              )
            ))

										ch
										)
    	)}

    ;; just do click
  		:click {:fn (fn [el]
  			 (try 
	  			 (.focus el)
	  				(.click el)

	  				:ok
  			 	(catch  js/Error e (.error js/console e))
  			 )
  		)}



  }
)


(defn- remove-playing-vid-from-playlist* []
			(let [k1 (rand-sid) k2 (rand-sid)
									c1 (rand-sid) c2 (rand-sid)

		       delete-menu? (fn [x] (str/includes? (woof-dom/txt x) "Вилучити"))
		       el-visible?  (fn [x] (not= 0 (.-clientHeight x)))
									] 
						{ 
							 ;; - find menu button of selected video in playlist view
							 k1 [:condition (fn [] (woof-dom/q "ytd-playlist-panel-video-renderer[selected] #button") )]
								c1 [:click k1] 

								;; todo: maybe add wait for first click to happen
								;; - find delete menu item in popup menu and select it
								k2 [:condition (fn []
																									 (let [_els (woof-dom/q* "ytd-menu-popup-renderer ytd-menu-service-item-renderer")
																									 					 els (filter (fn [x] (and (delete-menu? x) (el-visible? x))) _els)
																														[el] els
																									    ]
																								 el))]
								c2 [:click k2]
								} )
)


(defn copy-video-title* [] 
		(let [selector "h1.title.ytd-video-primary-info-renderer"
							 el (woof-dom/q selector)

							 TITLE (str 
							 	 "[" (woof-dom/txt el) "]"
							 	 "(" (str (.-location js/window)) ")") 
							 ]

							 (woof-dom/copy-to-clipboard TITLE)
		))


(defn video-scraping-sub-wf [*wf-state *WF-UI]
		(let [action (partial _action *wf-state)

						]
		{
				:api [
									 ;; extract YT video name in roam format
										(chord-action (woof-dom/chord 49 :shift true) "📋 video title" copy-video-title*) ;; shift+!

										[]
										;; remove YT video from playlist
										(action "🗑️ video from playlist" remove-playing-vid-from-playlist*)

									]

				:init  []
				:ctx   [menu-clicker-ctx]
				:steps [
							{
								::hello [:prn "watch later scraper running"]
							}
				]
			}))

;;
;; WF running strategies
;; 
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



(defn combine [aspects k]
		(apply concat (map #(get % k []) aspects	)))


;;
;;
;; WF IMPL
;;
(defn wf! [*wf-state meta-info]
  
  (let [*WF-UI (rum/cursor-in *wf-state [:wf/UI]) ;; state for UI updates

        ;; choose scraping impl based on page - watch later/history/etc.
  					 page-type (get meta-info :yt/t :history)
        _SCRAPE_ (condp = page-type
        												:watch-later (watch-later-scraping-sub-wf *wf-state *WF-UI)
        												:video (video-scraping-sub-wf *wf-state *WF-UI)
        												(history-scraping-sub-wf *wf-state *WF-UI)
        												)

        API (get _SCRAPE_ :api [])

  					 ;; WF aspects

        ASPECTS [(yt-ui/ui-sub-wf page-type *WF-UI)
        									(api-wf/actions-impl! API api-wf/default-on-chord) 
        									 _SCRAPE_ 
        									 (runner-sub-wf :execute-mode :idle-w-timeout :t 10)]

        ]

    {
    	;; todo: for now, sub-WFs are concatenated manually
     :init            (combine ASPECTS :init)
     :ctx             (combine ASPECTS :ctx)
     :steps           (combine ASPECTS :steps)
     :opts            (combine ASPECTS :opts)

     ;; expose some wf API
     :api             API

     :on-stop         (fn [state]
                        (__log "ON STOP")

                        ;; clean up added css during scraping
                        (woof-dom/remove-added-css [
                                                    "WOOF-WIP"
                                                    "WOOF-DONE"
                                                    "WOOF-SEEN"

                                                    "PROCESSED-VIDEO"
                                                    "DOUBLE-PROCESSED-VIDEO"
                                                    ])

                        ;; for now do not return channel - e.g. stop immediately
                        nil)

     ;; for now provide custom on-run handler - as older wf APIs are a map
     :scraper/on-run! (partial rum-wf/_on-run! *WF-UI)
     }
    )
  )
