(ns woof.client.browser.scraper.scrape
  (:require

    [cljs.core.async :refer [go go-loop] :as async]

    [goog.dom :as dom]
    [goog.dom.classes :as classes]

    [woof.base :as base :refer [&chan-factory make-chan
                                rand-sid sid]]
    [woof.utils :as u :refer [sid?]]
    
    [woof.client.dom :as woof-dom :refer [q q*]]

    [woof.wfs.evt-loop :as evt-loop]
    [woof.wfs.alpha :as alpha]

    ))

;;
;; WIP: shared scraping code 
;; 



(defn _tick [params [t max-num]]
  (let [chan-factory (&chan-factory params)
        chan (make-chan chan-factory (rand-sid))]

    (async/go-loop [i 0]
                   (async/>! chan (u/now))
                   (async/<! (u/timeout t))

                   (if (< i max-num)
                     (recur (inc i))))

    chan))





;;
;; context for different scraping strategies
;; -
(defn make-ctx-fn [SCRAPE-SELECTOR
                   SCRAPE-FN
																			is-scraped?
																			mark-scraped!
                   ;; todo: configure recurring scraping wait time
                   ]

  ;;
  ;;

  (let [
        ;; idea: is to have certain prefix, so these can be extracted in opts
        PARSE-STEP-PREFIX "SCRAPE__"


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
                             sid (if (sid? _sid)
                                   _sid
                                   (base/rand-sid PARSE-STEP-PREFIX))]
                         {sid [:scrape-el el]}))

        ;; re-curring expand steps

        *brute-force-counter (atom 0) ;; why this is here?
        RECUR-MAX-WAIT-TIME (* 10 1000)
        RECUR-WAIT-NO-ELS 300
        RECUR-WAIT-AFTER-ELS-APPEARED 1000

        recurring-scrape-expand! (fn [els]
	         (let [
	               k_items (base/rand-sid)
	               k_selector (base/rand-sid)
	               k_log (base/rand-sid)
	               k_scroll-wait-time (base/rand-sid)
	               k_!selector (base/rand-sid)

	               empty-els? (empty? els)

	               wait-time (if empty-els?
	                           (* RECUR-WAIT-NO-ELS (swap! *brute-force-counter inc))
	                           (do
	                             (reset! *brute-force-counter 0)
	                             RECUR-WAIT-AFTER-ELS-APPEARED))
	               ]

	           (if (> wait-time RECUR-MAX-WAIT-TIME)
	             {
	                k_log [:warn (str "stopping attempts to scrape")]
	             }
	             ;; no more els to scrape - scroll and brute force again
	             (merge
	               ;; if no els - auto scroll
	               #_(if empty-els? {(base/rand-sid) [:scroll (rand-nth [1 2 3])]}
	                                {})
	               {}
	               {
	                k_selector         [:v SCRAPE-SELECTOR]
	                k_log              [:log (str "recurring scraper scheduled in " wait-time " ms")]
	                k_scroll-wait-time [:v (u/timeout wait-time)]

	                k_!selector        [:wait-rest [k_selector k_scroll-wait-time]]
	                k_items            [:find-els k_!selector]

	                (base/rand-sid)    [:8-step-scrape k_items]
	                }
	               )
	             )
	           )
	         )


        ; expand-scrape
        _expand-scrape (fn [is-scraped? mark-scraped! process-step selector]
            ;;(.log js/console "simple scrape: A")

            ;; try to find elements to be processed, but skip already processed
            (let [els (filter (fn [el] (not (is-scraped? el)))
                              (q* selector))]

              ;;(.log js/console "els" els (woof-dom/q* selector))

              (reduce (fn [a el]
                        (let [_sid (mark-scraped! el)
                              sid (if (qualified-keyword? _sid)
                                    _sid
                                    (rand-sid PARSE-STEP-PREFIX))]
                          (assoc a sid [process-step el])))
                      {} els)
              ))
        ]

    (fn [params]
       {
       	;; intermediary steps

       	; [:scrape-el <el>] => data|chan - scrape step 
     			:scrape-el          {:fn SCRAPE-FN}
     			; [:find-els ".foo"] => <el>[]
        :find-els           {:fn (fn [selector]
                                  (filter (fn [el] (not (is-scraped? el))) 
                                  			(woof-dom/q* selector))

                                  #_(take 10 (filter (fn [el] (not (is-scraped? el))) (woof-dom/q* selector))) ;; uncomment for limitted 
                                  )
                            }

        :tick               {:fn       (partial _tick params)
                             :infinite true}

        :rnd-scroll         {:fn (fn [_]
                                  (rand-nth [1 2 3]))}

        :8-scroll 	{:fn (partial _infinite-scroll! params
                               (fn [] + 1000 (int (rand 1000)))
                               (fn []
                                 (.scrollBy js/window 0  (* (.-innerHeight js/window)
                                                            (rand-nth [1 2 3])))
                                 )
                               )
                  :infinite true
                  }


       ;=====================================
       ; 
       ; scrape all found elements in a loop (via non-expand step handler)
       ; 
       ;  + preserves order
       ;  - no intermediary results
       ;  - sync scrape fn can block the WF
       ;  - non-recurring
       ; 
       ;  ? what to return ?
       ; 
       ;; old name brute!
       :loop-scrape {

          :fn (fn [selector]
                 ;; try to find elements to be processed, but skip already processed
                 (let [els (filter (fn [el] (not (is-scraped? el)))
                                   (q* selector))]

                   ;; process each el by one
                   (doseq [el els]
                     (let [_sid (mark-scraped! el)]
                       (SCRAPE-FN el)))

                   :ok))
       }



       ;;==================================
       ;;
       ;; expand scrape
       ;; 
       ;; 
       :expand-scrape {
          :fn       (partial _expand-scrape 
          																					is-scraped? mark-scraped! :scrape-el)
          :expands? true
          :collect? true
       }

       ;;
       ;; recurring scrapers 
       ;;
       ;; ================== 
       ;; scrape items from els step
       ;;
       :step-scrape        {:fn       (partial _expander!
                                               (fn [] {})
                                               item-expand!)
                            :collect? true
                            :expands? true}

       ;; recurring step scrape
       :8-step-scrape     {:fn       (partial _expander!
                                               recurring-scrape-expand!
                                               item-expand!)
                            :collect? true
                            :expands? true}

        }
      )
    )
  )

