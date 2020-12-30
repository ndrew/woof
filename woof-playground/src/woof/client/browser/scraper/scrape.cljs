(ns woof.client.browser.scraper.scrape
  (:require

    [cljs.core.async :refer [go go-loop] :as async]

    [goog.dom :as dom]
    [goog.dom.classes :as classes]

    [woof.base :as base]
    [woof.utils :as u]
    
    [woof.client.dom :as woof-dom]

    [woof.wfs.evt-loop :as evt-loop]

    ))

;;
;; WIP: shared scraping code 
;; 



(defn _tick [params [t max-num]]
  (let [chan-factory (base/&chan-factory params)
        chan (base/make-chan chan-factory (base/rand-sid))]

    (async/go-loop [i 0]
                   (async/>! chan (u/now))
                   (async/<! (u/timeout t))

                   (if (< i max-num)
                     (recur (inc i))))

    chan))




;;
;;


#_{:scrape-el          {:fn (fn [el]
                              (try
                                (let [result (parser/scrape-element el)]
                                  ;; todo: ...
                                  (swap! *SCRAPED-DATA conj result)

                                  (swap! *WF-scrape-data conj result)

                                  result)
                                (catch js/Error e
                                  (do
                                    (classes/add el "parsed-error")

                                    (.error js/console e)

                                    (swap! *FAILED conj (woof-dom/el-map el))

                                    )
                                  )
                                )

                              )}
   }



(defn make-ctx-fn [mark-scraped!
                   is-scraped?

                   SCRAPE-SELECTOR
                   SCRAPE-FN]
  (let [
        ;; brute-force scraping, pass all parameters and retrieve, filter els in it
        _simple-brute-force (fn  [is-scraped? mark-scraped! process-step selector]
                              
                              ;;(.log js/console "simple scrape: A")

                              ;; try to find elements to be processed, but skip already processed
                              (let [els (filter (fn [el] (not (is-scraped? el)))
                                                (woof-dom/q* selector))]

                                ;;(.log js/console "els" els (woof-dom/q* selector))

                                (reduce (fn [a el]
                                          (let [_sid (mark-scraped! el)
                                                sid (if (qualified-keyword? _sid)
                                                      _sid
                                                      (base/rand-sid "el-"))]
                                            (assoc a sid [process-step el])
                                            )
                                          ) {} els)

                                ))

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
                             sid (if (qualified-keyword? _sid)
                                   _sid
                                   (base/rand-sid "el-"))]
                         {sid [:scrape-el el]}))

        *brute-force-counter (atom 0)

        ;; re-curring expand steps
        recurring-scrape-expand! (fn [els]
                                   (let [
                                         k_items (base/rand-sid)

                                         k_selector (base/rand-sid)

                                         k_log (base/rand-sid)

                                         k_scroll-wait-time (base/rand-sid)
                                         k_scroll-amount (base/rand-sid)

                                         k_!selector (base/rand-sid)

                                         wait-time (if (empty? els)
                                                     (* 1000 (swap! *brute-force-counter inc))
                                                     (do
                                                       (reset! *brute-force-counter 0)
                                                       5000)
                                                     )
                                         ]

                                     (if (> wait-time (* 15 1000))
                                       (do
                                         {k_log [:log (str "stopping attempts to scrape")]}
                                         )
                                       ;; no more els to scrape - scroll and brute force again
                                       {
                                        k_selector         [:v SCRAPE-SELECTOR]

                                        k_log              [:log (str "recurring scraper scheduled in " wait-time " ms")]

                                        ;; if scroll needed it can be also done
                                        ;; k_scroll-amount    [:scroll (rand-nth [1 2 3])]

                                        k_scroll-wait-time [:v (u/timeout wait-time)]

                                        k_!selector        [:wait-rest [k_selector k_scroll-wait-time]]
                                        k_items            [:find-els k_!selector]

                                        ;;
                                        (base/rand-sid)    [:brute-recurring k_items]
                                        }
                                       )
                                     )
                                   )
        ]

    (fn [params]
       {
        :scrape-el          {:fn (fn [el]
                                   (SCRAPE-FN el)
                                   )}

       :tick               {:fn       (partial _tick params)
                            :infinite true
                            }
       :rnd-scroll         {:fn (fn [_]
                                  (rand-nth [1 2 3]))}

       :8-scroll {:fn (partial _infinite-scroll! params
                               (fn [] + 1000 (int (rand 1000)))
                               (fn []
                                 (.scrollBy js/window 0  (* (.-innerHeight js/window)
                                                            (rand-nth [1 2 3])))
                                 )
                               )
                  :infinite true
                  }



       ;;
       ;; conditional expand

       ;; brute force approach A

       :brute-force-simple {
                            :fn       (partial _simple-brute-force
                                               is-scraped?
                                               mark-scraped!
                                               :scrape-el)
                            :expands? true
                            :collect? true
                            }

       ;; brute force approach B

       :find-els           {:fn (fn [selector]
                                  (filter (fn [el] (not (is-scraped? el))) (woof-dom/q* selector))
                                  #_(take 10 (filter (fn [el] (not (is-scraped? el))) (woof-dom/q* selector)))
                                  )
                            }

        ;;

       :brute-1            {:fn       (partial _expander!
                                               (fn [] {})
                                               item-expand!)
                            :collect? true
                            :expands? true}

       :brute-recurring    {:fn       (partial _expander!
                                               recurring-scrape-expand!
                                               item-expand!)
                            :collect? true
                            :expands? true}
       }
      )
    )
  )

