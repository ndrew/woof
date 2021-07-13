(ns woof.client.browser.yt.nu-wf-ui
  (:require
    [cljs.core.async :as async :refer [go go-loop]]

    [clojure.string :as str]

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

    [woof.wfs.alpha :as alpha]
    [woof.wfs.evt-loop :as evt-loop]

    [woof.client.ws :as ws]
    [woof.core.protocols :as protocols]
    [woof.data :as d]
    [woof.utils :as u])
  )


(rum/defcs <details-ui> < rum/static
  [local *state STATE]

  [:div.scrape.flex
   (if-let [results (:RESULTS STATE)]
     [:div
      [:header "RESULTS" (str "(" (count results) ")" )]
      (d/pretty!
        (reduce (fn [a [k v]]
                  (assoc a k (count (:videos v)))
                  )  (sorted-map) results))
      ])

   (if-let [wip (:SCRAPE/WIP STATE)]
     [:div
      [:header "WIP"]
      [:pre.html (d/pretty! wip)]])

   (if-let [ready (:SCRAPE/READY STATE)]
     [:div
      [:header "SCRAPED"]
      [:pre.html (d/pretty! ready)]])

   (if-let [err (:SCRAPE/ERROR STATE)]
     [:div
      [:header "ERROR"]
      [:pre.html (d/pretty! err)]])

   ]
  )


(rum/defc <scraping-ui> < rum/static
  [*state STATE]

  [:div.woof-scraper-control

   [:div.woof_details
    (<details-ui> *state STATE)]

   ;; api
   (when (seq (:api STATE))
     (ui/menubar "API" (:api STATE)
                 :class "woof_api") )




   #_(let [upd (get STATE :upd 0)]
     [:span (pr-str upd)])

   #_(if-let [F (:SCRAPE-FN STATE)]
     (ui/btn "SCRAPE!!!" (fn []
                           (F (:SCRAPE-SELECTOR STATE))
                           ))
     )

   ;; clean the dom from parsed elements
   #_(if-let [upds (:upd-class STATE)]
     (let [t (u/now)]
       [:div
        (ui/menubar ""
                    (map
                      (fn [_t]
                        [(str _t) (fn []
                                    (let [$els (woof-dom/q* (str ".ttt-" _t) )]
                                      (.log js/console $els)
                                      (doseq [$el $els]
                                        (woof-dom/html! $el ""))
                                      (swap! *state update :upd-class disj _t))
                                    )]
                        )
                      (sort upds)
                      )
                    )
        ]
       )
     )
   ])



(def <rum-ui> (rum-wf/gen-rum-ui <scraping-ui>))

(defn _ui-process-fn__extract-results [*WF-UI result]

  (let [results* (select-keys result
                              (filter #(str/starts-with? (str %) ":/SCRAPE__") (keys result)))
        *errors (volatile! {})
        ready-results (reduce
                        (fn [a [k v]]
                          (if-not (u/channel? v)
                            (if (:error v)
                              (do
                                (vswap! *errors assoc k v)
                                a
                                )
                              (assoc a (:d v) (count (:videos v)))
                              )

                            a))
                        {} results*
                        )
        ]

    ;;(.warn js/console ready-results)
    (swap! *WF-UI assoc :SCRAPE/READY ready-results)
    (swap! *WF-UI assoc :SCRAPE/ERROR @*errors)
    )
  )



;;
;; UI WF
(defn ui-sub-wf [page-type *WF-UI]
  (assoc
    (rum-wf/ui-impl! *WF-UI <rum-ui>)
    :steps [(fn [params]
              {
               :CSS/minimal-styles    [:css-file "http://localhost:9500/css/r.css"]
               :CSS/playground-styles [:css-file "http://localhost:9500/css/playground.css"]
               })]
    :opts [
			    {:op-handlers-map {:process (fn [result]
                                           ;; just take the results by key prefix
                                           (_ui-process-fn__extract-results *WF-UI result)

                                           result
                                           )}}
    ]
    )
  )
