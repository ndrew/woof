(ns woof.client.browser.rieltor.ui
  (:require

    [goog.dom :as dom]
    [goog.dom.classes :as classes]
    [goog.dom.dataset :as dataset]

    [cljs.core.async :refer [go go-loop] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]
    [clojure.string :as str]

    [woof.base :as base]
    [woof.data :as d]

    [woof.client.dom :as wdom]
    [woof.client.dbg :as dbg :refer [__log]]
    [woof.client.ws :as ws]

    [woof.utils :as u]

    [woof.wfs.evt-loop :as evt-loop]
    [woof.wfs.watcher :as watcher]

    [cljs-time.core :as time]
    [cljs-time.format :as time-fmt]

    [woof.client.dom :as woof-dom]
    [rum.core :as rum]
    [woof.client.playground.ui :as ui]
    [woof.client.playground.ui :as pg-ui]


    [woof.client.browser.scraper.scraping-ui :as s-ui]

    [woof.client.browser.scraper.rum-ui :as rum-wf]

    )
  )

;; ui for scraping workflow

(rum/defc <scraping-ui> < rum/static
  [*state STATE]

  [:div.woof-scraper-control

   (when (seq (:api STATE))
     (ui/menubar "API" (:api STATE) :class "woof_api"))

   (str "ids: (" (count (get STATE :ids #{})) ")")


   ;;
   (when-let [data (:scraped STATE)]
     (pg-ui/<transform-list>
       s-ui/<listing>
       data
       ;; filters
       {} ;(group-by :ID @*asserts)
       :id-fn :id
       ;:copy-fn #(dissoc % :test)
       ;:sort-fn (fn [a b] (compare (:code a) (:code b)))
       :filter-map {
                    ;"test-street" (partial pg-ui/_marker-class-filter "test-street")
                    ;"drv-street" (partial pg-ui/_marker-class-filter "drv-street")
                    ;"non drv-street" (partial pg-ui/_marker-except-class-filter "drv-street")
                    ;"no-house" (partial pg-ui/_marker-class-filter "no-house")
                    }
       )
     )

   ;[:hr]
   #_[:pre
    (d/pretty! (-> STATE
                   (dissoc :api)
                   (dissoc :scraped)
                   ))
    ]

   ])



(def <rum-ui> (rum-wf/gen-rum-ui <scraping-ui>))


;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;


(defn ui-sub-wf [*WF-UI API]
  (assoc
    ;; cfg: debounce interval
    (rum-wf/ui-impl! *WF-UI <rum-ui>)
    :steps
    [(fn [params]
       {
        ; :CSS/test-page-styles [:css-file "http://localhost:9500/css/t.css"]
        :CSS/scraper-styles   [:css-file "http://localhost:9500/css/r.css"]
        })]
    )
  )
