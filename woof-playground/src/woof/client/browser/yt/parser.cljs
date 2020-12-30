(ns woof.client.browser.yt.parser
  (:require
    [goog.dom :as dom]

    [goog.dom.classes :as classes]
    [goog.dom.dataset :as dataset]

    [clojure.string :as str]

    [woof.base :as base]
    [woof.data :as d]

    [woof.client.dom :as wdom]
    [woof.client.dbg :as dbg :refer [__log]]

    [woof.utils :as u]

    [cljs-time.core :as time]
    [cljs-time.format :as time-fmt]

    ))



(defn history-selector []
  "#contents .ytd-section-list-renderer"
  )



(defn _history-scrape [el]

  (.log js/console "parsing" el)
  {
   :kinda :parsed
   }
  )



(defn history-scrape [el]
  (try
    (let [result (_history-scrape el)]

      ;; todo: ...
      ; (swap! *SCRAPED-DATA conj result)
      ; (swap! *WF-scrape-data conj result)

      result)
    (catch js/Error e
      (do
        (classes/add el "parsed-error")
        (.error js/console e)

        ;; (swap! *FAILED conj (woof-dom/el-map el))
        )
      )
    )
  )


(defn is-scraped? [el]
  (dataset/has el "woof_id"))


(defn mark-scraped! [el]
  (let [sid (base/rand-sid)
        ALLOW-DOUBLE-PARSE true
        ]
    ;; mark element as processed
    (if-not ALLOW-DOUBLE-PARSE
      (dataset/set el "woof_id" sid))
    ))
