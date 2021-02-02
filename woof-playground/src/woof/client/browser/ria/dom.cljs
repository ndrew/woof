(ns woof.client.browser.ria.dom
  (:require
    [goog.dom :as dom]
    [goog.dom.classes :as classes]
    [goog.dom.dataset :as dataset]

    [cljs.core.async :refer [go go-loop] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]

    [cljs-time.core :as time]
    [cljs-time.format :as time-fmt]

    [clojure.string :as str]

    [woof.base :as base :refer [&chan-factory make-chan]]
    [woof.data :as d]

    [woof.client.dom :as woof-dom :refer [q q* attr txt txt-only mark!]]
    [woof.utils :as u]))


(defn parse-listing [el]

  ;; search-realty-17637657

  (let [_id (.-id el )
        [_ id] (re-matches #"search-realty-(\d+)" _id)
        ]

    {
     :id id
     }
    )

  )