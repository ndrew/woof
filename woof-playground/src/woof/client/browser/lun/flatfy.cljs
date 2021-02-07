(ns woof.client.browser.lun.flatfy
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
  (.log js/console el)

  {
   :id (rand-int 10000)
   }

  )