(ns woof.client.browser.rieltor.parser
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

    ;; todo:
    [woof.client.browser.scraper.scraping-ui :as sui]

    [woof.client.browser.rieltor.ui :as wf-ui]
    [rum.core :as rum]
    ))



(defn safe-href [el selector]
  (if-let [sub-el (wdom/q el selector)]
    (wdom/attr sub-el "href")
    (do
      (.log js/console "can't find element for selector: " selector "parent-el" el)
      (classes/add el "parsed-error")
      ""
      )
    )
  )


(defn safe-txt [el selector]
  (if-let [sub-el (wdom/q el selector)]

    ;; don't trim for now
    (wdom/txt sub-el)
    (do
      (.log js/console "can't find element for selector: " selector "parent-el" el)
      (classes/add el "parsed-error")
      ""
      )
    )
  )


(defn mark![el parse-id]
  (when el
    (classes/set el "DDD")
    (dataset/set el "parseId" parse-id)
    ))
