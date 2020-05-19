(ns blog.frontend.util
  (:require
    [goog.object]
    [goog.dom :as gdom]
    [goog.dom.classlist :as gclasslist]

    [cljsjs.commonmark]
    [cljs.core.async :refer [go] :as async]

    [clojure.string :as str]
    [clojure.edn :as edn]

    [goog.net.XhrIo :as xhr]

    [rum.core :as rum]

    [woof.base :as base]
    [woof.utils :as utils]
    [woof.wfs.evt-loop :as evt-loop]
    [woof.wfs.kv :as kv]


    )
  (:import
    [goog.net.XhrIo ResponseType])
  )

(defn GET [url handler]
  (xhr/send url (fn [event]
                  (let [response (.-target event)]
                       (handler (.getResponseText response)))
                  )))

