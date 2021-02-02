(ns woof.client.browser.scraper.listings
  (:require
    ;; core
    [cljs.core.async :as async :refer [go go-loop]]
    [clojure.string :as str]

    ;; woof core
    [woof.base :as base :refer [rand-sid sid
                                &chan-factory make-chan own-chan]]
    [woof.data :as d]
    [woof.utils :as u]

    ;; client utils
    [woof.client.dom :as woof-dom :refer [q q* txt dataset]]

    ;; wf helpers -
    [woof.client.ws :as ws]

    ; helpers from base
    [woof.wfs.alpha :as alpha]
    [woof.wfs.evt-loop :as evt-loop]

    ;; ui
    [goog.dom.classes :as classes]
    [goog.dom :as dom]

    [rum.core :as rum]


    ;; riel
    [woof.client.browser.rieltor.parser :as riel-parser]

    ;; domik
    [woof.client.browser.domik.parser :as domik-parser]

    ;; blago
    [woof.client.browser.blago.listings :as blago-parser]
    ))

;;
;; actual scraping


(defn- riel? [url]
  (or (str/starts-with? url "http://localhost:9500/r.html")
      (str/starts-with? url "https://rieltor.ua/")))

(defn- domik? [url]
  (or (str/starts-with? url "http://localhost:9500/d.html")
      (str/starts-with? url "http://domik.ua/")))

(defn- blago? [url]
  (or (str/starts-with? url "http://localhost:9500/b.html")
      (str/starts-with? url "https://blagovist.ua/")))


(defn get-source [url]
  (cond
    (riel? url) :riel
    (domik? url) :domik
    (blago? url) :blago))


(defn get-container-selector [src]
  (cond
    (= :riel src) ".index-list-container"
    (= :domik src) "#divListObjects"
    (= :blago src) "#map_fix"
    ))


(defn get-scrape-selector [src]
  ;; :not(.WOOF-WIP) is very important
  (let [exclude-processed ":not(.WOOF-WIP):not(.WOOF-ERROR)"]
    (cond
      (= :riel src)  (str ".index-list-container > .catalog-item" exclude-processed)
      (= :domik src) (str "#divListObjects .objava" exclude-processed)
      (= :blago src) (str "#map_fix .search-item" exclude-processed)
      )
    )
  )



(defn get-parse-fn [src]
  (cond
    (= :riel src)  riel-parser/parse-listing
    (= :domik src) domik-parser/parse-listing
    (= :blago src) blago-parser/parse-listing))