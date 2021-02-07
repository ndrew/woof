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
    ;; ria
    [woof.client.browser.ria.dom :as ria-parser]
    ;; flatfy
    [woof.client.browser.lun.flatfy :as flatfy]
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

(defn- ria? [url]
  (or (str/starts-with? url "http://localhost:9500/ria.html")
      (str/starts-with? url "https://dom.ria.com/"))
  )

(defn- flatfy? [url]
  (or (str/starts-with? url "http://localhost:9500/flat.html")
      (str/starts-with? url "https://flatfy.ua/"))
  )

(defn get-source
  ([]
   (get-source (.. js/document -location -href)))
  ([url]
  (cond
    (riel? url) :riel
    (domik? url)
    (cond (or
            (str/starts-with? url "http://localhost:9500/d.html")
            (str/starts-with? url "http://domik.ua/kompanii/kiev/rieltorskie-kompanii")) :domik-agencies
          :else :domik)
    (blago? url) :blago
    (ria? url) :ria
    (flatfy? url) :flatfy
    )
   ))


(defn get-container-selector [src]
  (cond
    (= :riel src) ".index-list-container"
    (= :domik src) "#divListObjects"
    (= :domik-agencies src) ".centr_left .white_round_block_content"
    (= :blago src) "#map_fix"
    (= :ria src) "#searchResults"
    (= :flatfy src) ".table-view"
    ))


(defn get-scrape-selector [src]
  ;; :not(.WOOF-WIP) is very important
  (let [exclude-processed ":not(.WOOF-WIP):not(.WOOF-ERROR)"]
    (cond
      (= :riel src)  (str ".index-list-container > .catalog-item" exclude-processed)
      (= :domik src) (str "#divListObjects .objava" exclude-processed)
      (= :domik-agencies src)
                      (str ".centr_left .white_round_block_content .catalog_entry" exclude-processed)
      (= :blago src) (str "#map_fix .search-item" exclude-processed)
      (= :ria src) (str "#searchResults .ticket-clear" exclude-processed)
      (= :flatfy src) (str ".table-view .realty-block__wrapper .realty-preview" exclude-processed)
      )
    )
  )



(defn get-parse-fn [src]
  (cond
    (= :riel src)  riel-parser/parse-listing
    (= :domik src) domik-parser/parse-listing
    (= :domik-agencies src) domik-parser/parse-agency
    (= :blago src) blago-parser/parse-listing
    (= :ria src) ria-parser/parse-listing
    (= :flatfy src) flatfy/parse-listing
    )
  )