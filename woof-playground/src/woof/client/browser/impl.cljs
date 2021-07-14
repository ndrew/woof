(ns woof.client.browser.impl
  (:require

    [clojure.string :as str :refer [starts-with?]]
    [woof.base :as base]
    [woof.data :as d]
    [woof.utils :as u]

    [woof.client.dom :as woof-dom]

    ;; example workflows
    [woof.client.browser.example.ui-wf :as ex-ui-wf]
    [woof.client.browser.example.seq-wf :as ex-seq-wf]
    [woof.client.browser.example.seq-wf-a :as nu-seq-wf]
    [woof.client.browser.example.scroll-wf :as ex-scroll-wf]
    [woof.client.browser.scraper.test-wf :as test-scraper-wf]

    ;; real site scenarios

    [woof.client.browser.yt.nu-wf :as yt-nu]

    [woof.client.browser.kga.scraper :as kga]

    [woof.client.browser.scraper.scrape-wf :as scrape-wf]

    [woof.client.browser.scraper.generic :as default-scraper]
    ; [woof.client.browser.domik.scraper :as domik]
    ))

(defn choose-workflow-meta [url META-INFO]

  (cond
    (str/starts-with? url "https://web.telegram.org") (assoc META-INFO :ui/fixed-side :left)

    (str/starts-with? url "http://localhost:9500/s/yt/wl.html") (merge META-INFO {:yt/t :watch-later})
    (str/starts-with? url "https://www.youtube.com/playlist?")  (merge META-INFO {:yt/t :watch-later})

    ;; (str/starts-with? url "http://localhost:9500/s/yt/wl.html")   (merge META-INFO {:yt/t :video})

    (str/starts-with? url "https://www.youtube.com/watch?v=")   (merge META-INFO {:yt/t :video })


    :else META-INFO
    )
  )


;;
;;
(defn choose-workflow [url]
  (cond
		  ;; document.body.classList.add("GENERIC-SCRAPER")
  	 (woof-dom/has-class? (.-body js/document) "GENERIC-SCRAPER") default-scraper/wf!
  	 ;; 
    ;; youtube
    ;; 

    (str/starts-with? url "https://www.youtube.com/playlist") yt-nu/wf!
    (str/starts-with? url "http://localhost:9500/s/yt/wl.html") yt-nu/wf! 
    (str/starts-with? url "http://localhost:9500/s/yt/history.html") yt-nu/wf!
    (str/starts-with? url "https://www.youtube.com/feed/history") yt-nu/wf! 
    (str/starts-with? url "https://www.youtube.com/watch?v=") yt-nu/wf!


    ;;
    ;; examples

    ;; the example of workflow that scrapes data from web page and stores them in the scraping session
    (str/starts-with? url "http://localhost:9500/scraper") test-scraper-wf/scrapping-test-wf!

    ;; react like UI updates
    (= url "http://localhost:9500/example/ui.html") ex-ui-wf/wf!

    ;; sequential processing of dom elements
    ;(= url "http://localhost:9500/example/seq.html") ex-seq-wf/wf!

    (= url "http://localhost:9500/example/seq.html") nu-seq-wf/wf!
    (str/starts-with? url "https://web.telegram.org") nu-seq-wf/wf!

    (str/starts-with? url "https://www.deezer.com/uk/playlist/") nu-seq-wf/wf!

    ;; scraper examples
    (str/starts-with? url "http://localhost:9500/example/scroll-inview.html") ex-scroll-wf/in-view-wf!
    (str/starts-with? url "http://localhost:9500/example/scroll-brute.html") ex-scroll-wf/brute-wf!


    ;; kga
    (str/starts-with? url "https://mkk.kga.gov.ua/map/") kga/wf!

    ;;
    (str/starts-with? url "http://localhost:9500/r.html") scrape-wf/wf!
    (str/starts-with? url "https://rieltor.ua/") scrape-wf/wf!

    ;; return wf map

    ; (= url "http://localhost:9500/domik.html") (domik/domik-scraping! url)
    ;(str/starts-with? url "http://domik.ua/") (domik/domik-scraping! url) ;;riel/wf!

    ;; domik
    ; (= url "http://localhost:9500/domik.html") domik-scraper/parse-listings-steps
    ; (= url "http://localhost:9500/domik_house.html") domik-scraper/parse-house-steps

    ;   (str/starts-with? url "http://domik.ua/poleznoe/photoalbum/")  domik-scraper/parse-house-steps
    ;   (str/starts-with? url "http://domik.ua/nedvizhimost/") domik-scraper/parse-listings-steps


    ;; :else test-scraper-wf/scrapping-test-wf!

    )
  )