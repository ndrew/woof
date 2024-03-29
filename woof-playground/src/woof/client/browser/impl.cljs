(ns woof.client.browser.impl
  (:require
    [woof.base :as base]
    [woof.data :as d]
    [woof.utils :as u]

    ;; example workflows
    [woof.client.browser.example.ui-wf :as ex-ui-wf]
    [woof.client.browser.example.seq-wf :as ex-seq-wf]

    [woof.client.browser.example.seq-wf-a :as nu-seq-wf]

    [woof.client.browser.example.scroll-wf :as ex-scroll-wf]

    [woof.client.browser.scraper.test-wf :as test-scraper-wf]

    ;; real site scenarios

    [woof.client.browser.yt.wf :as yt]
    [woof.client.browser.yt.nu-wf :as yt-nu]


    [woof.client.browser.kga.scraper :as kga]

    [woof.client.browser.scraper.scrape-wf :as scrape-wf]
    [woof.client.browser.domik.scraper :as domik]

    ))


(defn choose-workflow-meta [url META-INFO]
  (cond
    (clojure.string/starts-with? url "https://web.telegram.org")
    (assoc META-INFO :ui/fixed-side :left)

    :else META-INFO
    )
  )

;;
;; choose which browser wf to run for url
(defn choose-workflow [url]
  (cond

    ;;
    ;; examples

    ;; the example of workflow that scrapes data from web page and stores them in the scraping session
    (clojure.string/starts-with? url "http://localhost:9500/scraper") test-scraper-wf/scrapping-test-wf!

    ;; react like UI updates
    (= url "http://localhost:9500/example/ui.html") ex-ui-wf/wf!

    ;; sequential processing of dom elements
    ;(= url "http://localhost:9500/example/seq.html") ex-seq-wf/wf!

    ;; scraper examples
    (clojure.string/starts-with? url "http://localhost:9500/example/scroll-inview.html") ex-scroll-wf/in-view-wf!
    (clojure.string/starts-with? url "http://localhost:9500/example/scroll-brute.html") ex-scroll-wf/brute-wf!


    ;; yt


    ;; extract history of youtube views
    (clojure.string/starts-with? url "https://www.youtube.com/feed/history") yt-nu/wf!
    (clojure.string/starts-with? url "http://localhost:9500/s/yt/history.html") yt-nu/wf!
    ;; watch later
    (clojure.string/starts-with? url "https://www.youtube.com/playlist?list=WL") yt/wf!
    (clojure.string/starts-with? url "http://localhost:9500/s/yt/y.html") yt/wf!

    ;; tg
    (= url "http://localhost:9500/example/seq.html") nu-seq-wf/wf!
    (clojure.string/starts-with? url "https://web.telegram.org") nu-seq-wf/wf!


    ;; kga
    (clojure.string/starts-with? url "https://mkk.kga.gov.ua/map/") kga/wf!


    ;;
    ;;
    ;;

    ; blago
    (= url "http://localhost:9500/b.html") scrape-wf/wf!
    (clojure.string/starts-with? url "https://blagovist.ua/") scrape-wf/wf!


    ; rieltor.ua
    (clojure.string/starts-with? url "http://localhost:9500/r.html") scrape-wf/wf!
    (clojure.string/starts-with? url "https://rieltor.ua/") scrape-wf/wf!

    ; domik.ua
    (= url "http://localhost:9500/d.html") scrape-wf/wf!
    (clojure.string/starts-with? url "http://domik.ua/") scrape-wf/wf!

    (= url "http://localhost:9500/ria.html") scrape-wf/wf!
    (clojure.string/starts-with? url "https://dom.ria.com/") scrape-wf/wf!

    ;; flatfy
    (= url "http://localhost:9500/flat.html") scrape-wf/wf!
    (clojure.string/starts-with? url "https://flatfy.ua") scrape-wf/wf!

    ;

    ;; old domik scraping
    ;(= url "http://localhost:9500/domik.html") (domik/domik-scraping! url)
    ;(clojure.string/starts-with? url "http://domik.ua/") (domik/domik-scraping! url) ;;riel/wf!


    ;; domik
    ; (= url "http://localhost:9500/domik.html") domik-scraper/parse-listings-steps
    ; (= url "http://localhost:9500/domik_house.html") domik-scraper/parse-house-steps

    ;   (clojure.string/starts-with? url "http://domik.ua/poleznoe/photoalbum/")  domik-scraper/parse-house-steps
    ;   (clojure.string/starts-with? url "http://domik.ua/nedvizhimost/") domik-scraper/parse-listings-steps

    ;;
    (clojure.string/starts-with? url "https://www.deezer.com/uk/playlist/") nu-seq-wf/wf!

    )
  )