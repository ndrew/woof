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

    [woof.client.browser.rieltor.wf :as riel]
    [woof.client.browser.domik.scraper :as domik]

    ))

(defn choose-workflow-meta [url META-INFO]

  (cond
    (clojure.string/starts-with? url "https://web.telegram.org")
    (assoc META-INFO :ui/fixed-side :left)

    :else META-INFO
    )
  )


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

      (= url "http://localhost:9500/example/seq.html") nu-seq-wf/wf!
      (clojure.string/starts-with? url "https://web.telegram.org") nu-seq-wf/wf!

      (clojure.string/starts-with? url "https://www.deezer.com/uk/playlist/") nu-seq-wf/wf!

      ;; scraper examples
      (clojure.string/starts-with? url "http://localhost:9500/example/scroll-inview.html") ex-scroll-wf/in-view-wf!
      (clojure.string/starts-with? url "http://localhost:9500/example/scroll-brute.html")  ex-scroll-wf/brute-wf!


    ;; yt
    (clojure.string/starts-with? url "http://localhost:9500/s/yt/y.html") yt/wf!

    (clojure.string/starts-with? url "http://localhost:9500/s/yt/history.html") yt-nu/wf!

    ;; extract history of youtube views
    (clojure.string/starts-with? url "https://www.youtube.com/feed/history") yt-nu/wf!

    (clojure.string/starts-with? url "https://www.youtube.com/playlist?list=WL") yt/wf!

    ;; kga
    (clojure.string/starts-with? url "https://mkk.kga.gov.ua/map/") kga/wf!

    (clojure.string/starts-with? url "http://localhost:9500/r.html") riel/wf!


    (clojure.string/starts-with? url "https://rieltor.ua/") riel/wf!

    ;; return wf map

    (= url "http://localhost:9500/domik.html") (domik/domik-scraping! url)
    ;(clojure.string/starts-with? url "http://domik.ua/") (domik/domik-scraping! url) ;;riel/wf!


    ;; domik
   ; (= url "http://localhost:9500/domik.html") domik-scraper/parse-listings-steps
   ; (= url "http://localhost:9500/domik_house.html") domik-scraper/parse-house-steps

 ;   (clojure.string/starts-with? url "http://domik.ua/poleznoe/photoalbum/")  domik-scraper/parse-house-steps
 ;   (clojure.string/starts-with? url "http://domik.ua/nedvizhimost/") domik-scraper/parse-listings-steps


    )
  )