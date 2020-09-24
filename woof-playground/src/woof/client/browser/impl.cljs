(ns woof.client.browser.impl
  (:require
    [woof.base :as base]
    [woof.data :as d]
    [woof.utils :as u]

    ;; example workflows
    [woof.client.browser.example.ui-wf :as ex-ui-wf]
    [woof.client.browser.example.seq-wf :as ex-seq-wf]
    [woof.client.browser.scraper.test-wf :as test-scraper-wf]

    ))


(defn choose-workflow [url]
  (cond
    ;; the example of workflow that scrapes data from web page and stores them in the scraping session
    (clojure.string/starts-with? url "http://localhost:9500/scraper")  test-scraper-wf/scrapping-test-wf!

    (= url "http://localhost:9500/example/ui.html")  ex-ui-wf/wf!
    (= url "http://localhost:9500/example/seq.html")  ex-seq-wf/wf!


    )
  )