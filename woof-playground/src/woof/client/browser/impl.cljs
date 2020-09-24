(ns woof.client.browser.impl
  (:require
    [woof.base :as base]
    [woof.data :as d]
    [woof.utils :as u]

    [woof.client.browser.scraper.test-wf :as test-scraper-wf]
    ))


(defn choose-workflow [url]
  (cond
    ;; the example of workflow that scrapes data from web page and stores them in the scraping session

    (clojure.string/starts-with? url "http://localhost:9500/scraper")  test-scraper-wf/scrapping-test-wf!

    )
  )