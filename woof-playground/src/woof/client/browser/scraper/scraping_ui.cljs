(ns woof.client.browser.scraper.scraping-ui
  (:require
    [goog.object]
    [goog.dom :as dom]
    [goog.object]
    [goog.dom.classes :as classes]

    [cljs.core.async :as async]
    [woof.base :as base]

    [clojure.string :as str]
    [woof.data :as d]
    [woof.utils :as u]
    [woof.client.dom :as woof-dom]
    [woof.client.ws :as ws]
    [woof.client.browser.scraper.session :as ss]

    [woof.wfs.evt-loop :as evt-loop]
    ))




(defn scraping-ui-impl! []
  ;; todo: pass configuration for urls
  (let [clear-session-btn-el (dom/createDom "button" "" "clear session")
        get-session-btn-el   (dom/createDom "button" "" "get session")
        panel (dom/createDom "div" "panel")
        ]

    (dom/appendChild panel get-session-btn-el)
    (dom/appendChild panel clear-session-btn-el)


    (woof-dom/on-click get-session-btn-el
              (fn [e]
                (ws/GET "http://localhost:8081/scraping-session"
                        (fn [raw-edn]
                          (.log js/console raw-edn)))))

    (woof-dom/on-click clear-session-btn-el
              (fn [e]
                (ws/GET "http://localhost:8081/clear-scraping-session"
                        (fn [raw-edn]
                          (.log js/console raw-edn)))))

    (woof-dom/ui-add-el! panel)

    )
  )
