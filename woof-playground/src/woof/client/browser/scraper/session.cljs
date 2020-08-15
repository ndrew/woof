(ns woof.client.browser.scraper.session
  (:require
    [clojure.string :as str]
    [woof.utils :as u]
    ))

;; scraping session impl

;; scraping session is a generic way to store and process parsed listings from scraping worklfows


(defn scraping-data-msg [data summary]
  [:scraping/data
   {
    :data    data
    :summary summary

    :host    (.. js/window -location -host)
    :url     (str (.-location js/window))
    }
   ]
  )

(defn init-scraping-msg []
  [:scraping/session
   {
    :host (.. js/window -location -host)
    :url (str (.-location js/window))
    }])

