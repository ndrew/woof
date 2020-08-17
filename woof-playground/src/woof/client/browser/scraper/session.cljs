(ns woof.client.browser.scraper.session
  (:require
    [clojure.string :as str]

    [cljs.core.async :as async]

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
   ])


(defn init-scraping-msg []
  [:scraping/session
   {
    :host (.. js/window -location -host)
    :url (str (.-location js/window))
    }])


(defn _get-summary-msg-fn [summary-chan params msg-envelope]
  (let [{ws-id   :ws-id
         [t msg] :msg} msg-envelope]

    (cond
      (= :scraping/session t)
      (let [summary (get msg :summary {})]
        ; (.warn js/console "GOT" msg)

        ;; propagate summary further via separate channel
        (async/put! summary-chan summary)

        ;; other way is via injecting a specific key-step via event loop
        ;; for now we don't use evt-chan
        #_(let [evt-chan (evt-loop/&evt-loop params)]
            (async/put! evt-chan
                        {
                         ;; :ws/SUMMARY [:identity summary]
                         }))
        )
      :else (do
              (.warn js/console "unknow message " msg-envelope)
              )
      )))