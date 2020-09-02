(ns woof.client.browser.scraper.session
  (:require
    [cljs.core.async :as async]

    [woof.utils :as u]
    ))

;; scraping session impl

;; scraping session is a generic way to store and process parsed listings from scraping workflows


(defn scraping-data-msg [data summary]
  [:scraping/data
   {
    :data    data
    :summary summary

    :host    (.. js/window -location -host)
    :url     (str (.-location js/window))
    }
   ])


(defn start-scraping-session-msg []
  [:scraping-client/connect
   {
    :host (.. js/window -location -host)
    :url (str (.-location js/window))
    }])


(defn ask-for-update-msg []
  [:scraping-client/broadcast
   {
    :host (.. js/window -location -host)
    :url (str (.-location js/window))
    }
   ]
  )


(defn _get-summary-msg-fn
  ""
  [summary-chan params msg-envelope]
  (let [{ws-id   :ws-id
         [t msg] :msg} msg-envelope]

    (cond
      ;; server sends current scraping session summary
      (= :scraping/summary t)
      (let [summary (get msg :summary {})]

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

      (= :broadcast t)
      (do
        (.log js/console "GOT THE BROADCAST MESSAGE" msg)
        )

      :else (do
              (.warn js/console "unknown message " msg-envelope)
              )
      )))