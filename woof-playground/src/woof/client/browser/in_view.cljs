(ns woof.client.browser.in-view
  (:require

    [woof.base :as base]
    [woof.client.dom :as woof-dom]
    [woof.client.dbg :as dbg :refer [__log]]
    [woof.data :as d]
    [woof.utils :as u]

    [goog.dom :as dom]
    [goog.dom.classes :as classes]

    [goog.dom.dataset :as dataset]
    ;; common wf

    [woof.wfs.evt-loop :as evt-loop]

    [woof.client.ws :as ws]
    [woof.client.browser.scraper.session :as ss]
    [woof.client.browser.scraper.scraping-ui :as sui]

    [cljs.core.async :refer [go] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]
    ))




(defn in-view-wf! [*wf-state meta-info]

  {
   ;:init []
   :ctx [(fn [params]
           {
            :load-in-view {
                           :fn (fn [url]

                                 (if-not (. js/window -inView)
                                   (let [chan-factory (base/&chan-factory params)
                                         ch (base/make-chan chan-factory (base/rand-sid))]

                                     (woof-dom/add-script url (fn []
                                                                (async/put! ch {:loaded url})
                                                                ))
                                     ch
                                     )
                                   :loaded
                                   )
                                 )
                           }
            :scroll {
                     :fn (fn [dy]
                           (.scrollBy js/window 0 (.-innerHeight js/window))
                           )
                     }

            :scrape {
                     :fn (fn [el]
                           (.warn js/console (woof-dom/dataset el))

                           (classes/swap el "parsed" "parsed-twice")

                           (classes/add el "parsed")






                           )
                     }

            :in-view {:fn (fn [selector]
                            (let [evt-loop (evt-loop/&evt-loop params)]

                              (->
                                (js/inView selector)

                                (.on "enter" (fn [el]


                                               (if-let [id (dataset/get el "woof_id")]
                                                 (do
                                                   ;; (.log js/console "skipping")
                                                   )
                                                 (do
                                                   (dataset/set el "woof_id" (base/rand-sid))
                                                   (async/put! evt-loop {
                                                                         (base/rand-sid) [:scrape el]
                                                                         })
                                                   )
                                                 )


                                               ) )
                                )
                              )
                            )
                      }
            }
           )

         ]
   :steps [(fn [params]
             {
              :JS/load-in-view [:load-in-view "http://localhost:9500/scraper/in-view.min.js"]

              :JS/_scrape-selector [:v ".foo"]
              :JS/scrape-selector [:wait-rest [:JS/_scrape-selector :JS/load-in-view]]

              :JS/register [:in-view :JS/scrape-selector]
              })]

   :api (array-map
          "scroll" (fn []
                     (let [params (get @*wf-state :WF/params {})
                           evt-loop (evt-loop/&evt-loop params)]
                       (async/put! evt-loop {
                                             (base/rand-sid) [:scroll 1]
                                             })
                       )

                       )

          "request broad cast" (fn []
                                 #_(let [params (get @*wf-state :WF/params {})
                                       evt-loop (evt-loop/&evt-loop params)

                                       MSG (base/rand-sid)
                                       ]

                                   (async/put! evt-loop {
                                                         MSG [:v (ss/ask-for-update-msg)]
                                                         (base/rand-sid) [:ws-send! [:ws/socket MSG]]

                                                         })

                                   ;; (.log js/console @*wf-state)

                                   )
                                 )

          )

   :on-stop (fn [state]

              (__log "ON STOP")
              (.log js/console state)

              ;; can return channel
              )
   }

  )