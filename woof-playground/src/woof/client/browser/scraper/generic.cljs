(ns woof.client.browser.scraper.generic
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
    [woof.wfs.watcher :as watcher]

    [woof.client.ws :as ws]
    [woof.utils :as u]
    [woof.client.browser.scraper.session :as ss]
    [woof.client.browser.scraper.scraping-ui :as sui]

    [cljs.core.async :refer [go] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]
    [clojure.string :as str]))



(defn wf! [*wf-state meta-info]

  (let [dummy 123]
    {
     :init    [
               (fn [params]

                 ;; remove previosly added styles
                 (woof-dom/remove-added-css)

                 ;; remove classes for previously added elements
                 (classes/addRemove
                   (.-body js/document) "woof-el" "")

                 {}
                 )]

     :ctx     [(fn [params]
                 {
                  :rnd-scroll {:fn (fn [_]
                                     (rand-nth [1 2 3]))}

                  }
                 )
               ]
     :steps   [(fn [params]
                 {
                  ::hello [:prn (u/now)]
                  }
                 )]
     :otps    []

     :api     (array-map

                "scroll" (fn []
                           (let [params (get @*wf-state :WF/params {})
                                 evt-loop (evt-loop/&evt-loop params)]
                             (async/put! evt-loop {
                                                   (base/rand-sid) [:scroll 1]
                                                   })
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
  )