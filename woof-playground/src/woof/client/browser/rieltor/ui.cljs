(ns woof.client.browser.rieltor.ui
  (:require

    [goog.dom :as dom]
    [goog.dom.classes :as classes]
    [goog.dom.dataset :as dataset]

    [cljs.core.async :refer [go go-loop] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]
    [clojure.string :as str]

    [woof.base :as base]
    [woof.data :as d]

    [woof.client.dom :as wdom]
    [woof.client.dbg :as dbg :refer [__log]]
    [woof.client.ws :as ws]

    [woof.utils :as u]

    [woof.wfs.evt-loop :as evt-loop]
    [woof.wfs.watcher :as watcher]

    [cljs-time.core :as time]
    [cljs-time.format :as time-fmt]

    [woof.client.dom :as woof-dom]
    [rum.core :as rum]
    [woof.client.playground.ui :as ui]))

;; ui for scraping workflow

(rum/defc <scraping-ui> < rum/static
  [*state STATE]

  [:div.woof-scraper-control

   (when (seq (:api STATE))
     (ui/menubar "API" (:api STATE)))


   [:header "some ui here. Unfortunately, evt handlers should be set separately"]
   [:button {:on-click (fn [e]
                         (.log js/console e)
                         (swap! *state assoc :now (u/now))
                         )} "foo"]

   [:div.resize-test
    "foo"
    ]

   [:hr]
   (pr-str STATE)
   ])


(defn <rum-ui>
  ([]
   ;; init UI
   (let [$panel (dom/createDom "div" "panel woof-scrape-panel ui-container")]
     (woof-dom/ui-add-el! $panel))
   )
  ([*state STATE]
   (let [container-el (woof-dom/q ".ui-container")]
     ;; react UI
     (rum/mount (<scraping-ui> *state STATE)
                container-el)

     )))
