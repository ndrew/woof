(ns ^:figwheel-hooks woof.client.playground.ui.internal
  (:require
    [rum.core :as rum]

    [woof.playground.v1.ui :as ui]
    [goog.net.XhrIo :as xhr]
    )
  (:import [goog.net.XhrIo ResponseType])

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



(defn GET [url handler]
  (xhr/send url (fn [event]
                  (let [response (.-target event)]
                       (handler (.getResponseText response)))
                  )))


(rum/defc <menu> < rum/reactive
  [*SETTINGS]

  [:div.woof-header {:style {:float "right"}}

   [:img {:src "/favicon.png"
          :style {:height "1rem"
                  :float "right"}
          :on-click (fn [e] (swap! *SETTINGS update-in [:show-menu?] not))
          }]
   (when (:show-menu? @*SETTINGS)
     [:div.woof-menu
      [:code "last ping: " (pr-str (:ping @*SETTINGS))]
      (ui/menubar ""
                  [["ping" (fn []
                             (GET "/ping" (fn [response]
                                            ;; todo: parse ping properly
                                            (swap! *SETTINGS assoc :ping response)))
                             )]
                   #_["boo" (fn []

                            )]
                   ])
      ]
     )
   ])

