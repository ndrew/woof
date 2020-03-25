(ns ^:figwheel-hooks woof.client.playground.ui.internal
  (:require
    [rum.core :as rum]

    [woof.client.playground.ui :as ui]
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


(rum/defc <checkbox> < rum/static
  [change-fn v & {:keys [attrs] :or {attrs {}}}]
  [:input (merge attrs
                 {:type      "checkbox"
                  :checked   v
                  :on-change (fn [e]
                               (change-fn (not v)))

                  })
   ]
  )


(rum/defc <menu> < rum/reactive
  [*SETTINGS]

  [:div.woof-internal {:style {:float "right"}}

   [:img {:src "/favicon.png"
          :style {:height "1rem"
                  :float "right"}
          :on-click (fn [e] (swap! *SETTINGS update-in [:show-menu?] not))
          }]
   (let [settings @*SETTINGS
         {show-menu? :show-menu?
          last-ping :ping
          stop-wf-on-reload? :stop-wf-on-reload?} settings
         ]
     (when show-menu?
       [:div.woof-internal-popup

        (ui/menubar "Internal"
                    [["ping" (fn []
                               (GET "/ping" (fn [response]
                                              ;; todo: parse ping properly
                                              (swap! *SETTINGS assoc :ping response)))
                               )]
                     ;["boo" (fn [])]
                     ;["yoo" (fn [])]
                     ])
        ; [:pre (pr-str @*SETTINGS)]

        [:div
         "stop wf on reload"
         (<checkbox> (fn [v]
                       (swap! *SETTINGS assoc :stop-wf-on-reload? v))

                     stop-wf-on-reload?)
         ]
        [:hr]
        [:div "last ping: " (pr-str last-ping)]

        ]
       )
     )

   ])

