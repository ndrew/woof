(ns woof.app
  (:require
    [rum.core :as rum])

  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))


(enable-console-print!)

(defonce *APP-STATE
  (atom {
          :hello :woof
        }))


(rum/defcs app-ui
  < [local *STATE]
  [:div#app
    [:header (str "Hello " (:hello @*STATE))]
   ])


(rum/mount (app-ui *APP-STATE)
           (. js/document (getElementById "app")))


(add-watch *APP-STATE :watcher
  (fn [key atom old-state new-state]
    (rum/mount (app-ui *APP-STATE)
               (. js/document (getElementById "app")))))


(defn on-js-reload []
  #_(.clear js/console)
)






