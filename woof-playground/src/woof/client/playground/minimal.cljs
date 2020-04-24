(ns ^:figwheel-hooks woof.client.playground.minimal
  (:require
    [rum.core :as rum]

    ;; common workflow stuff and ui
    [woof.playground.common :as cmn]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; template for rum playground api


(defonce *TREE (atom {}))


;; ui

(def init! (partial cmn/default-init! *TREE))
(def reload! (partial cmn/default-reload! *TREE))



(rum/defc <your-app-ui> < rum/reactive [*TREE]

  [:pre
   (pr-str @*TREE)
   ]

  )


(def <app> #(<your-app-ui> *TREE))
