(ns ^:figwheel-hooks woof.playground.v1.ui
  (:require
    [rum.core :as rum]

    ;; client core
    [woof.base :as base]
    [woof.ui :as ui]
    [woof.u :as u]
    [woof.data :as d]
    [woof.utils :as utils]

    [cljs.core.async :as async]

    [woof.playground.v1.utils :refer [dstr kstr vstr]]

    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


(rum/defcs <debug> < rum/reactive (rum/local true ::show?)

  [{show? ::show?} data]
  (let [h (fn [] (swap! show? not))]
    (if @show?
      [:pre.debug
       ;(ui/btn "..." h) "\n"
       (dstr (into (sorted-map) data))]
      (ui/btn "..." h)
      )
    )
  )
