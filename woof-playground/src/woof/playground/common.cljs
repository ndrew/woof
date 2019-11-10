(ns ^:figwheel-hooks woof.playground.common
  (:require
    [cljs.reader]
    ;; core async
    [cljs.core.async :as async]

    [goog.string :as gstring]
    [goog.string.format]

    [rum.core :as rum]

    ;[woof.u :as u]
    [woof.utils :as u]
    [woof.graph :as g]

    [woof.playground.old.app-data :as app-model]
    [woof.playground.old.wf-runner :as runner]

    [woof.test-data :as test-data]
    [woof.data :as d])

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


(defn default-init!  ;; ;; todo: re-implement as subscription
  "initializes ui state"
  [*UI-STATE mount-fn]
   (add-watch *UI-STATE :woof-main
              (fn [key atom old-state new-state]
                (mount-fn)))

   (when-not (::initialized @*UI-STATE)
     (swap! *UI-STATE merge
            {::initialized true})))


(defn default-reload! [*UI-STATE]
  (swap! *UI-STATE merge {
                          ::initialized false
                          }))
