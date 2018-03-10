(ns woof.wf-ui
  (:require
    [cljs.core.async :as async]
    [rum.core :as rum]


    [woof.data :as d]
    [woof.wf :as wf]
    [woof.utils :as u]

    [clojure.data :as cd])
)


(defonce UI-UPDATE-RATE 50) ; ms

