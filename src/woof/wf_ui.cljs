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


;; todo: implement a state machine for ui state


(defonce status-classes-map {
                              :woof.app/not-started ""
                              :woof.app/done        "done"
                              :woof.app/running     "pending"
                              :woof.app/stopped     "error"
                              :woof.app/error       "error"
                              })

(defonce status-caption-map {
                              :woof.app/not-started "…"
                              :woof.app/done        "done!"
                              :woof.app/running     "running"
                              :woof.app/stopped     "stopped!"
                              :woof.app/error       "error!"
                              })


(rum/defc <wf-status-ui>  < rum/static
  [status]

  [:span.tag
   {:class (get status-classes-map status "")}
   (get status-caption-map status "")])
