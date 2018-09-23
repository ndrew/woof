(ns woof.wf-ui
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.utils :as u]
    [woof.ui :as ui]

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



(rum/defc <step-status>   <    rum/static
                               {:key-fn (fn [k _ _] k)}
  [k step r]

  (let [ch? (u/channel? r)
        status (if ch? ::running
                 (get {nil ::not-started, :nil ::error} r ::done))]
    [:div.step
     [:div.result
      (if ch? "…" (pr-str r))]
     [:div.info
      [:span.k (pr-str k)]
      [:span.action (d/pretty step)]

     (<wf-status-ui> status)]]))



(rum/defc <wf-menu-ui> < rum/reactive
  [header status all-actions]
  [:div.main-menu
   [:span "  " (<wf-status-ui> status)]
   (let [actions (get all-actions status [])]
     (ui/menubar header actions))
   ])


