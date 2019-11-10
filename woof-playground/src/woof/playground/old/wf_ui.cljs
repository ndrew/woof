(ns woof.playground.old.wf-ui
  (:require
    [rum.core :as rum]

    [woof.data :as d]
    [woof.utils :as u]
    [woof.playground.v1.ui :as ui])
  )


(defonce UI-UPDATE-RATE 50) ; ms


;; todo: implement a state machine for ui state


(defonce status-classes-map {
                             :not-started ""
                             :done        "done"
                             :running     "pending"
                             :stopped     "error"
                             :error       "error"
                             })

(defonce status-caption-map {
                             :not-started "…"
                             :done        "done!"
                             :running     "running"
                             :stopped     "stopped!"
                             :error       "error!"
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
        status (if ch? :running
                       (get {nil :not-started, :nil :error} r :done))]
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


