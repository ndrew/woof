(ns woof.playground.old.wf-ui
  (:require
    [rum.core :as rum]

    [woof.data :as d]
    [woof.utils :as u]
    [woof.playground.v1.ui :as ui])
  )


(defonce UI-UPDATE-RATE 50) ; ms




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

      ;; todo: use status ui
      (pr-str status)
      ; (<wf-status-ui> status)
      ]]))




