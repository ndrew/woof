(ns woof.playground.old.ui
  (:require

    [goog.string :as gstring]
    [goog.string.format]

    [rum.core :as rum]

    ; [woof.data.core :as c]
    [woof.data :as d])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]
    [woof.utils-macros :refer [put!?]]))



;;;;;;;;;
;;
;; EDN EDITOR


(rum/defcs data-editor   <   rum/reactive
                             (rum/local false ::editing)
                             (rum/local nil   ::value)
                             "primitive edn editor
                             - stops editing via esc
                             - saves changes via enter
                             "
  [{*EDITING?   ::editing
    *NEW-VALUE  ::value} change-fn d]

  (let [;is-data (satisfies? c/DATA d)
        change (fn [new-value] ; editor
                 (change-fn new-value)
                 (reset! *EDITING? false))]

    (if @*EDITING?
      [:div.edn.data.edit [:input {:on-change    (fn [s] (reset! *NEW-VALUE (.. s -target -value)))
                                   :on-blur      (fn[e] (change (d/to-primitive @*NEW-VALUE)))
                                   :on-key-down (fn[e]
                                                  (if (== 27 (.-keyCode e))
                                                    (reset! *EDITING? false)))
                                   :on-key-press (fn[e]
                                                   (if (== 13 (.-charCode e)) (change (d/to-primitive @*NEW-VALUE))))
                                   :value @*NEW-VALUE}]]

      ;; data
      [:div.edn.data
       {:on-double-click (fn[e]
                           (swap! *EDITING? not)
                           (reset! *NEW-VALUE d))}
       (d/pretty d)])))




;; todo: customizible menu input (next to a menu button)


;;;
;;

(rum/defc <text-area> < rum/reactive
  [value change-fn]
  [:textarea { :type "text"
              :value value
              :on-change (fn [e]
                           (change-fn (.. e -currentTarget -value))
                           ) }])
