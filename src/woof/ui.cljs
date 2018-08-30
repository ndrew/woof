(ns woof.ui
  (:require
    [cljs.core.async :as async]

    [goog.string :as gstring]
    [goog.string.format]

    [rum.core :as rum]

    [woof.core :as c]
    [woof.data :as d]
    [woof.graph :as g]
    [woof.wf :as wf]
    [woof.utils :as u]
    [woof.test-data :as test-data])
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

  (let [is-data (satisfies? c/DATA d)
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



;;;;;;;;;;;
;;
;; MENU


(rum/defc
  menu-item          <   { :key-fn (fn [label _] (str label))}
  "menu item component"
  [label action-fn]

  [:a.menu-item
   {:href "#" :on-click (fn [e]
                          (action-fn)
                          (.preventDefault e)
                          false)}
   label])


(rum/defc menubar     <   rum/reactive     { :key-fn (fn [header items] (str header (count items)))}
  "generic menubar component. has header and buttons"
  [menu-header menu-items]

  (into [:span.menubar
         [:.header menu-header]]
        (map (fn [[label action]]
               (if (and (nil? label) (nil? action))
                 [:.separator]
                 (menu-item label action)))
             menu-items)))

;; todo: customizible menu input (next to a menu button)
