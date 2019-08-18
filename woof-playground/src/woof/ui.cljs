(ns ^:figwheel-hooks woof.ui
  (:require
    [rum.core :as rum]

    [woof.u :as u]
    [woof.data :as d]

    ;; core async
    [cljs.core.async :as async]
    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


(rum/defc btn < rum/static [label f]
  [:button {:on-click (fn[e]
                        (f))} label]

  )

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

