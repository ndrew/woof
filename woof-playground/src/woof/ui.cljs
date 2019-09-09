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



;; react-based ui for woof playground


;;
;; button
(rum/defc btn < rum/static [label f]
  [:button {:on-click (fn[e] (f))} label])


;;
;; menu

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


(rum/defc menubar     <   rum/static     { :key-fn (fn [header items] (str header (count items)))}
                          "generic menubar component. has header and buttons"
  [menu-header menu-items]

  (into [:span.menubar
         [:.header menu-header]]
        (map (fn [[label action]]
               (if (and (nil? label) (nil? action))
                 [:.separator]
                 (menu-item label action)))
             menu-items)))


;;
;; select

;; immutable ds
(defn select-data-source
  ([items]
   (select-data-source items 0))
  ([items selected-idx]
   {
    :data items
    :ui   {
           :selected-idx selected-idx
           }
    :api  {
           ;; 'public' api
           :select-i (fn [*select-ds new-idx]
                       (swap! *select-ds assoc-in [:ui :selected-idx] new-idx))
           ;; todo: get value?
           }
    }
   )
  )


(rum/defc <select> < rum/reactive [*select-ds]
  (let [ds (rum/react *select-ds)
        data (get ds :data [])

        select-i! (partial (get-in ds [:api :select-i]) *select-ds)
        selected-idx (get-in ds [:ui :selected-idx])
        ]
    (into [:select
           {:on-change (fn [e]
                         (let [new-idx (.. e -target -options -selectedIndex )]
                           (select-i! new-idx)))
            ;; checked control
            :value (:v (get data selected-idx))
            }
           ]
          (map-indexed (fn [i a]
                         ;;
                         [:option {:value (:v a)} (:text a)]) data)
          )
    )
  )
