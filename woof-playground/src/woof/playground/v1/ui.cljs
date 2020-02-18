(ns ^:figwheel-hooks woof.playground.v1.ui
  (:require
    [rum.core :as rum]

    [woof.playground.v1.utils :refer [dstr kstr vstr]]
    )
  )



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
         (if-not (= "" menu-header)
           [:.header menu-header])
         ]
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

;; generic wf ui



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


(rum/defc <tag>  < rum/static
  [class text]

  [:span.tag
   {:class class}  text])


(rum/defc <wf-status-ui>  < rum/static
  [status]

  (<tag> (get status-classes-map status "")
         (get status-caption-map status "")))


(rum/defc <wf-menu-ui> < rum/reactive
  [header status all-actions]
  [:div.main-menu
   [:span "  " (<wf-status-ui> status)]
   (let [actions (get all-actions status [])]
     (menubar header actions))
   ])


;;

(rum/defcs <debug> < rum/reactive (rum/local true ::show?)

  [{show? ::show?} data]
  (let [h (fn [] (swap! show? not))]
    (if @show?
      [:pre.debug
       (dstr (into (sorted-map) data))]
      (btn "..." h)
      )
    )
  )
