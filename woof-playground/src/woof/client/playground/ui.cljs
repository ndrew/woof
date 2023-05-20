(ns ^:figwheel-hooks woof.client.playground.ui
  (:require
    [rum.core :as rum]

    [woof.playground.v1.utils :refer [dstr kstr vstr]]
    [clojure.string :as str]
    [woof.client.dom :as woof-dom]
    [clojure.set :as set])
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

  [:span.menu-item
   {:href "#" :on-click (fn [e]
                          (action-fn)
                          (.preventDefault e)
                          false)}
   label])


(rum/defc menubar     <   rum/static     { :key-fn (fn [header items] (str header (count items)))}
                          "generic menubar component. has header and buttons"
  [menu-header menu-items & {:keys [class] :or {class ""}}]

  (into [:span.menubar {:class class}
         (if-not (= "" menu-header)
           [:.header menu-header])
         ]
        (map (fn [[label action]]
               (if (and (nil? label) (nil? action))
                 [:.separator]
                 ;; todo: menu-item handle shortcuts
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
  [header status actions-map ui-items]

   (let [actions (get actions-map status [])]
     [:div.main-menu
      (menubar header actions)

      (if (seq ui-items)
        (menubar "UI" ui-items))

      (<wf-status-ui> status)
      ]
     )
   )


;;

(rum/defcs <debug> < rum/reactive (rum/local true ::show?)

  [{show? ::show?} data]
  (let [h (fn [] (swap! show? not))]
    (if @show?
      [:pre.debug
       (dstr (into (sorted-map) data))
       (btn "..." h)
       ]
      (btn "..." h)
      )
    )
  )


;;;

(defn shorten-bool [b]
  (if b
    "✓" "✕"))


(defonce OPENING-BRACKETS
         {"cljs.core/PersistentTreeSet" "#{"
          "cljs.core/PersistentHashSet" "#{"
          "cljs.core/PersistentHashMap" "{"
          "cljs.core/List"              "["
          "cljs.core/EmptyList"         "["
          "cljs.core/LazySeq"           "("
          "cljs.core/KeySeq"            "("
          "cljs.core/IndexedSeq"        "("
          "cljs.core/PersistentVector"  "["
          "cljs.core/PersistentArrayMap" "{"
          })

(defonce CLOSING-BRACKETS
         {"cljs.core/PersistentTreeSet" "}"
          "cljs.core/PersistentHashSet" "}"
          "cljs.core/PersistentHashMap" "}"
          "cljs.core/List"              "]"
          "cljs.core/EmptyList"         "]"
          "cljs.core/PersistentVector"  "]"
          "cljs.core/KeySeq"            ")"
          "cljs.core/LazySeq"           ")"
          "cljs.core/IndexedSeq"        ")"
          "cljs.core/PersistentArrayMap" "}"
          })



;; todo: make h first
(rum/defc <edn-list> < rum/static
  [edn h]

  (let [t (pr-str (type edn))
        EDN-STR (reduce
                  str
                  ""
                  (concat
                    (get OPENING-BRACKETS t (str "!!!" t)) "\n"
                    (map
                      #(str " " (pr-str %) "\n") edn)
                    (get CLOSING-BRACKETS t (str "!!!" t))
                    )

                  )
        ]
    [:.html
     (menu-item "copy" (partial woof-dom/copy-to-clipboard EDN-STR))
     (if h (str " ;; " h "\n") "\n")
     EDN-STR
     ]
    )
  )


(defn _marker-class-filter [class item st]
  (if st
    (let [classes (reduce set/union #{} (map :class st))]
      (get classes class))
    false
    )
  )

(defn _marker-except-class-filter [class item st]
  (if st
    (let [classes (reduce set/union #{} (map :class st))]
      (not (get classes class)))
    true
    )
  )


(rum/defcs <transform-list> < rum/static
                              (rum/local nil ::filter)
  [st <item> items markers-map & {:keys [id-fn sort-fn copy-fn filter-map api-fns]
                                  :or  {id-fn identity
                                        copy-fn identity
                                        filter-map {}
                                        api-fns []
                                        }
                                  }]

  (let [&markers    (fn [item] (get markers-map (id-fn item)))

        filter-ids (keys filter-map)

        *filter (::filter st)

        ;; select first available filter instead of displaying all
        _ (if (nil? @*filter) (reset! *filter
                                      (if filter-ids (first filter-ids)
                                                     :all)))
        filter-id @*filter

        show-all? (= :all filter-id)

        filter-rule (filter (fn [item]
                              (let [st (&markers item)
                                    xf (get filter-map filter-id (fn [_ _] show-all?))]
                                (xf item st))))

        sorted-items (if sort-fn
                       (sort sort-fn items)
                       items)


        display-items (into [] filter-rule sorted-items )
        ]

    [:div.list
     (menubar (str (count display-items)  " ___ filters: ")
              (into
                [
                 ["copy" (fn []
                           (woof-dom/copy-to-clipboard
                             (str "[\n" (str/join "\n"
                                                  (into []
                                                        (comp filter-rule
                                                              (map copy-fn)
                                                              (map pr-str))
                                                        sorted-items)) "\n]")
                             )
                           )]
                 []
                 ["all" (fn [] (reset! *filter :all))] []]

                (concat
                  (map #(do [(if (= filter-id %)
                               (str "✅️" (pr-str %))
                               (pr-str %)
                               )
                             (fn [] (reset! *filter %))]) filter-ids)
                  api-fns
                  )
                )
              )

     (into [:.items]
           (map (fn [item]
                  (if-let [c (&markers item)]
                    [:.item {:class (str/join " " (reduce set/union #{} (map :class c)))} (<item> item)]
                    (<item> item))))
           display-items)
     ]
    )
  )
