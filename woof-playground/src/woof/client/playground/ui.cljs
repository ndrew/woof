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
               (let [has-label? (not (nil? label))
               				  has-action? (not (nil? action))]

               				  (cond 
               				  		(and has-label? has-action?)		(menu-item label action)
               				  		has-label? [:.header label]
               				  		:else [:.separator]
               				  )
																	)
               )
               
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

(defn shorten-bool 
	([b]
  (if b
    "✓" "✕"))
 ([s b]
 	 (str (if b "✓" "✕") s)))


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


;;
;; 
(rum/defcs <transform-list> < rum/static
                              (rum/local nil ::filter)
  [st <item> items markers-map & {:keys [id-fn  ;; each item should have unique id
  																																							
  																																							sort-fn 
  																																							
  																																							copy-fn 
  																																							filter-map 
  																																							api-fns

  																																							group-fn

  																																							global-fn-map 
  																																							group-fn-map
  																																]
                                  :or  {id-fn identity
                                        copy-fn identity
                                        filter-map {}
                                        api-fns []
                                        group-fn nil

                                        global-fn-map {}
                                        group-fn-map {}
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
								grouping? (not (nil? group-fn))
        

        filter-xs (filter (fn [item]
                              (let [st (&markers item)
                                    xf (get filter-map filter-id (fn [_ _] show-all?))]
                                (xf item st))))

        
        react-k-xs (map #(assoc % :_k (str (id-fn %))))
        group-xs   (map #(assoc % :_g (if grouping? (group-fn %))))
        markers-xs (map #(assoc % :_m (get markers-map (id-fn %))))

        xs (comp filter-xs 
        									react-k-xs
        									group-xs
        									markers-xs
        									)

        ;; as sort cannot be handled via trasducer
        sorted-items (if sort-fn
                       (sort sort-fn items)
                       items)


        display-items (into [] 
        																		xs 
        																		sorted-items)
      

        global-menu (into
                [
                	[(str "total: " (count display-items) )]

                 ["copy" (fn [] ;; deprecated
                           (woof-dom/copy-to-clipboard
                             (str "[\n" (str/join "\n"
                                                  (into []
                                                        (comp (map copy-fn)
                                                              (map pr-str))

                                                        display-items)) "\n]")
                             )
                           )]
                 

                 [" filters: "]
                 ;; generic filter 
                 ["all" (fn [] (reset! *filter :all))] []]

                (concat
                  (map #(do [(if (= filter-id %)
                               (str "✅️" (pr-str %))
                               (pr-str %))
                             (fn [] (reset! *filter %))]) filter-ids)
                  [[" api (sort):"]]
                  api-fns
                  ;; 
                  [[" GLOBAL "]]
																  (map (fn [[k f]]
                 	 	 [k (fn [] (f display-items) )]
                 	 ) global-fn-map)
                  )
                )

        ;; expose classes for each item in wrapper
        ;; todo: maybe do grouping here?
        ;;  - no grouping: [[item][item]]
        ;;  - grouping  [[item item item] [item item] ]
        ;; todo: ui for item?
        
        render-list-xs (map (fn [item]
                  		(if-let [c (get item :_m)]
                    			[:.item {:class (str/join " " (reduce set/union #{} (map :class c))) :key (:_k item)} (<item> item)]
			                    [:.item {:key (:_k item)} (<item> item)]
                    )))

        groupped-items (group-by :_g display-items)

        ]

    [:div.list

     (menubar "" global-menu)
     (if grouping? 
      (into [:.items] 
      	  (map (fn [[k v]]
		      	  ;; todo: migrate to a separate component
      	  		 (let [group-k (str k)
      	  		 				  group-actions (into []
      	  		 				  																				(map (fn [[k f]] [k (partial f v)]) group-fn-map))
      	  		 					] 
															[:.group 
																 (rum/with-key (menubar (str group-k) group-actions) group-k)	 
											      (into [:.items] render-list-xs v)	  		 		
		      	  		 ]
      	  		 )
      	  		       	  	
      	  ))
      			groupped-items)
     ;; render list
						(into [:.items] render-list-xs display-items)
     )

     ; [:pre (pr-str (keys groupped-items))]
     
     ]
    )
  )
