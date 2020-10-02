(ns woof.client.playground.scraper.tw
  (:require
    [cljs.core.async :as async]
    [clojure.string :as string]

    [goog.dom :as dom]
    [goog.dom.classes :as classes]

    [rum.core :as rum]

    [woof.base :as base]
    [woof.data :as d]
    [woof.utils :as utils]

    [woof.browser :as woof-browser]

    [woof.client.dom :as wdom]
    [woof.client.playground.ui :as pg-ui]
    [woof.client.playground.ui.wf :as wf-ui]

    [woof.client.ws :as ws]

    [woof.wfs.evt-loop :as evt-loop]
    [clojure.string :as str])

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;;
;; UI

(defn- load-edn [*dict url k]
  (ws/GET url
          (fn [response]
            (let [edn (cljs.reader/read-string response)]
              (swap! *dict assoc k edn)
              )))

  )

(rum/defc <link> < rum/static
                    {:key-fn (fn [m] (pr-str m))}
  [href els]
  [:.link
   href " - "
   (pr-str (map #(select-keys % #{:text :title}  )
                els
                ))
   ]
  )

(rum/defc <tweet> < rum/static
                    {:key-fn (fn [m] (:tw-id  m))}
  [tw]

  [:.tw

   (if-not (:tw-id tw) [:div.assertion "no :tw-id"])

   (if (empty? (:replies tw))
     [:div.assertion "no :replies"])

   [:header (:title tw) " " [:a {:href (str "https://twitter.com" (:tw-id tw))  :target "_blank"} (:nick tw) ]]


   (let [links (->
           (group-by :href (:links tw))
          ;; remove link to current tweet
           (dissoc (:tw-id tw))
           ;; remove link to user
           (dissoc (str/replace (:nick tw) #"@" "/"))
           )

         ]

     [:.links
      (map #(apply <link> %) links)
      ]

     )

   (let [imgs (->
                (filter (fn[img]
                          (cond
                            (re-find #"/profile_images" (:src img)) false
                            (re-find #"twimg.com/emoji/" (:src img)) false
                            :else true
                            )) (:imgs tw))

                )
         ]


     [:.imgs
      (map (fn [img]
             [:div.img-box
              [:img {:src (:src img)}]
              ]
              ) imgs)
      ]
     )

   ]

  )



(defn cfg-v [st k]
  (get @(::cfg st) k))

(defn <menu-btn> [st k label-fn upd-fn]
  (let [*cfg (::cfg st)
        v (cfg-v st k)
        ui [(label-fn v) (fn [] (swap! *cfg update-in [k] upd-fn))]
        ]
    ui
    )
  )


(defn _def-key-fn [prefix cfg] (str prefix (str/join "|" (::ids cfg))))

(rum/defcs <node> < rum/static
                    (rum/local false ::details?)
                    {:key-fn (fn [cfg m]
                               (str
                                 (:_$  m)
                                 "_"
                                 (str/join (sort (keys m))))

                               )}
  [st cfg node]

  (let [curr-tag (:tag node)]
    [:div.plan

     [:div
      (pg-ui/menubar
        (str (:_$ node)
             " (" (:idx node) ", parent="
             (:parent-idx node) ")"
             )
        [
         [(str "details " (pg-ui/shorten-bool @(::details? st)))
          (fn [] (swap! (::details? st) not))
          ]
         ]
        )
      ]

     (if (= "IMG" curr-tag)
       [:img.el-img {:src (:img-src node)}]

       #_(str
           "<img class='el-img' src='" (wdom/attr (:el n) "src") "'/>"
           )
       )

     (if (= "A" curr-tag)
       [:.el-attr
        [:a {:href (:href node) :target "_blank"} (:href node)]]
       )

     (let [t (:text node)]
       (if (not= "" t)
         [:.el-value t]
         ))



     (if @(::details? st)
       [:.details

        [:hr]
        [:.html (d/pretty! node)]
        ]
       )
     ]
    )

  )


(rum/defc <full-plan> < rum/static
                        {:key-fn (partial _def-key-fn "<full-plan>" )}
  [cfg filtered-plan]

  [:div.full-plan-root

   #_(if (::debugger? cfg)
       [:div.html (d/pretty! cfg) ])

   [:header "FULL PLAN: " (str (count filtered-plan)) ]
   (map (partial <node> cfg) filtered-plan)
   ]
  )



(rum/defc <tree-node> < rum/static
                        {:key-fn (fn [cfg _ node] (str (first (::ids cfg)) "_" (:idx node)))}
  [cfg parent node]

  (let [selector (:_$ node)
        parent-selector (if parent (:_$ parent) "")

        filter-info (get-in cfg [:filter-info (:idx node) :applied-filters] #{})
        selected? (not (empty? filter-info))

        ch (get node :children [])
        has-children? (seq ch)

        short-selector (str/trim (str/replace selector parent-selector ""))
        ]

    [:.plan (cond
              (and (not has-children?) selected?) {:class "selected leaf"}
              selected? {:class "selected"})

     (if selected?

        [:div
         [:.flex
          [:div {:style {:flex-grow 1}} short-selector]
          [:div {:style {:margin-left "auto"
                         :text-align "right"}}
           (map #(pg-ui/<tag> "filter-tag" (str %)) filter-info)]]
         (:text node)

         ]
       [:span short-selector])

     (if has-children?
       (map (fn [child] (<tree-node> cfg
                                     node
                                     child))
            ch))])

  )





(rum/defc <tree-ui> < rum/static
                      {:key-fn (partial _def-key-fn "<tree-ui>")}
  [cfg plan tree-plan]

  (let [selected-idxs (reduce (fn [a n] (conj a (:idx n))) #{} plan)

        filter-info (reduce (fn [a n] (assoc a (:idx n)
                                       {:selected? true
                                        :applied-filters (:applied-filters n)
                                        }
                                       )) {} plan)
        ;; get filter mapping per node idx
        ]

    [:div.tree-root

     (if (::debugger? cfg)
       (<full-plan> cfg plan))

     [:header "TREE:" (pr-str (::ids cfg))]
     (map (fn [item] (<tree-node> (assoc cfg
                                    :selected selected-idxs
                                    :filter-info filter-info
                                    ) nil item)) tree-plan)
     ]
    )


  )



(defn cfg+id [cfg id]
  (update-in cfg [::ids] conj id)
  )


(rum/defc <group> < rum/static
                    {:key-fn (fn [cfg _ parent-idx]
                               (str (first (::ids cfg)) "_" parent-idx))}
  [cfg gr parent-idx]
  [:.node-group
   #_(if (::debugger? cfg)
     [:div.html (d/pretty! cfg) ])

   [:header (pr-str parent-idx)]
   (map (partial <node> cfg)
        (get gr parent-idx))
   ]
  )

(rum/defc <grouped-plan> < rum/static
                           {:key-fn (partial _def-key-fn "<grouped-plan>")}
  [cfg filtered-plan]
  (let [gr (wdom/parent-group filtered-plan)

        roots (sort (keys gr))]
    [:div.grouped-plan-root
     [:header "PLAN GROUPED BY PARENT IDX:"]
     (map (partial <group> cfg gr) roots)
     ]
    )
  )




(rum/defc <h-plan> < rum/static
                     {:key-fn (partial _def-key-fn "<h-plan>")}
  [cfg node]

  (let [_plan (:el-map node)
        filter-fn (:filter/fn cfg)
        mode (:root-UI/mode cfg)
        ]
    (let [filtered-plan (vec
                          (reduce
                            (fn [a n]
                              (if-let [filter-result (filter-fn n)]
                                (conj a (assoc
                                          n :applied-filters (into #{} filter-result)))
                                a
                                )
                              )
                            []
                            _plan )
                          )
          full-tree-plan (wdom/el-plan-as-tree _plan)]

      [:.plan-root.flex

       ;; (if (::debugger? cfg) [:div.html (d/pretty! filtered-plan)])

       (if (#{::all ::tree} mode)
         (<tree-ui>    cfg filtered-plan full-tree-plan))

       (if (#{::all ::grouped-plan} mode)
        (<grouped-plan> cfg filtered-plan))

       (if (#{::all ::full-plan} mode)
         (<full-plan>    cfg filtered-plan))
       ]
      )
    )
  )


;;



(rum/defcs <html-node> < rum/static
                         {:key-fn (fn [n] (:id n))}
                         (rum/local {
                                     ::html? false
                                     } ::cfg)
  [st n]
  (let [
        html-UI (<menu-btn> st ::html?
                            #(str "html " (pg-ui/shorten-bool %)) not)
        html (.-outerHTML (.-parentElement (get-in (:el-map n) [0 :el])))
        ]
    [:li
     (pg-ui/menubar (str "scrape id=" (pr-str (:id n))) [html-UI])

     (if (cfg-v st ::html?)
       [:div.html-preview
        {:dangerouslySetInnerHTML {:__html html}}]
       [:div.html html]
       )
     ]
    )
  )

(rum/defc <node-list> < rum/static
  [cfg nodes]

  [:ul.el-list.flex
   {:class (if (get cfg :root-UI/overflow?) "overflow-x" "")}
   (map <html-node> nodes)])


(def filters-map
  (assoc (sorted-map)
    :img #(= "IMG" (:tag %))
    :link? #(= "A" (:tag %))
    :text? #(not= "" (:text %))
    :leaf? #(= (:child-count %) 0))
  )

(defn make-filter-fn [selected-filters]
  (fn [node]
    (let [sub-filters (select-keys filters-map selected-filters)]
      (filter #(not= nil %) (map (fn [[k v]]
                         (if (v node)
                           k)) sub-filters))
      )
    )
  )

(rum/defcs <dashboard> < rum/static
                         (rum/local {
                                     ::ids               []

                                     ::debugger?         false

                                     :root-UI/mode       ::tree
                                     :root-UI/overflow?  true

                                     :node-list-UI/show? false

                                     :filter/selected-ids (into #{} (keys filters-map))
                                     ;; todo filter
                                     } ::cfg)
  [st nodes]

  (let [

        *cfg (::cfg st)
        cfg  @(::cfg st)

        show (get cfg :root-UI/mode)

        show-UI [(str "show: " (name show))
         (fn []
           (swap! *cfg update-in [:root-UI/mode] {::all          ::tree
                                                  ::tree         ::grouped-plan
                                                  ::grouped-plan ::full-plan
                                                  ::full-plan    ::all}))
         ]

        overflow-UI  (<menu-btn> st :root-UI/overflow? #(str "overflow " (pg-ui/shorten-bool %)) not)
        node-list-UI (<menu-btn> st :node-list-UI/show? #(str "show nodes:" (pg-ui/shorten-bool %)) not)
        debugger-UI  (<menu-btn> st ::debugger? #(str "debug " (pg-ui/shorten-bool %)) not)

        ;; { k #{a b c d e f}}, where a b c - are node ids
        selector-usage (reduce
                         (fn [a node]
                           (reduce (fn [b sl]
                                     (update-in b [sl] (fnil conj #{}) (:id node))
                                     ) a
                                   (map #(get % :_$) (:el-map node))))
                         (sorted-map) nodes)
        selected-filters (cfg-v st :filter/selected-ids)
        ]

    [:div.scrape-ide
     (pg-ui/menubar "" [node-list-UI [] overflow-UI debugger-UI [] show-UI])

     (if (cfg-v st :node-list-UI/show?)
       (<node-list> cfg nodes))

     [:div.filters
      [:.menubar
       [:header "FILTER: any of"]
       (map (fn [filter-id]
              (let [selected? (get selected-filters filter-id )]
                (pg-ui/menu-item (str (pg-ui/shorten-bool selected?) " " (name filter-id))
                                 (fn []
                                   (swap! *cfg update :filter/selected-ids
                                          (if selected? disj conj)
                                          filter-id)))
                )
              )
            (keys filters-map))
       ]
      ]


     [:div.flex.plans {:class (if (cfg-v st :root-UI/overflow?) "overflow-x" "")}

      (let [filter-fn (make-filter-fn selected-filters)

            nu-cfg (assoc cfg ::usage selector-usage
                              :filter/fn filter-fn) ]
        (map #(<h-plan>
                (update-in nu-cfg [::ids] conj (:id %))
                %)
             nodes))
      ]
     ]
    )
  )

;; yt parsing dashboard
(rum/defc <yt> < rum/reactive
  [st *state]

  [:div
   (let [skip-fn (fn [$el $]
                   (#{"SVG" "G" "PATH"} (str/upper-case (.-tagName $el))))

         els (wdom/q* "#contents #content")

         id-1 0 ;(rand-int (count els)) ;
         id-2 1 ;(rand-int (count els)) ;(nth coll )

         ;; for now just take random items
         el-map-1 (wdom/el-map (nth els id-1)
                               :skip-fn skip-fn
                               :node-fn wdom/enrich-node)

         el-map-2 (wdom/el-map (nth els id-2)
                               :skip-fn skip-fn
                               :node-fn wdom/enrich-node)

         el-1 (reduce (fn [a node] (assoc a (:_$ node) node)) {} el-map-1)
         el-2 (reduce (fn [a node] (assoc a (:_$ node) node)) {} el-map-2)

         ]

     (<dashboard>
       [
        {:id id-1

         :el-map el-map-1
         :nodes el-1
         }
        {
         :id id-2

         :el-map el-map-2
         :nodes el-2
         }
        ]))]

  )


;;;;;;;;;;;;
;;


(rum/defcs <wf-root> < rum/reactive
                       (rum/local {} ::inline-results?)
  [local *wf]

  (let [wf @*wf]
    [:div
     (if (= :not-started (:status wf))
       [:div "wf is not running"]

       (let [*state (rum/cursor-in *wf [:state])]
         [:div.example-tw
          (<yt> (get-in wf [:state]) *state)
          ]
         #_[:div.example-tw
          (<tw> (get-in wf [:state]) *state)
          ;(<tw-real> (get-in wf [:state]) *state)
          ]
         )

       )
     ]
    )
  )


(defonce *history-swipe-preventor (volatile! false))
;;
;; WF definition
(defn wf! [*SWF]

  (when-not @*history-swipe-preventor
    (.addEventListener js/document "touchmove" (fn [event]
                                                 (.preventDefault event)))
    (vreset! *history-swipe-preventor true)
    )

  (.clear js/console)

  (let [CHAN-FACTORY (base/chan-factory (atom {}))
        *state (rum/cursor-in *SWF [:state])]
    {

     :title       "twitor history"
     :explanation [:div.explanation
                   [:p "Analyze scraped data here"]]

     ;; this state will be added to a wf?
     :state {
             :some-state :here
             }

     :init-fns    [


                   { ;; pass modifiable zipper
                    ::*state *state
                    }

                   (base/build-init-chan-factory-fn CHAN-FACTORY)
                   (evt-loop/build-evt-loop-init-fn (base/make-chan CHAN-FACTORY (base/rand-sid "evt-")))
                   ]
     ;;
     :ctx-fns     [
                   evt-loop/evt-loop-ctx-fn
                   woof-browser/common-ctx ;; re-use common browser step handlers
                   wdom/dom-ctx
                   ]

     ;;
     :steps-fns   [(fn [params]
                     {
                      ::#EVT-LOOP# [:evt-loop (evt-loop/&evt-loop params)]

                      ::hello [:log "Hello"]

                      ;; :CSS/custom-css-file [:css-file "http://localhost:9500/css/t.css"]

                      })]

     :opt-fns     [
                   (base/build-opts-chan-factory-fn CHAN-FACTORY)
                   ;; uncomment to display wf results (esp. if there was an error)
                   (base/build-opt-on-done (fn [params result]
                                             (.warn js/console params result)))
                   ]

     :ui-fn       (partial wf-ui/<wf-UI> (partial <wf-root>))

     ;; dev stuff
     :playground/keys-to-update-on-reload [

                                           :actions
                                           :title
                                           :explanation
                                           :wf-actions

                                           ;; overwrite ui
                                           :ui-fn

                                           ;; update/overwrite workflow
                                           :init-fns :steps-fns :opt-fns
                                           ;; overwrite the state
                                           ;:state
                                           ]
     })
  )



;; old

(rum/defc <tw> < rum/reactive
  [st *state]

  [:div
   (pg-ui/menubar "tw"
                  [
                   ;["load tw 1" (fn [] (load-edn *state "/s/twitter/parsed.edn" :tweets))]
                   ;["load tw 2" (fn [] (load-edn *state "/s/twitter/parsed_real.edn" :tweets))]

                   ["load tw 01" (fn [] (load-edn *state "/s/twitter/tw_01.edn" :tweets))]
                   ["load tw 02" (fn [] (load-edn *state "/s/twitter/tw_02.edn" :tweets))]
                   ["load tw 03" (fn [] (load-edn *state "/s/twitter/tw_03.edn" :tweets))]
                   ])



   #_[:p "processing twittor "]

   #_[:ul
      [:li "text + link"]
      [:li "text + photos - /user/...photo/n"]
      [:li "retweet of a link - has other @tweet handle"]
      [:li "tweet with hash tag - /hashtag/...."]
      [:li "tweet with youtube, link with :title and :text = https://youtu.be/..."]
      ]


   ;; uncomment this to work with actually scraped data

   #_(when-let [tweets (:tweets st)]

       (<h-plan> (:full-dom-plan (first tweets)))

       ;(map <tweet> tweets)
       )

   ]
  )



(rum/defc <tw-real> < rum/reactive
  [st *state]

  [:div
   (pg-ui/menubar "tw"
                  [
                   ;["load tw 1" (fn [] (load-edn *state "/s/twitter/parsed.edn" :tweets))]
                   ;["load tw 2" (fn [] (load-edn *state "/s/twitter/parsed_real.edn" :tweets))]

                   ["load tw 01" (fn [] (load-edn *state "/s/twitter/tw_01.edn" :tweets))]
                   ["load tw 02" (fn [] (load-edn *state "/s/twitter/tw_02.edn" :tweets))]
                   ["load tw 03" (fn [] (load-edn *state "/s/twitter/tw_03.edn" :tweets))]
                   ])


   [:p "processing twittor "]

   [:ul
    [:li "text + link"]
    [:li "text + photos - /user/...photo/n"]
    [:li "retweet of a link - has other @tweet handle"]
    [:li "tweet with hash tag - /hashtag/...."]
    [:li "tweet with youtube, link with :title and :text = https://youtu.be/..."]
    ]

   ;; uncomment this to work with actually scraped data
   ;; todo: scrape data with proper el-plan structure

   #_(when-let [tweets (:tweets st)]

       ;(<h-plan> (:full-dom-plan (first tweets)))


       ;(map <tweet> tweets)
       )

   ]
  )