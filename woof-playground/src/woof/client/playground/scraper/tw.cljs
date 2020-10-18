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



(def filters-map
  (assoc (sorted-map)
    :img? #(= "IMG" (:tag %))
    :link? #(= "A" (:tag %))
    :text? #(not= "" (str/trim (:text %)))
    :leaf? #(= (:child-count %) 0))
  )


(defn img-scrape [n]
  (select-keys n #{:img-src}))

(defn link-scrape [n]
  (select-keys n #{:href}))

(defn text-scrape [n]
  (select-keys n #{:text}))


;; todo:
(def filter-scraping-rules
  {
   :img? img-scrape
   :link? link-scrape
   :text? text-scrape
   })


(defn make-filter-fn [selected-filters]
  (fn [node]
    (let [sub-filters (select-keys filters-map selected-filters)]
      (filter #(not= nil %) (map (fn [[k v]]
                                   (if (v node)
                                     k)) sub-filters))
      )
    )
  )

(defn selector-usage [nodes]
  (reduce
    (fn [a node]
      (reduce (fn [b sl]
                (update-in b [sl] (fnil conj #{}) (:id node))
                ) a
              (map #(get % :_$) (:el-map node))))
    (sorted-map) nodes))


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


(defn _def-key-fn [prefix cfg] (str (str/join "|" (::ids cfg)) "_" prefix ))


(rum/defc <tag> < rum/static
                  {:key-fn (fn [cfg node filter-id]
                             (str
                               (first (::ids cfg)) "_"
                               (get cfg :node/prefix "")
                               "_" (:idx node)
                               "_" (name filter-id)
                               )
                             )}
  [cfg node filter-id]

  (let [applied-filters (get node :applied-filters #{})]
    (pg-ui/<tag> (str "small-tag " "filter-tag "
                      (if (applied-filters filter-id) "applied-tag" ""))
                 (str filter-id))
    )

  )

(rum/defc <plan-header> < rum/static
                 {:key-fn (fn [cfg *details? m ]
                            (str
                              (first (::ids cfg)) "_"
                              (get cfg :node/prefix "")
                              "_" (str @*details?)
                              "_" (:idx  m)

                              )
                            )}
  [cfg *details? node]
  (let [parent-selector (get cfg :node/parent-selector "")
        selector (:_$ node)
        partial-selector (wdom/shorten-selector-string selector parent-selector)

        details? @*details?
        idx (:idx node)

        exclusions (get cfg :filter/exclusions #{})

        excluded? (exclusions selector)

        aliases (get cfg :selector/aliases {})


        analysis-nodes (get cfg :selector/analysis {})

        analyzed? (analysis-nodes selector)

        upd! (:cfg/upd! cfg)

        quick-menu [(if analyzed?
                      ["cancel analysis" (fn [] (upd! :selector/analysis (dissoc analysis-nodes selector)))]
                      ["analysis"          (fn [] (upd! :selector/analysis (assoc analysis-nodes selector {
                                                                                                           :parent-selector parent-selector
                                                                                                           :node node
                                                                                                           })))])
                    (if excluded?
                      ["cancel exclusion" (fn [] (upd! :filter/exclusions (disj exclusions selector)))]
                      ["exclude"          (fn [] (upd! :filter/exclusions (conj exclusions selector)))])
                    ["alias" (fn []
                               (upd! :selector/aliases
                                (assoc aliases
                                  selector
                                  (js/prompt "provide an alias" "")
                                  ))

                               )]]
        ]

    [:div.plan-header

     (pg-ui/menubar "" [[(if details? "▿" "▹") (fn [] (swap! *details? not))]]
                    :class "minimal" )


     (if-let [alias (get aliases selector)]
       [:span.tag.alias (pr-str alias)]
       )

     [:span.small-tag.tag.selector partial-selector]

     (if details?
       (pg-ui/menubar ""
                      (conj quick-menu
                            []
                            ["📋full $" (fn [] (wdom/copy-to-clipboard selector))]
                            ["📋partial $" (fn [] (wdom/copy-to-clipboard partial-selector))] ))
       (pg-ui/menubar "" quick-menu)

       )

     [:.tags (map #(<tag> (assoc cfg
                            :node/prefix "tag_"
                            ) node %) (:matching-filters node))]

     [:span.small-tag.tag.idx (str idx)
      [:sup (str (:parent-idx node))]]
     ]
    )

  )

(rum/defcs <node> < rum/static
                    (rum/local false ::details?)
                    {:key-fn (fn [cfg m]
                               (str
                                 (first (::ids cfg)) "_"
                                 (get cfg :node/prefix "")
                                 (:idx  m)
                                 ; "_" (str/join (sort (keys m)))
                                 )

                               )}
  [st cfg node]

  (let [curr-tag (:tag node)
        details? @(::details? st)
        applied-filters (get node :applied-filters #{})

        selector (:_$ node)

        exclusions (get cfg :filter/exclusions #{})
        excluded? (exclusions selector)

        used-by (get (::usage cfg) selector #{})
        node-id (first (::ids cfg))
        used-by-others (disj used-by node-id)


        ]

    [:div.plan
     {:class (str/join " " (concat (if (empty? applied-filters) #{"not-matched"} )
                                   (if excluded? #{"excluded"})
                                   (if (empty? used-by-others) #{"unique"})))}


     (<plan-header> (assoc cfg
                      :node/prefix "pl_header_")
                      (::details? st) node)

     [:.details


      (if (= "IMG" curr-tag)
        [:img.el-img {:src (:img-src node)}]
        #_(str
            "<img class='el-img' src='" (wdom/attr (:el n) "src") "'/>"
            )
        )

      (if (= "A" curr-tag)
        [:.el-attr
         [:a {:href (:href node) :target "_blank"} (:href node)]])

      (let [t (:text node)]
        (if (not= "" t)
          [:.el-value t]))

      ;; todo: data attributes

      (if details?
        [:.html (d/pretty! node)]
        )
      ]

     ]
    )

  )


(rum/defc <group> < rum/static
                    {:key-fn (fn [cfg _ _ parent-idx]
                               (str (first (::ids cfg)) "_" "group_"  parent-idx))}
  [cfg plan gr parent-idx]
  (let [parent-node (get plan parent-idx)
        parent-selector (if parent-node (:_$ parent-node) "")
        nodes (get gr parent-idx)

        all-applied-filters (reduce (fn [a n]
                                      (into a (get n :applied-filters #{}))
                                      ) #{} nodes)
        ]
    [:.node-group
     (if (empty? all-applied-filters) {:class "not-matched"})

     #_(if (::debugger? cfg)
         [:div.html (d/pretty! cfg) ])

     [:.plan-header
      [:.tag.small-tag.parent-selector parent-selector] "[" (pr-str (inc parent-idx)) "]"]

     (map (partial <node> (assoc cfg
                            :node/prefix "group_"
                            :node/parent-selector parent-selector))
          nodes)
     ]
    )

  )


(defn build-scrape-plan [cfg plan]
  (.groupCollapsed js/console "SCRAPE PLAN")
  (let [result (reduce (fn [a n]
                         (let [selector (:_$ n)
                               applied-filters (:applied-filters n)]
                           (if
                             (and
                               (not (empty? applied-filters))
                               (not (get-in cfg [:filter/exclusions selector])))
                             (let []
                               (conj a
                                     (merge
                                       {:_$ selector
                                        :filters applied-filters}
                                       (reduce (fn [_a [k f]]
                                                 (merge _a (f n))
                                                 )
                                               {}
                                              (select-keys filter-scraping-rules applied-filters))
                                       (if-let [alias (get-in cfg [:selector/aliases selector])]
                                         {:alias alias})
                                       )
                                     )
                               )
                             a
                             )
                           )


                         ) [] plan)]
    (.groupEnd js/console)

    result
    )

  )

(rum/defcs <full-plan> < rum/static
                         (rum/local true ::hide-not-matched?)
                         {:key-fn (partial _def-key-fn "<full-plan>")}
  [st cfg plan]

  (let [hide? @(::hide-not-matched? st)]
    [:div.grouped-plan-root
     (if hide? {:class "hide-not-matched"} {})

     [:div
      (pg-ui/menubar "" [
                         [(if hide? "show all" "show only matched") (fn [] (swap! (::hide-not-matched? st) not))]
                         [] [] [] ["BUILD PARSE PLAN" (fn []
                                                        (let [scrape-plan (build-scrape-plan cfg plan)]
                                                          (.log js/console scrape-plan)
                                                          (wdom/copy-to-clipboard scrape-plan)
                                                          )
                                                        )]
                         ])
      ]

     (let [gr (wdom/parent-group plan)
           roots (sort (keys gr))]
       (map (partial <group> cfg plan gr) roots))
     ]
    )
  )


(defn filter-class [filter-k]
  (str "filter__" (str/replace (name filter-k)
                                "?" "\\?")))

(defn tree-key-fn [cfg k]
  (str (first (::ids cfg)) "_" k)
  )

(rum/defcs <selector> < rum/static
                        {:key-fn (fn [cfg s] (tree-key-fn cfg s))}
  [st cfg short-selector node]
  (let [filter-info (get-in cfg [:filter-info (:idx node)])
        applied-filters (:applied-filters filter-info)
        selected? (not (empty? applied-filters))
        existing-classes (get-in node [:attrs "class"] "")
        $tags [:.tags (map #(pg-ui/<tag> (str "small-tag filter-tag "
                                              (if (applied-filters %) "applied-tag" "")
                                              )
                                         (str %)) (:matching-filters filter-info))]
        ]
    (if selected?
      [:header.flex
       [:.selector short-selector
        " ___(" existing-classes ")"
        ]
       $tags]
      [:.short.flex
       [:.selector short-selector]
       $tags
       ]
      )

    )

  )

(rum/defc <tree-node> < rum/static
                        {:key-fn (fn [cfg _ node] (str (first (::ids cfg)) "_" (:idx node)))}
  [cfg _parent _node]

  (let [direct-children (get _node :children [])

        linear-children (loop [ch direct-children
                               r [_node]]
                          (if (and (= 1 (count ch))
                                   (empty? (:matching-filters (first ch))))
                            (do
                              (recur
                                (get (first ch) :children [])
                                (conj r (first ch)))
                              )
                            (drop-last r)))

        collapse? (:tree-UI/collapse? cfg)

        _parent-selector (if _parent
                           (:_$ _parent) "")

        ;; todo: check if collapsing does not affect shortening
        parent-selector _parent-selector #_(if collapse?
                          (if (seq linear-children)
                            (:_$ (second (reverse linear-children)))
                            ""
                            )
                          _parent-selector
                          )



        node (if collapse?
               (if (seq linear-children) (last linear-children) _node)
               _node)

        selector (:_$ node)

        filter-info (get-in cfg [:filter-info (:idx node)] )
        applied-filters (:applied-filters filter-info)
        selected? (not (empty? applied-filters))

        has-children? (seq direct-children)

        ;; has-children?-1 (seq (get node :children []))

        short-selector (wdom/shorten-selector-string selector parent-selector)

        used-by (get (::usage cfg) selector #{})
        node-id (first (::ids cfg))

        used-by-others (disj used-by node-id)


        node-class (str/join " " (concat (if selected? #{"selected"} )
                                         (if (not has-children?) #{"leaf"})
                                         (if (= 1 (count (get node :children []))) #{"single-child"})
                                         (if (empty? used-by-others) #{"unique"})

                                         (map filter-class applied-filters)
                                         ))
        ]

    [:.tree-node {:class node-class}

     #_(if (nil? _parent)
       [:.debug
        selector
        [:hr]
        (pr-str (:$ node))
        [:hr]
        (pr-str
          (wdom/to-selector (concat []
                                    ))
          )
        [:hr]
        short-selector
        ]
       )


     (if (::debugger? cfg)
       [:.debug

        #_[:.html
         (pr-str
           (count direct-children)
           "vs"
           (count (get node :children []))
           ;[has-children? has-children?-1]
           )
         ]
        (<node> cfg node)
        ]
       (if selected?
         [:.detailed
          (<selector> cfg short-selector node)
          ;; todo: what to show here
          (let [curr-tag (:tag node)]
            [:.content

             (if (= "IMG" curr-tag)
               [:img.el-img {:src (:img-src node)}])

             (if (= "A" curr-tag)
               [:.el-attr [:a {:href (:href node) :target "_blank"} (:href node)]])

             (let [t (:text node)]
               (if (not= "" t)
                 [:.el-value t]))
             ]
            )
          ]
         (<selector> cfg short-selector node))
       )


     (if has-children?
       (map (fn [child] (<tree-node> cfg
                                     node
                                     child))
            direct-children))]

    )
  )





(rum/defc <tree-plan> < rum/static
                        {:key-fn (partial _def-key-fn "<tree-ui>")}
  [cfg plan tree-plan]

  (let [selected-idxs (reduce (fn [a n] (conj a (:idx n))) #{} plan)
        ;; get filter mapping per node idx
        filter-info (reduce (fn [a n]
                              (assoc a (:idx n)
                                       {:selected?        true
                                        :matching-filters (:matching-filters n)
                                        :applied-filters  (:applied-filters n)}
                                       )) {} plan)

        ]

    [:div.tree-root
     (if (:tree-UI/horizontal? cfg ) {:class "horizontal"})

     #_(if (::debugger? cfg)
       (<full-plan> cfg plan))

     ;; migrate to defcs in order to add flex grow
     [:header "TREE:" (pr-str (::ids cfg))]

     [:.tree-nodes
      (map (fn [item] (<tree-node> (assoc cfg
                                     :selected selected-idxs
                                     :filter-info filter-info
                                     ) nil item)) tree-plan)
      ]
     ]
    )
  )



(defn cfg+id [cfg id]
  (update-in cfg [::ids] conj id)
  )




(rum/defc <plan> < rum/static
                   {:key-fn (partial _def-key-fn "<h-plan>")}
  [cfg node]

  (let [plan (:el-map node)
        mode (:root-UI/mode cfg)

        full-tree-plan (wdom/el-plan-as-tree plan)
        ]

    [:.plan-root.flex

     ;(if (::debugger? cfg) [:div.html (d/pretty! (::usage cfg))])

     (if (#{::all ::tree} mode)
       (<tree-plan> cfg plan full-tree-plan))

     (if (#{::all ::plan} mode)
       (<full-plan>    cfg plan))
     ]
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



(rum/defcs <dashboard>
  < rum/static
    (rum/local {
                ;; generic cfg
                ::ids                []
                ::debugger?          false

                ;; root ui
                :root-UI/mode        ::plan ;::tree
                :root-UI/overflow?   true

                ;; node ui
                :node-list-UI/show?  false

                ;; tree ui
                :tree-UI/horizontal? false
                :tree-UI/collapse?   true

                ;; filter ui
                :filter/selected-ids (disj (into #{} (keys filters-map)) :leaf?)
                :filter/all-ids (keys filters-map)

                :filter/exclusions #{}

                :selector/aliases {}
                :selector/analysis {}

                } ::cfg)
  [st nodes]

  (let [*cfg (::cfg st)
        _cfg  @(::cfg st)

        upd! (fn [k v]
              (swap! *cfg assoc k v)
              )

        ;; { k #{a b c d e f}}, where a b c - are node ids
        selector-usage-map (selector-usage nodes)
        selected-filters (cfg-v st :filter/selected-ids)

        filter-fn  (make-filter-fn (cfg-v st :filter/all-ids))
        cfg     (assoc _cfg
                     ::usage selector-usage-map
                     :filter/fn filter-fn
                     :cfg/upd! upd!)

        show (get cfg :root-UI/mode)

        show-UI [(str "show: " (name show))
         (fn []
           (swap! *cfg update-in [:root-UI/mode] {::all          ::tree
                                                  ::tree         ::plan
                                                  ::plan         ::all}))
         ]
        overflow-UI  (<menu-btn> st :root-UI/overflow? #(str "overflow " (pg-ui/shorten-bool %)) not)
        node-list-UI (<menu-btn> st :node-list-UI/show? #(str "show nodes:" (pg-ui/shorten-bool %)) not)
        debugger-UI  (<menu-btn> st ::debugger? #(str "debug " (pg-ui/shorten-bool %)) not)



        tree-UI-horizontal  (<menu-btn> st :tree-UI/horizontal? #(str "tree: horizontal " (pg-ui/shorten-bool %)) not)
        tree-UI-collapse  (<menu-btn> st :tree-UI/collapse? #(str "collapse intermediary nodes " (pg-ui/shorten-bool %)) not)

        ]

    [:div.scrape-ide
     (pg-ui/menubar "" [show-UI [] node-list-UI
                        [] [] [] tree-UI-horizontal tree-UI-collapse [] [] [] overflow-UI debugger-UI])


     (if (cfg-v st :node-list-UI/show?)
       (<node-list> cfg nodes))

     [:div.filters
      [:.filter.menubar
       [:header "FILTER: any of"]
       (map (fn [filter-id]
              (let [selected? (get selected-filters filter-id )]
                [:a.menu-item
                 {:href     "#"
                  :class    (filter-class filter-id)
                  :on-click (fn [e]
                              (swap! *cfg update :filter/selected-ids
                                     (if selected? disj conj)
                                     filter-id)
                              (.preventDefault e)
                              false)}
                 (str (pg-ui/shorten-bool selected?) " " (name filter-id))]
                )
              )
            (keys filters-map))
       ]
      [:.exclusions.menubar
       [:header "exclusions"]
       (pr-str (get cfg :filter/exclusions))
       ]
      [:.aliases.menubar
       [:header "aliases"]
       (pr-str (get cfg :selector/aliases))
       ]
      ]

     (let [analysis-map (get cfg :selector/analysis)]

       [:div.analysis
        [:header "ANALYSIS"]
        (map
          (fn [[k v]]
            [:div (pr-str k)
             [:div ; (pr-str v)
              ]
             ]
            )
          analysis-map
          )
        ]
       )


     ;[:.html (d/pretty! selected-filters)]

     [:div.flex.plans {:class (if (cfg-v st :root-UI/overflow?) "overflow-x" "")}
      (map #(<plan>
              (update-in cfg [::ids] conj (:id %))
              (assoc %
                :el-map (vec (map (fn [n]
                                    (let [selected-filters-ids (get cfg :filter/selected-ids)
                                          filters-matched (into #{} (filter-fn n))
                                          filter-result (clojure.set/intersection filters-matched selected-filters-ids)]
                                      (assoc n
                                        :matching-filters filters-matched
                                        :applied-filters filter-result
                                        ))) (:el-map %)))
                )
              )
           nodes)
      ]
     ]
    )
  )

;; yt parsing dashboard
(rum/defc <yt> < rum/reactive
  [st *state]

  [:div
   (let [skip-fn (fn [$el $]
                   (#{; "SVG"
                      "G" "PATH"} (str/upper-case (.-tagName $el))))

         els (wdom/q* "#contents #content")

         ;; taking first and second items
         ;id-1 0 ;(rand-int (count els)) ;
         id-2 0 ;;1

         ;; taking random items
         id-1 (rand-int (count els)) ;
         ;;id-2 (rand-int (count els)) ;(nth coll )


         _el-map-1 (wdom/el-map (nth els id-1)
                               :skip-fn skip-fn
                               :node-fn wdom/enrich-node
                               :top-selector-fn (fn [base el] { :nth-child (:i base)}))


         _el-map-2 (wdom/el-map (nth els id-2)
                               :skip-fn skip-fn
                               :node-fn wdom/enrich-node
                               :top-selector-fn (fn [base el] { :nth-child (:i base)}))

         el-map-1 [{:child-count 4,
                     :text "",
                     :$ [{:t "A", :i 1, :nth-child 1}],
                     :tag "A",
                     :_$ "A:nth-child(1)",
                     :parent-idx 0,
                     :idx 1}
                    {:child-count 0,
                     :text "",
                     :$ [{:t "DIV", :i 2, :nth-child 2}],
                     :tag "DIV",
                     :_$ "DIV:nth-child(2)",
                     :parent-idx 0,
                     :idx 2}
                    {:child-count 1,
                     :text "",
                     :$ [{:t "DIV", :i 3, :nth-child 3}],
                     :tag "DIV",
                     :_$ "DIV:nth-child(3)",
                     :parent-idx 0,
                     :idx 3}
                    {:child-count 1,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "YTD-THUMBNAIL", :i 1, :child-count 4, :nth-child 1}],
                     :_$ "A:nth-child(1) > YTD-THUMBNAIL:nth-child(1)",
                     :tag "YTD-THUMBNAIL",
                     :idx 4,
                     :parent-idx 1}
                    {:child-count 2,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}],
                     :_$ "A:nth-child(1) > DIV:nth-child(2)",
                     :tag "DIV",
                     :idx 5,
                     :parent-idx 1}
                    {:child-count 0,
                     :text "\n      ",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "YTD-BADGE-SUPPORTED-RENDERER",
                                    :i 3,
                                    :child-count 4,
                                    :nth-child 3}],
                     :_$ "A:nth-child(1) > YTD-BADGE-SUPPORTED-RENDERER:nth-child(3)",
                     :tag "YTD-BADGE-SUPPORTED-RENDERER",
                     :idx 6,
                     :parent-idx 1}
                    {:child-count 0,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "YT-FORMATTED-STRING", :i 4, :child-count 4, :nth-child 4}],
                     :_$ "A:nth-child(1) > YT-FORMATTED-STRING:nth-child(4)",
                     :tag "YT-FORMATTED-STRING",
                     :idx 7,
                     :parent-idx 1}
                    {:child-count 2,
                     :text "",
                     :$
                                  [{:t "DIV", :i 3, :nth-child 3}
                                   {:t "YTD-MENU-RENDERER", :i 1, :child-count 1}],
                     :_$ "DIV:nth-child(3) > YTD-MENU-RENDERER",
                     :tag "YTD-MENU-RENDERER",
                     :idx 8,
                     :parent-idx 3}
                    {:child-count 4,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "YTD-THUMBNAIL", :i 1, :child-count 4, :nth-child 1}
                                   {:t "A", :i 1, :child-count 1}],
                     :_$ "A:nth-child(1) > YTD-THUMBNAIL:nth-child(1) > A",
                     :tag "A",
                     :idx 9,
                     :parent-idx 4}
                    {:child-count 2,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "H3", :i 1, :child-count 2, :nth-child 1}],
                     :_$ "A:nth-child(1) > DIV:nth-child(2) > H3:nth-child(1)",
                     :tag "H3",
                     :idx 10,
                     :parent-idx 5}
                    {:child-count 2,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-VIDEO-META-BLOCK", :i 2, :child-count 2, :nth-child 2}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > YTD-VIDEO-META-BLOCK:nth-child(2)",
                     :tag "YTD-VIDEO-META-BLOCK",
                     :idx 11,
                     :parent-idx 5}
                    {:child-count 0,
                     :text "",
                     :$
                                  [{:t "DIV", :i 3, :nth-child 3}
                                   {:t "YTD-MENU-RENDERER", :i 1, :child-count 1}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}],
                     :_$ "DIV:nth-child(3) > YTD-MENU-RENDERER > DIV:nth-child(1)",
                     :tag "DIV",
                     :idx 12,
                     :parent-idx 8}
                    {:child-count 1,
                     :text "",
                     :$
                                  [{:t "DIV", :i 3, :nth-child 3}
                                   {:t "YTD-MENU-RENDERER", :i 1, :child-count 1}
                                   {:t "YT-ICON-BUTTON", :i 2, :child-count 2, :nth-child 2}],
                     :_$
                                  "DIV:nth-child(3) > YTD-MENU-RENDERER > YT-ICON-BUTTON:nth-child(2)",
                     :tag "YT-ICON-BUTTON",
                     :idx 13,
                     :parent-idx 8}
                    {:child-count 1,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "YTD-THUMBNAIL", :i 1, :child-count 4, :nth-child 1}
                                   {:t "A", :i 1, :child-count 1}
                                   {:t "YT-IMG-SHADOW", :i 1, :child-count 4, :nth-child 1}],
                     :_$
                                  "A:nth-child(1) > YTD-THUMBNAIL:nth-child(1) > A > YT-IMG-SHADOW:nth-child(1)",
                     :tag "YT-IMG-SHADOW",
                     :idx 14,
                     :parent-idx 9}
                    {:child-count 4,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "YTD-THUMBNAIL", :i 1, :child-count 4, :nth-child 1}
                                   {:t "A", :i 1, :child-count 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}],
                     :_$
                                  "A:nth-child(1) > YTD-THUMBNAIL:nth-child(1) > A > DIV:nth-child(2)",
                     :tag "DIV",
                     :idx 15,
                     :parent-idx 9}
                    {:child-count 0,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "YTD-THUMBNAIL", :i 1, :child-count 4, :nth-child 1}
                                   {:t "A", :i 1, :child-count 1}
                                   {:t "DIV", :i 3, :child-count 4, :nth-child 3}],
                     :_$
                                  "A:nth-child(1) > YTD-THUMBNAIL:nth-child(1) > A > DIV:nth-child(3)",
                     :tag "DIV",
                     :idx 16,
                     :parent-idx 9}
                    {:child-count 0,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "YTD-THUMBNAIL", :i 1, :child-count 4, :nth-child 1}
                                   {:t "A", :i 1, :child-count 1}
                                   {:t "DIV", :i 4, :child-count 4, :nth-child 4}],
                     :_$
                                  "A:nth-child(1) > YTD-THUMBNAIL:nth-child(1) > A > DIV:nth-child(4)",
                     :tag "DIV",
                     :idx 17,
                     :parent-idx 9}
                    {:child-count 1,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "H3", :i 1, :child-count 2, :nth-child 1}
                                   {:t "YTD-BADGE-SUPPORTED-RENDERER",
                                    :i 1,
                                    :child-count 2,
                                    :nth-child 1}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > H3:nth-child(1) > YTD-BADGE-SUPPORTED-RENDERER:nth-child(1)",
                     :tag "YTD-BADGE-SUPPORTED-RENDERER",
                     :idx 18,
                     :parent-idx 10}
                    {:child-count 0,
                     :text
                                  "\n            Пленки Вовка - новый поворот. Импичмент Тимошенко. Наливайченко и Сакварелидзе топят за Ахметова\n          ",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "H3", :i 1, :child-count 2, :nth-child 1}
                                   {:t "SPAN", :i 2, :child-count 2, :nth-child 2}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > H3:nth-child(1) > SPAN:nth-child(2)",
                     :tag "SPAN",
                     :idx 19,
                     :parent-idx 10}
                    {:child-count 2,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-VIDEO-META-BLOCK", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(1)",
                     :tag "DIV",
                     :idx 20,
                     :parent-idx 11}
                    {:child-count 1,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-VIDEO-META-BLOCK", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DIV", :i 2, :child-count 2, :nth-child 2}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(2)",
                     :tag "DIV",
                     :idx 21,
                     :parent-idx 11}
                    {:child-count 1,
                     :text "",
                     :$
                                  [{:t "DIV", :i 3, :nth-child 3}
                                   {:t "YTD-MENU-RENDERER", :i 1, :child-count 1}
                                   {:t "YT-ICON-BUTTON", :i 2, :child-count 2, :nth-child 2}
                                   {:t "BUTTON", :i 1, :child-count 1}],
                     :_$
                                  "DIV:nth-child(3) > YTD-MENU-RENDERER > YT-ICON-BUTTON:nth-child(2) > BUTTON",
                     :tag "BUTTON",
                     :idx 22,
                     :parent-idx 13}
                    {:child-count 0,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "YTD-THUMBNAIL", :i 1, :child-count 4, :nth-child 1}
                                   {:t "A", :i 1, :child-count 1}
                                   {:t "YT-IMG-SHADOW", :i 1, :child-count 4, :nth-child 1}
                                   {:t "IMG", :i 1, :child-count 1}],
                     :_$
                                  "A:nth-child(1) > YTD-THUMBNAIL:nth-child(1) > A > YT-IMG-SHADOW:nth-child(1) > IMG",
                     :tag "IMG",
                     :idx 23,
                     :parent-idx 14}
                    {:child-count 2,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "YTD-THUMBNAIL", :i 1, :child-count 4, :nth-child 1}
                                   {:t "A", :i 1, :child-count 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-THUMBNAIL-OVERLAY-PLAYBACK-STATUS-RENDERER",
                                    :i 1,
                                    :child-count 4,
                                    :nth-child 1}],
                     :_$
                                  "A:nth-child(1) > YTD-THUMBNAIL:nth-child(1) > A > DIV:nth-child(2) > YTD-THUMBNAIL-OVERLAY-PLAYBACK-STATUS-RENDERER:nth-child(1)",
                     :tag "YTD-THUMBNAIL-OVERLAY-PLAYBACK-STATUS-RENDERER",
                     :idx 24,
                     :parent-idx 15}
                    {:child-count 1,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "YTD-THUMBNAIL", :i 1, :child-count 4, :nth-child 1}
                                   {:t "A", :i 1, :child-count 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-THUMBNAIL-OVERLAY-RESUME-PLAYBACK-RENDERER",
                                    :i 2,
                                    :child-count 4,
                                    :nth-child 2}],
                     :_$
                                  "A:nth-child(1) > YTD-THUMBNAIL:nth-child(1) > A > DIV:nth-child(2) > YTD-THUMBNAIL-OVERLAY-RESUME-PLAYBACK-RENDERER:nth-child(2)",
                     :tag "YTD-THUMBNAIL-OVERLAY-RESUME-PLAYBACK-RENDERER",
                     :idx 25,
                     :parent-idx 15}
                    {:child-count 2,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "YTD-THUMBNAIL", :i 1, :child-count 4, :nth-child 1}
                                   {:t "A", :i 1, :child-count 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-THUMBNAIL-OVERLAY-TIME-STATUS-RENDERER",
                                    :i 3,
                                    :child-count 4,
                                    :nth-child 3}],
                     :_$
                                  "A:nth-child(1) > YTD-THUMBNAIL:nth-child(1) > A > DIV:nth-child(2) > YTD-THUMBNAIL-OVERLAY-TIME-STATUS-RENDERER:nth-child(3)",
                     :tag "YTD-THUMBNAIL-OVERLAY-TIME-STATUS-RENDERER",
                     :idx 26,
                     :parent-idx 15}
                    {:child-count 1,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "YTD-THUMBNAIL", :i 1, :child-count 4, :nth-child 1}
                                   {:t "A", :i 1, :child-count 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-THUMBNAIL-OVERLAY-NOW-PLAYING-RENDERER",
                                    :i 4,
                                    :child-count 4,
                                    :nth-child 4}],
                     :_$
                                  "A:nth-child(1) > YTD-THUMBNAIL:nth-child(1) > A > DIV:nth-child(2) > YTD-THUMBNAIL-OVERLAY-NOW-PLAYING-RENDERER:nth-child(4)",
                     :tag "YTD-THUMBNAIL-OVERLAY-NOW-PLAYING-RENDERER",
                     :idx 27,
                     :parent-idx 15}
                    {:child-count 1,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "H3", :i 1, :child-count 2, :nth-child 1}
                                   {:t "YTD-BADGE-SUPPORTED-RENDERER",
                                    :i 1,
                                    :child-count 2,
                                    :nth-child 1}
                                   {:t "DOM-REPEAT", :i 1, :child-count 1}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > H3:nth-child(1) > YTD-BADGE-SUPPORTED-RENDERER:nth-child(1) > DOM-REPEAT",
                     :tag "DOM-REPEAT",
                     :idx 28,
                     :parent-idx 18}
                    {:child-count 2,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-VIDEO-META-BLOCK", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1)",
                     :tag "DIV",
                     :idx 29,
                     :parent-idx 20}
                    {:child-count 1,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-VIDEO-META-BLOCK", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 2, :nth-child 2}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(2)",
                     :tag "DIV",
                     :idx 30,
                     :parent-idx 20}
                    {:child-count 1,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-VIDEO-META-BLOCK", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DIV", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DOM-REPEAT", :i 1, :child-count 1}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(2) > DOM-REPEAT",
                     :tag "DOM-REPEAT",
                     :idx 31,
                     :parent-idx 21}
                    {:child-count 1,
                     :text "",
                     :$
                                  [{:t "DIV", :i 3, :nth-child 3}
                                   {:t "YTD-MENU-RENDERER", :i 1, :child-count 1}
                                   {:t "YT-ICON-BUTTON", :i 2, :child-count 2, :nth-child 2}
                                   {:t "BUTTON", :i 1, :child-count 1}
                                   {:t "YT-ICON", :i 1, :child-count 1}],
                     :_$
                                  "DIV:nth-child(3) > YTD-MENU-RENDERER > YT-ICON-BUTTON:nth-child(2) > BUTTON > YT-ICON",
                     :tag "YT-ICON",
                     :idx 32,
                     :parent-idx 22}
                    {:child-count 0,
                     :text "ПЕРЕГЛЯНУТО",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "YTD-THUMBNAIL", :i 1, :child-count 4, :nth-child 1}
                                   {:t "A", :i 1, :child-count 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-THUMBNAIL-OVERLAY-PLAYBACK-STATUS-RENDERER",
                                    :i 1,
                                    :child-count 4,
                                    :nth-child 1}
                                   {:t "YT-FORMATTED-STRING", :i 1, :child-count 2, :nth-child 1}],
                     :_$
                                  "A:nth-child(1) > YTD-THUMBNAIL:nth-child(1) > A > DIV:nth-child(2) > YTD-THUMBNAIL-OVERLAY-PLAYBACK-STATUS-RENDERER:nth-child(1) > YT-FORMATTED-STRING:nth-child(1)",
                     :tag "YT-FORMATTED-STRING",
                     :idx 33,
                     :parent-idx 24}
                    {:child-count 1,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "YTD-THUMBNAIL", :i 1, :child-count 4, :nth-child 1}
                                   {:t "A", :i 1, :child-count 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-THUMBNAIL-OVERLAY-PLAYBACK-STATUS-RENDERER",
                                    :i 1,
                                    :child-count 4,
                                    :nth-child 1}
                                   {:t "DOM-REPEAT", :i 2, :child-count 2, :nth-child 2}],
                     :_$
                                  "A:nth-child(1) > YTD-THUMBNAIL:nth-child(1) > A > DIV:nth-child(2) > YTD-THUMBNAIL-OVERLAY-PLAYBACK-STATUS-RENDERER:nth-child(1) > DOM-REPEAT:nth-child(2)",
                     :tag "DOM-REPEAT",
                     :idx 34,
                     :parent-idx 24}
                    {:child-count 0,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "YTD-THUMBNAIL", :i 1, :child-count 4, :nth-child 1}
                                   {:t "A", :i 1, :child-count 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-THUMBNAIL-OVERLAY-RESUME-PLAYBACK-RENDERER",
                                    :i 2,
                                    :child-count 4,
                                    :nth-child 2}
                                   {:t "DIV", :i 1, :child-count 1}],
                     :_$
                                  "A:nth-child(1) > YTD-THUMBNAIL:nth-child(1) > A > DIV:nth-child(2) > YTD-THUMBNAIL-OVERLAY-RESUME-PLAYBACK-RENDERER:nth-child(2) > DIV",
                     :tag "DIV",
                     :idx 35,
                     :parent-idx 25}
                    {:child-count 0,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "YTD-THUMBNAIL", :i 1, :child-count 4, :nth-child 1}
                                   {:t "A", :i 1, :child-count 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-THUMBNAIL-OVERLAY-TIME-STATUS-RENDERER",
                                    :i 3,
                                    :child-count 4,
                                    :nth-child 3}
                                   {:t "YT-ICON", :i 1, :child-count 2, :nth-child 1}],
                     :_$
                                  "A:nth-child(1) > YTD-THUMBNAIL:nth-child(1) > A > DIV:nth-child(2) > YTD-THUMBNAIL-OVERLAY-TIME-STATUS-RENDERER:nth-child(3) > YT-ICON:nth-child(1)",
                     :tag "YT-ICON",
                     :idx 36,
                     :parent-idx 26}
                    {:child-count 0,
                     :text "\n  28:56\n",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "YTD-THUMBNAIL", :i 1, :child-count 4, :nth-child 1}
                                   {:t "A", :i 1, :child-count 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-THUMBNAIL-OVERLAY-TIME-STATUS-RENDERER",
                                    :i 3,
                                    :child-count 4,
                                    :nth-child 3}
                                   {:t "SPAN", :i 2, :child-count 2, :nth-child 2}],
                     :_$
                                  "A:nth-child(1) > YTD-THUMBNAIL:nth-child(1) > A > DIV:nth-child(2) > YTD-THUMBNAIL-OVERLAY-TIME-STATUS-RENDERER:nth-child(3) > SPAN:nth-child(2)",
                     :tag "SPAN",
                     :idx 37,
                     :parent-idx 26}
                    {:child-count 0,
                     :text "Зараз відтворюється",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "YTD-THUMBNAIL", :i 1, :child-count 4, :nth-child 1}
                                   {:t "A", :i 1, :child-count 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-THUMBNAIL-OVERLAY-NOW-PLAYING-RENDERER",
                                    :i 4,
                                    :child-count 4,
                                    :nth-child 4}
                                   {:t "SPAN", :i 1, :child-count 1}],
                     :_$
                                  "A:nth-child(1) > YTD-THUMBNAIL:nth-child(1) > A > DIV:nth-child(2) > YTD-THUMBNAIL-OVERLAY-NOW-PLAYING-RENDERER:nth-child(4) > SPAN",
                     :tag "SPAN",
                     :idx 38,
                     :parent-idx 27}
                    {:child-count 0,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "H3", :i 1, :child-count 2, :nth-child 1}
                                   {:t "YTD-BADGE-SUPPORTED-RENDERER",
                                    :i 1,
                                    :child-count 2,
                                    :nth-child 1}
                                   {:t "DOM-REPEAT", :i 1, :child-count 1}
                                   {:t "TEMPLATE", :i 1, :child-count 1}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > H3:nth-child(1) > YTD-BADGE-SUPPORTED-RENDERER:nth-child(1) > DOM-REPEAT > TEMPLATE",
                     :tag "TEMPLATE",
                     :idx 39,
                     :parent-idx 28}
                    {:child-count 2,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-VIDEO-META-BLOCK", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "YTD-CHANNEL-NAME", :i 1, :child-count 2, :nth-child 1}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1) > YTD-CHANNEL-NAME:nth-child(1)",
                     :tag "YTD-CHANNEL-NAME",
                     :idx 40,
                     :parent-idx 29}
                    {:child-count 0,
                     :text "•",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-VIDEO-META-BLOCK", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 2, :nth-child 2}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1) > DIV:nth-child(2)",
                     :tag "DIV",
                     :idx 41,
                     :parent-idx 29}
                    {:child-count 1,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-VIDEO-META-BLOCK", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DOM-REPEAT", :i 1, :child-count 1}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(2) > DOM-REPEAT",
                     :tag "DOM-REPEAT",
                     :idx 42,
                     :parent-idx 30}
                    {:child-count 0,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-VIDEO-META-BLOCK", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DIV", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DOM-REPEAT", :i 1, :child-count 1}
                                   {:t "TEMPLATE", :i 1, :child-count 1}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(2) > DOM-REPEAT > TEMPLATE",
                     :tag "TEMPLATE",
                     :idx 43,
                     :parent-idx 31}
                    {:child-count 1,
                     :text "",
                     :$
                                  [{:t "DIV", :i 3, :nth-child 3}
                                   {:t "YTD-MENU-RENDERER", :i 1, :child-count 1}
                                   {:t "YT-ICON-BUTTON", :i 2, :child-count 2, :nth-child 2}
                                   {:t "BUTTON", :i 1, :child-count 1}
                                   {:t "YT-ICON", :i 1, :child-count 1}
                                   {:t "svg", :i 1, :child-count 1}],
                     :_$
                                  "DIV:nth-child(3) > YTD-MENU-RENDERER > YT-ICON-BUTTON:nth-child(2) > BUTTON > YT-ICON > svg",
                     :tag "svg",
                     :idx 44,
                     :parent-idx 32}
                    {:child-count 0,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "YTD-THUMBNAIL", :i 1, :child-count 4, :nth-child 1}
                                   {:t "A", :i 1, :child-count 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-THUMBNAIL-OVERLAY-PLAYBACK-STATUS-RENDERER",
                                    :i 1,
                                    :child-count 4,
                                    :nth-child 1}
                                   {:t "DOM-REPEAT", :i 2, :child-count 2, :nth-child 2}
                                   {:t "TEMPLATE", :i 1, :child-count 1}],
                     :_$
                                  "A:nth-child(1) > YTD-THUMBNAIL:nth-child(1) > A > DIV:nth-child(2) > YTD-THUMBNAIL-OVERLAY-PLAYBACK-STATUS-RENDERER:nth-child(1) > DOM-REPEAT:nth-child(2) > TEMPLATE",
                     :tag "TEMPLATE",
                     :idx 45,
                     :parent-idx 34}
                    {:child-count 2,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-VIDEO-META-BLOCK", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "YTD-CHANNEL-NAME", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1) > YTD-CHANNEL-NAME:nth-child(1) > DIV:nth-child(1)",
                     :tag "DIV",
                     :idx 46,
                     :parent-idx 40}
                    {:child-count 0,
                     :text "\n    ",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-VIDEO-META-BLOCK", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "YTD-CHANNEL-NAME", :i 1, :child-count 2, :nth-child 1}
                                   {:t "YTD-BADGE-SUPPORTED-RENDERER",
                                    :i 2,
                                    :child-count 2,
                                    :nth-child 2}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1) > YTD-CHANNEL-NAME:nth-child(1) > YTD-BADGE-SUPPORTED-RENDERER:nth-child(2)",
                     :tag "YTD-BADGE-SUPPORTED-RENDERER",
                     :idx 47,
                     :parent-idx 40}
                    {:child-count 0,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-VIDEO-META-BLOCK", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DOM-REPEAT", :i 1, :child-count 1}
                                   {:t "TEMPLATE", :i 1, :child-count 1}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(2) > DOM-REPEAT > TEMPLATE",
                     :tag "TEMPLATE",
                     :idx 48,
                     :parent-idx 42}
                    {:child-count 1,
                     :text "",
                     :$
                                  [{:t "DIV", :i 3, :nth-child 3}
                                   {:t "YTD-MENU-RENDERER", :i 1, :child-count 1}
                                   {:t "YT-ICON-BUTTON", :i 2, :child-count 2, :nth-child 2}
                                   {:t "BUTTON", :i 1, :child-count 1}
                                   {:t "YT-ICON", :i 1, :child-count 1}
                                   {:t "svg", :i 1, :child-count 1}
                                   {:t "g", :i 1, :child-count 1}],
                     :_$
                                  "DIV:nth-child(3) > YTD-MENU-RENDERER > YT-ICON-BUTTON:nth-child(2) > BUTTON > YT-ICON > svg > g",
                     :tag "g",
                     :idx 49,
                     :parent-idx 44}
                    {:child-count 1,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-VIDEO-META-BLOCK", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "YTD-CHANNEL-NAME", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1) > YTD-CHANNEL-NAME:nth-child(1) > DIV:nth-child(1) > DIV:nth-child(1)",
                     :tag "DIV",
                     :idx 50,
                     :parent-idx 46}
                    {:child-count 1,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-VIDEO-META-BLOCK", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "YTD-CHANNEL-NAME", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "PAPER-TOOLTIP", :i 2, :child-count 2, :nth-child 2}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1) > YTD-CHANNEL-NAME:nth-child(1) > DIV:nth-child(1) > PAPER-TOOLTIP:nth-child(2)",
                     :tag "PAPER-TOOLTIP",
                     :idx 51,
                     :parent-idx 46}
                    {:child-count 0,
                     :text "",
                     :$
                                  [{:t "DIV", :i 3, :nth-child 3}
                                   {:t "YTD-MENU-RENDERER", :i 1, :child-count 1}
                                   {:t "YT-ICON-BUTTON", :i 2, :child-count 2, :nth-child 2}
                                   {:t "BUTTON", :i 1, :child-count 1}
                                   {:t "YT-ICON", :i 1, :child-count 1}
                                   {:t "svg", :i 1, :child-count 1}
                                   {:t "g", :i 1, :child-count 1}
                                   {:t "path", :i 1, :child-count 1}],
                     :_$
                                  "DIV:nth-child(3) > YTD-MENU-RENDERER > YT-ICON-BUTTON:nth-child(2) > BUTTON > YT-ICON > svg > g > path",
                     :tag "path",
                     :idx 52,
                     :parent-idx 49}
                    {:child-count 1,
                     :text "",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-VIDEO-META-BLOCK", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "YTD-CHANNEL-NAME", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "YT-FORMATTED-STRING", :i 1, :child-count 1}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1) > YTD-CHANNEL-NAME:nth-child(1) > DIV:nth-child(1) > DIV:nth-child(1) > YT-FORMATTED-STRING",
                     :tag "YT-FORMATTED-STRING",
                     :idx 53,
                     :parent-idx 50}
                    {:child-count 0,
                     :text "\n      \n        Сергій Лещенко\n      \n    ",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-VIDEO-META-BLOCK", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "YTD-CHANNEL-NAME", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "PAPER-TOOLTIP", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DIV", :i 1, :child-count 1}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1) > YTD-CHANNEL-NAME:nth-child(1) > DIV:nth-child(1) > PAPER-TOOLTIP:nth-child(2) > DIV",
                     :tag "DIV",
                     :idx 54,
                     :parent-idx 51}
                    {:child-count 0,
                     :text "Сергій Лещенко",
                     :$
                                  [{:t "A", :i 1, :nth-child 1}
                                   {:t "DIV", :i 2, :child-count 4, :nth-child 2}
                                   {:t "YTD-VIDEO-META-BLOCK", :i 2, :child-count 2, :nth-child 2}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "YTD-CHANNEL-NAME", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "DIV", :i 1, :child-count 2, :nth-child 1}
                                   {:t "YT-FORMATTED-STRING", :i 1, :child-count 1}
                                   {:t "A", :i 1, :child-count 1}],
                     :_$
                                  "A:nth-child(1) > DIV:nth-child(2) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1) > YTD-CHANNEL-NAME:nth-child(1) > DIV:nth-child(1) > DIV:nth-child(1) > YT-FORMATTED-STRING > A",
                     :tag "A",
                     :idx 55,
                     :parent-idx 53}]

         el-map-2 _el-map-2

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
        ])
     )
   ]

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

       (<plan> (:full-dom-plan (first tweets)))

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