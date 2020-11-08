(ns woof.client.playground.ui.html
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

;; data

(defn img-scrape [n]
  (select-keys n #{:img-src}))

(defn link-scrape [n]
  (select-keys n #{:href}))

(defn text-scrape [n]
  (select-keys n #{:text}))


(def filter-scraping-rules
  {
   :img? img-scrape
   :link? link-scrape
   :text? text-scrape
   })


(def filters-map
  (assoc (sorted-map)
    :img? #(= "IMG" (:tag %))
    :link? #(= "A" (:tag %))
    :text? #(not= "" (str/trim (:text %)))
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


(defn selector-usage [nodes]
  (reduce
    (fn [a node]
      (reduce (fn [b sl]
                (update-in b [sl] (fnil conj #{}) (:id node))
                ) a
              (map #(get % :_$) (:el-map node))))
    (sorted-map) nodes))



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

;; ui

(defn _def-key-fn [prefix cfg] (str (str/join "|" (::ids cfg)) "_" prefix ))
(defn tree-key-fn [cfg k]      (str (first (::ids cfg)) "_" k))

(defn filter-class [filter-k]  (str "filter__" (str/replace (name filter-k) "?" "\\?")))


(rum/defcs <selector> < rum/static
                        {:key-fn (fn [cfg s] (tree-key-fn cfg s))}
  [st cfg short-selector node]
  (let [filter-info      (get-in cfg [:filter-info (:idx node)])
        applied-filters  (:applied-filters filter-info)
        selected?        (not (empty? applied-filters))
        existing-classes (get-in node [:attrs "class"] "")
        $tags    [:.tags (map #(pg-ui/<tag> (str "small-tag filter-tag "
                                               (if (applied-filters %) "applied-tag" ""))
                                         (str %)) (:matching-filters filter-info))]
        ]
    (if selected?
      [:header.flex
       [:.selector short-selector " ___(" existing-classes ")"]
       $tags]
      [:.short.flex
       [:.selector short-selector]
       $tags
       ]
      )
    )
  )

(defn tag-key-fn [cfg node filter-id]
  (str
    (first (::ids cfg)) "_"
    (get cfg :node/prefix "")
    "_" (:idx node)
    "_" (name filter-id)
    )
  )

(rum/defc <tag> < rum/static {:key-fn tag-key-fn}
  [cfg node filter-id]

  (let [applied-filters (get node :applied-filters #{})]
    (pg-ui/<tag> (str "small-tag " "filter-tag "
                      (if (applied-filters filter-id) "applied-tag" ""))
                 (str filter-id))
    )
  )


(defn plan-header-key-fn [cfg *details? m ]
  (str
    (first (::ids cfg)) "_"
    (get cfg :node/prefix "")
    "_" (str @*details?)
    "_" (:idx  m)))


(rum/defc <plan-header> < rum/static
                          {:key-fn plan-header-key-fn}
  [cfg *details? node]
  (let [parent-selector (get cfg :node/parent-selector "")
        selector (:_$ node)
        partial-selector (wdom/shorten-selector-string selector parent-selector)
        details? @*details?
        idx (:idx node)
        exclusions      (get cfg :filter/exclusions #{})
        excluded?       (exclusions selector)
        aliases         (get cfg :selector/aliases {})
        analysis-nodes  (get cfg :selector/analysis {})
        analyzed?       (analysis-nodes selector)

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
                                       )))]]
        ]

    [:div.plan-header
     (pg-ui/menubar "" [[(if details? "▿" "▹") (fn [] (swap! *details? not))]]
                    :class "minimal" )


     (if-let [alias (get aliases selector)]
       [:span.tag.alias (pr-str alias)])

     [:span.small-tag.tag.selector partial-selector]

     (if details?
       (pg-ui/menubar ""
                      (conj quick-menu
                            []
                            ["📋full $"    (fn [] (wdom/copy-to-clipboard selector))]
                            ["📋partial $" (fn [] (wdom/copy-to-clipboard partial-selector))] ))
       (pg-ui/menubar "" quick-menu))

     [:.tags (map #(<tag> (assoc cfg
                            :node/prefix "tag_") node %) (:matching-filters node))]

     [:span.small-tag.tag.idx (str idx)
      [:sup (str (:parent-idx node))]]
     ]
    )
  )

(defn node-key-fn [cfg m]
  (str
    (first (::ids cfg)) "_"
    (get cfg :node/prefix "")
    (:idx  m)
    ; "_" (str/join (sort (keys m)))
    )
  )


(rum/defcs <node> < rum/static (rum/local false ::details?)
                    {:key-fn node-key-fn}
  [st cfg node]

  (let [curr-tag (:tag node)
        details? @(::details? st)
        applied-filters (get node :applied-filters #{})

        selector (:_$ node)

        exclusions (get cfg :filter/exclusions #{})
        excluded? (exclusions selector)

        used-by (get (::usage cfg) selector #{})
        node-id (first (::ids cfg))
        used-by-others (disj used-by node-id)]

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
      (if details? [:.html (d/pretty! node)])
      ]
     ]
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


(rum/defc <plan> < rum/static
                   {:key-fn (partial _def-key-fn "<plan>")}
  [cfg node]

  (let [plan (:el-map node)
        mode (:root-UI/mode cfg)

        full-tree-plan (wdom/el-plan-as-tree plan)
        ]

    [:.plan-root.flex

     (if (#{::all ::tree} mode)
       (<tree-plan> cfg plan full-tree-plan))

     (if (#{::all ::plan} mode)
       (<full-plan>    cfg plan))
     ]
    )
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