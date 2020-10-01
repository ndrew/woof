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



(rum/defc <v> < rum/static
  [cfg parent plan]

  (let [selector (:_$ plan)
        parent-selector (if parent (:_$ parent) "")
        selected (get cfg :selected #{})
        selected? (get selected (:idx plan))
        ch (get plan :children [])
        has-children? (seq ch)
        ]
    [:.plan (cond
              (and (not has-children?) selected?) {:class "selected leaf"}
              selected? {:class "selected"}

              )
     (if selected?
       [:header
          [:div (str/trim (str/replace selector parent-selector ""))]
          (:text plan)]
       [:span (str/trim (str/replace selector parent-selector ""))]
       )

     (if has-children?
       (map (fn [child] (<v> cfg
                          plan
                          child))
            ch))
     ]
    )

  )


(defn def-key-fn [cfg] (str/join "|" (::ids cfg)))


(rum/defc <tree-ui> < rum/static
                      {:key-fn def-key-fn}
  [cfg plan tree-plan]

  (let [selected-idxs (reduce (fn [a n] (conj a (:idx n))) #{} plan)]

    [:div.tree-root
     [:header "TREE:"]

     [:pre (d/pretty! selected-idxs)]

     (map (fn [item] (<v> {:selected selected-idxs} nil item)) tree-plan)

     ]
    )


  )

(rum/defcs <node> < rum/static
                    (rum/local false ::details?)
                    {:key-fn (fn [m]
                               (str
                                 (:_$  m)
                                 "_"
                                 (str/join (sort (keys m))))

                               )}
  [st node]

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
        [:code (d/pretty! node)]
        ]
       )
     ]
    )

  )


(defn cfg+id [cfg id]
  (update-in cfg [::ids] conj id)
  )


(rum/defc <grouped-plan> < rum/static
                           {:key-fn def-key-fn}
  [cfg filtered-plan]
  (let [gr (group-by (fn [node] (dec (:parent-idx node))) filtered-plan)
        roots (sort (keys gr))]
    [:div
     [:header "PLAN GROUPED BY PARENT IDX:"]
     (map
       (fn [root]
         [:.plan
          [:header (pr-str root)]

          (map (fn [ch]
                 [:.plan
                  (pr-str (wdom/to-selector (:$ ch)))
                  ])
               (get gr root))
          ]
         )

       roots
       )
     ]
    )
  )


(rum/defc <full-plan> < rum/static
                           {:key-fn def-key-fn}
  [cfg filtered-plan]

  [:div
   [:header "FULL PLAN: " (str (count filtered-plan)) ]
   (map <node> filtered-plan)
   ]
  )


(rum/defc <h-plan> < rum/static
                     {:key-fn def-key-fn}
  [cfg node]

  (let [_plan (:el-map node)
        filter-fn (:node/filter-fn cfg)
        ]
    [:.plan-root

     (let [filtered-plan (vec (filter filter-fn _plan))
           full-tree-plan (wdom/el-plan-as-tree _plan)
           ]

       [:.plan-box.flex

        (<tree-ui>      cfg filtered-plan full-tree-plan)

        (<grouped-plan> cfg filtered-plan)

        (<full-plan>    cfg filtered-plan)
        ]
       )
     ]
    )
  )

;;;;;;;;;;;;;;;;;;;
;; yt


(rum/defc <comparison-row> < rum/reactive
                             {:key-fn (fn [k s _ _ _ _]
                                        ;; (.log js/console (str k "_" s))
                                        (str k "_" s))}
  [k short-k in-a in-b el-1 el-2]
  [:tr  (if (and in-a in-b)
         {:class "match-both"}
         (if in-a
           {:class "match-a"}
           {:class "match-b"}
           )
         )
   [:td
    short-k
    ]
   [:td (pg-ui/shorten-bool in-a) ]
   [:td
    (if-let [node (get el-1 k)]
      (<node> node))
    ]
   [:td (pg-ui/shorten-bool in-b)]
   [:td
    (if-let [node (get el-2 k)]
      (<node> node))
    ]
   ]
  )


(rum/defcs <comparison> < rum/reactive
                          {:key-fn def-key-fn}
                          (rum/local ::all ::show)
  [st cfg nodes]

  (let [
        ;; el-1 el-2 sl1 sl2

        {el-map-1 :el-map
         el-1     :nodes
         } (first nodes)
        {
         el-map-2 :el-map
         el-2     :nodes
         } (second nodes)

        sls (map
              (fn [node]
                (into (sorted-set)
                      (map #(get % :_$) (:el-map node)))
                )
              nodes
              )

        [sl1 sl2] sls

        *prev (volatile! "")
        show @(::show st)]
    [:div

     (pg-ui/menubar "" [
        [(str @(::show st))
         (fn []
           (swap! (::show st)
                  {::all ::different
                   ::different ::all}
                  )
           )
         ]
        ]
       )


     (if (= show ::different)
       [:div ;; different tag in order to be properly rendered
        #_(let [items (reduce (fn [a k]
                              (let [in-a (get sl1 k)
                                    in-b (get sl2 k)
                                    short-$ (let [short-$ (str/trim (str/replace k @*prev ""))]
                                              (vreset! *prev k)
                                              short-$)
                                    ]
                                (if (or (nil? in-a) (nil? in-b))
                                  (conj a (<comparison-row> k short-$ in-a in-b el-1 el-2))
                                  a)
                                )
                              )
                            []
                            (concat sl1 sl2)
                            )
              table (into [:table.selector-diff
                           [:tr
                            [:th "$" (str (count items)) ]
                            [:th "A" ]
                            [:th "A v"]
                            [:th "B" ]
                            [:th "B v"]
                            ]]
                          items
                          )
              ]

          table
          )
        ]
       ;; else

       [:table.selector-diff

        [:tbody
         [:tr [:th "$"]
          (map (fn [node]
                 [:th {:col-span 2} (str (:id node))]) nodes)]

         ;; items



         (loop [res []
                ks (apply concat sls)
                prev-k ""
                parent-k (first (first sls))
                prev-margin 0
                ]
           (let [
                 k (first ks)
                 nu-ks (rest ks)

                 short-k (str/trim (str/replace k prev-k ""))
                 nu-margin (if (= k short-k)
                             0
                             (+ prev-margin 1)
                             )

                 ]

             (if (seq nu-ks)
               (recur (conj res
                            [:tr
                             {
                              :class (if (= short-k k)
                                       "not-child-row"
                                       "child-row")
                              }
                             [:td
                              {
                               :class (str "lpad-" nu-margin)
                               }
                              short-k]
                             [:td

                              (if (= k short-k)
                                (pr-str [parent-k k ])

                                )


                              ]
                             [:td "A"]
                             [:td "2"]
                             [:td "B"]
                             ])
                      nu-ks
                      k
                      parent-k
                      nu-margin

                      )
               res
               )
             )

           )

         ]


        ]

       )

     ]
    )
  )

(rum/defcs <dashboard> < rum/static
                         (rum/local {
                                     ::ids []

                                     :root-UI/show ::all
                                     :root-UI/overflow? true

                                     :node/filter-fn (fn [node]
                                                       #_(let [text? (not= "" (:text node))
                                                             ;no-children? (< (:child-count node) 2)
                                                             ;div? (= "DIV" (.-tagName (:el node)))
                                                             link? (= "A" (:tag node))
                                                             img? (= "IMG" (:tag node))
                                                             ]
                                                         #_(not (and no-text?
                                                                     ;no-children?
                                                                     ;div?
                                                                     ))
                                                         ;(.log js/console node)
                                                         (or link?
                                                             img?
                                                             text?)
                                                         )
                                                       true
                                                       )
                                     ;; todo filter
                                     } ::cfg)
  [st nodes]

  (let [

        *cfg (::cfg st)
        cfg  @(::cfg st)

        show (get cfg :root-UI/show)

        show-UI [(str (name show))
         (fn []
           (swap! *cfg update-in [:root-UI/show] {::all        ::comparison
                                                  ::comparison ::nodes
                                                  ::nodes      ::all}))
         ]

        overflow? (get cfg :root-UI/overflow?)
        overflow-UI [(str "overflow " (pg-ui/shorten-bool overflow?)) (fn [] (swap! *cfg update-in [:root-UI/overflow?] not))]
        ]

    [:div.scrape-ide

     (pg-ui/menubar "" [show-UI overflow-UI])

     [:hr]

     [:ul
      (map (fn [n]
             [:li
              [:header (pr-str (:id n))]
              [:div (.-outerHTML (.-parentElement (get-in (:el-map n) [0 :el])))]
              ;[:pre (d/pretty! n)]
              ]
             ) nodes)
      ]

     [:div.flex {:class (if overflow? "overflow-x" "")}

      #_(if-not (= ::nodes show)
        (<comparison> cfg nodes)
        )

      (if-not (= ::comparison show)
        (map #(<h-plan>
                (update-in cfg [::ids] conj (:id %))
                %)
             nodes)
        )
      ]
     ]
    )
  )

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
        ]
       )

     )




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
       )]

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