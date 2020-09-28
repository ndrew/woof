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

(rum/defcs <node> < rum/static
                    (rum/local false ::details?)
                   {:key-fn (fn [m] (:_$  m))}
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

(rum/defcs <tree-ui> < rum/static
  [st plan tree-plan]
  (let [selected-idxs (reduce (fn [a n] (conj a (:idx n))) #{} plan)
        ]

    [:div.tree-root
     [:header "TREE:"]

     (map (fn [item] (<v> {:selected selected-idxs} nil item)) tree-plan)

     ]
    )


  )

(rum/defcs <h-plan> < rum/static
                      ; (rum/local {} ::inline-results?)
  [st fltr _plan]

  [:.plan-root

   [:div.filters
     "I am a filter"
    ]

   (let [

         plan (vec (filter fltr _plan))

         tree-plan (wdom/el-plan-as-tree _plan)
         ]

     [:.plan-box.flex

      (<tree-ui> plan tree-plan)


      #_(let [gr (group-by :parent-idx plan)
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




      [:div
       [:header "FULL PLAN: " (str (count plan)) ]
       (map <node> plan)
       ]
      ]
     )
   ]
  )


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


   (let [skip-fn (fn [$el $] (#{"SVG" "G" "PATH"} (str/upper-case (.-tagName $el))))

         el-map-1 (wdom/el-map (wdom/q "#html")
                               :skip-fn skip-fn
                               :node-fn wdom/enrich-node)

         el-map-2 (wdom/el-map (wdom/q "#html1")
                               :skip-fn skip-fn
                               :node-fn wdom/enrich-node
                               )
         node-filter (fn [node]
           (let [text? (not= "" (:text node))
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

           )
         ]
     [:div.flex
      (let [sl1 (into (sorted-set) (map #(get % :_$) el-map-1))
            sl2 (into (sorted-set) (map #(get % :_$) el-map-2))

            prev (volatile! "")
            ]

        [:div


         [:table.selector-diff
          [:tr
           [:th "$" ]
           [:th "A" ]
           [:th "B" ]
           ]
          (map (fn [k]
                 (let [in-a (get sl1 k)
                       in-b (get sl2 k)]
                   [:tr (if (and in-a in-b)
                          {:class "match-both"}
                          (if in-a
                            {:class "match-a"}
                            {:class "match-b"}
                            )
                          )
                    [:td

                     (let [short-$ (str/trim (str/replace k @prev ""))]
                       (vreset! prev k)

                       short-$
                       )



                     ]
                    [:td (pg-ui/shorten-bool in-a) ]
                    [:td (pg-ui/shorten-bool in-b)]
                    ]

                   )

                 )
               (concat sl1 sl2)
               )
          ]




         ;[:code (d/pretty! sl1)]
         ;[:code (d/pretty! sl2)]

         ]

        )
       (<h-plan> node-filter el-map-1)
       (<h-plan> node-filter el-map-2)
      ]

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





(rum/defcs <wf-root> < rum/reactive
                       (rum/local {} ::inline-results?)
  [local *wf]

  (let [wf @*wf]
    [:div
     (if (= :not-started (:status wf))
       [:div "wf is not running"]

       (let [*state (rum/cursor-in *wf [:state])]
         [:div.example-tw
          (<tw> (get-in wf [:state]) *state)
          ;(<tw-real> (get-in wf [:state]) *state)
          ]
         )

       )
     ]
    )
  )



;;
;; WF definition
(defn wf! [*SWF]
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
