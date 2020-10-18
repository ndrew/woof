(ns woof.client.playground.scraper.cc
  (:require
    [cljs.core.async :as async]
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]

    [goog.dom :as dom]
    [goog.dom.classes :as classes]
    [goog.Uri :as uri]

    [rum.core :as rum]

    [woof.base :as base]
    [woof.data :as d]

    [clj-fuzzy.metrics :as metrics]

    [woof.utils :as utils]

    [woof.browser :as woof-browser]

    [woof.client.dom :as wdom]
    [woof.client.playground.ui :as pg-ui]
    [woof.client.playground.ui.wf :as wf-ui]
    [woof.client.ws :as ws]

    [woof.wfs.evt-loop :as evt-loop]

    [woof.client.playground.scraper.tw :as dashboard]
    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;;
;; UI

(rum/defc <street> < rum/static {
                                 :key-fn (fn [street] (str (:ua street) (:idx street)))}
  [street]

  (let [{ua :ua
         ru :ru
         en :en
         idx :idx
         district :district
         districts :districts
         other :other
         } street]

    [:div.street-row
     {:on-click (fn[e]
                  (.log js/console street)
                  )}
     [:span.idx idx]
     [:span.districts
      ;; [:span.district district]
      (map (fn [d] [:span.district d]) districts)
      ]
     [:div.langs
      [:.ua ua]
      [:.ru ru]
      [:.en en]]
     [:.other
      other
      ]

     (if (:test street)
       [:div {:style {:outline "1px solid red"}}
        (:test street)
        ]
       )
     ]
    #_[:pre.street-row
       (d/pretty! street)
       ]
    )

  )

(rum/defc <streets> < rum/reactive
  [dict *dict]

  [:div
   (pg-ui/menubar "streets"
                  [
                   ["load pre-parsed streets" (fn []
                             (ws/GET "/data/private/streets.edn"
                                     (fn [response]
                                       (let [edn (cljs.reader/read-string response)]
                                         ;; (.log js/console edn)
                                         (swap! *dict assoc :raw-streets edn)
                                         )))
                             )]
                   ])

   [:p "processing stuff via visual repl"]

   (when-let [sorted-raw-data (:yt-sorted dict)]

     (let [selected-filters @(::selected-filters st)
           filter-mode @(::filter-mode st)
           all-filters #{};(reduce #(into %1 %2) #{} (if-let [categories (vals categorizator)] categories [])  )

           sorted-channel-map (into (sorted-map-by (fn [k1 k2]
                                  (let [r (compare (get-in sorted-raw-data [k2 :count]) (get-in sorted-raw-data [k1 :count]))]
                                    (if (= 0 r)
                                      (compare k2 k1)
                                      r))
                                  )) sorted-raw-data)

           match-channel? (fn [k]
                         (let [channel-tags (get categorizator k)
                               matching-tags (clojure.set/intersection selected-filters channel-tags)
                               no-tags? (empty? channel-tags)]
                           (or (= ::all filter-mode)
                               (and (= ::no-category filter-mode) no-tags?)
                               (and (= ::filter filter-mode) (not (empty? matching-tags))))
                           ))
           ]


       [:div

        [:div.filters
         [:.filter.menubar
          [:header ""]
          (concat
            [
             [:a.menu-item
              {:href     "#"
               :on-click (fn [e]
                           (.log js/console categorizator)
                           (.preventDefault e)
                           false)}
              "categorization"
              ]
                           [:a.menu-item
              {:href     "#"
               :on-click (fn [e]

                           (let [roam-data (reduce
                                             (fn [s [k v]]
                                               (if (match-channel? k)
                                                 (str s k
                                                      " #yt-channel"
                                                      (apply str (map-indexed #(str " [🔗" %1 "](" %2 ")") (:channel-url v)))
                                                      (apply str (map #(str " #[[" % "]]") (get categorizator k #{})))
                                                      "\n"
                                                      (apply str
                                                             (map #(str "\t["

                                                                        (->
                                                                          (:title  %)
                                                                          (str/replace #"\[" "❬")
                                                                          (str/replace #"\]" "❭")
                                                                          )
                                                                        "]("

                                                                        (let [id (:id %)
                                                                              full-url (str "https://youtube.com" id)
                                                                              uri (uri/parse full-url)

                                                                              playlist-idx (.getParameterValue uri "index")
                                                                              ;playlist-idx (.getParameterValue uri "index")
                                                                              ]
                                                                          (->
                                                                            uri
                                                                            (.removeParameter "index")
                                                                            (.removeParameter "list")
                                                                            (.toString)
                                                                            )
                                                                          )


                                                                        ")" "\n") (:items v))
                                                             )
                                                      )
                                                 s
                                                 )
                                               )
                                             ""
                                             sorted-channel-map
                                             )]
                             (.log js/console roam-data)
                             )

                           (.preventDefault e)
                           false)}
              "EXPORT TO ROAM"]

             [:.separator]

             [:a.menu-item
              {:href     "#"
               ;;:class    (filter-class filter-id)
               :on-click (fn [e]
                           (swap! (::filter-mode st) {::all          ::no-category
                                                      ::no-category  ::filter
                                                      ::filter       ::all})
                           (.preventDefault e)
                           false)}
              (str (name filter-mode))]
             [:.separator]

             ]

            (if (= ::filter filter-mode)
              (map (fn [filter-id]
                     (let [selected? (get selected-filters filter-id )]
                       [:a.menu-item
                        {:href     "#"
                         ;;:class    (filter-class filter-id)
                         :on-click (fn [e]
                                     (swap! (::selected-filters st)
                                            (if selected? disj conj)
                                            filter-id)
                                     (.preventDefault e)
                                     false)}
                        (str (pg-ui/shorten-bool selected?) " " (name filter-id))]
                       )
                     )
                   (sort all-filters))
              [])

            )

          ]
         ]

      ;[:.html (pr-str (reduce #(into %1 %2) #{} (vals categorizator)))]

        ;; show all videos as list
        #_(map <video>
             (->>
               sorted-raw-data
               (vals )

               (map :items )
               (apply concat )
               (sort-by :i )
               (reverse )
               )
             )

        ;; show videos by channel
        (map (fn [[k v]]
               (if (match-channel? k)
                 (<channel> categorizator k v)))
             sorted-channel-map)
      ]
       )
     )



   (when-let [raw-yt (:yt-raw dict)]
     (let [gm (group-by :channel-href (take 5000 raw-yt))
           count-m (reduce-kv (fn [m k v]
                                (let [nu-k (first (reduce #(conj %1 (:channel-title %2)) #{} v))]

                                  (if (nil? nu-k)
                                    (do
                                      (.log js/console "possibly deleted/private video" v)
                                      m)
                                    (assoc m nu-k
                                             (-> (get m nu-k {:count 0
                                                              :channel nu-k
                                                              :channel-url #{}
                                                              :items []
                                                              })
                                                 (update :count + (count v))
                                                 (update :items concat
                                                         (map
                                                           #(let [full-url (str "https://youtube.com" (:id %))
                                                                  uri (uri/parse full-url)

                                                                  playlist-idx (.getParameterValue uri "index")]
                                                              (assoc % :i (int playlist-idx))
                                                              )
                                                           v)

                                                         )
                                                 (update :channel-url conj k)
                                                 )
                                             )
                                    )

                                  )
                        ) {} gm)

           sorted-m (into (sorted-map-by (fn [k1 k2]
                                               (let [r (compare (get-in count-m [k2 :count]) (get-in count-m [k1 :count]))]
                                                 (if (= 0 r)
                                                   (compare k2 k1)
                                                   r)
                                                 )

                                  )) count-m)

           ]

       [:div

        [:header
         (pr-str [(count gm) " vs " (count count-m) " vs " (count sorted-m)])
         ]

        (pg-ui/menubar "" [["save edn" (fn []
                                 (wdom/save-edn sorted-m)
                                 )]
                           ["save to ui" (fn []
                                           (swap! *dict assoc :yt-sorted sorted-m))]
                           ])

        #_[:.html (d/pretty! (keys sorted-m))]
        ;()

        (map
          (fn [[k v]]
            (<channel> categorizator k v))
          ;(sort-by-value-then-key count-m)
          sorted-m
          )



        #_[:.html (d/pretty!
                  (into (sorted-map-by >)
                      count-m)

                  )]

        ]

       )


     )

   (when-let [raw-blago (:blago-raw dict)]
     (map <blago-listing> raw-blago)
     )



   ]
  )



(rum/defcs <scraping-root> < rum/reactive
                             (rum/local {} ::inline-results?)
  [local *wf]

  (let [wf @*wf]
    [:div
     (if (= :not-started (:status wf))
       [:div "wf is not running"]

       ;; for now hadcode actions for ::current

       (let [*dict (rum/cursor-in *wf [:state ::dict])
             *categorizator (rum/cursor-in *wf [:state ::categorizator])


             ]
         [:div {:style {:padding "1rem"}}

          ;; streets

          ; [:pre (pr-str @*dict)]

          (pg-ui/<edn-list> (get-in wf [:state ::dict ::renamings]  []) "")

          #_[:pre
           (d/pretty! (get-in wf [:state ::dict ::renamings]  []))
           ]


          (pg-ui/menubar "" [["copy extracted renamings" (fn []
                                                           ; (wdom/copy-to-clipboard)
                                                           )]])


          #_(let [dom-nfos (get-in wf [:state ::dict ::els]  [])]
            (<extract-renaming>
              [(rand-nth dom-nfos) (rand-nth dom-nfos)]
              ;;dom-nfos
              )
            )


          ;;(<streets-cc> *dict)

          ;;(<cc> *dict *categorizator)

          ]
         )

       )
     ]
    )
  )

(defn expand-limited [step-id n ]
  {
   :fn (fn [els]
         (reduce (fn [a e] (assoc a (base/rand-sid) [step-id e]))
                 {}
                 (take n els)))
   :expands? true
   }
  )


(defn parse-ctx [params]
  {

   :process*  (base/expand-into :process) ;; (expand-limited :process 5)
   :process  {
              :fn (fn [el]

                    (let [*dict (::*dict params)
                          el-map (wdom/el-map el
                                              ;:skip-fn skip-fn
                                              :node-fn wdom/enrich-node
                                              :top-selector-fn (fn [base el] {:nth-child (:i base)}))

                          district (str/replace
                                     (wdom/txt (wdom/q el "DIV:nth-child(1) > DIV > H3:nth-child(1) > STRONG"))
                                     "район" "р-н"
                                     )
                          r1 (map wdom/txt (wdom/query-selector* el "TABLE  TR  > TD:nth-child(1)"))
                          r2 (map wdom/txt (wdom/query-selector* el "TABLE  TR  > TD:nth-child(2)"))

                          dom-nfo {
                                   :id     district
                                   :el-map el-map
                                   :nodes  (reduce (fn [a node] (assoc a (:_$ node) node)) {} el-map)
                                   }
                          ]

                      (swap! *dict assoc ::els [dom-nfo])


                      {
                       :renamings (apply hash-map (interleave r1 r2)) ;; (into (sorted-map) (partition 2 (interleave r1 r2)))
                       :district district
                       }

                      )

                    )
              }
   :save-renamings {
                    :fn (fn [v]
                          (let [*dict (::*dict params)
                                rename-map (reduce (fn [a z]
                                                     (assoc a (:district z)
                                                              (:renamings z))
                                                     ) {} v)
                                ]
                            (swap! *dict assoc ::renamings rename-map)
                            v
                            )
                          )
                    :collect? true
                    }
   }
  )

(defn parse-renamings-steps [params]
  {
   :renaming/__els* [:query-selector-all ".columnLayout.two-left-sidebar"]
   :renaming/parsed* [:process* :renaming/__els*]

   ::log [:&log :renaming/parsed*]

   ::save! [:save-renamings :renaming/parsed*]
   }
  )


;;
;; WF definition
(defn wf! [*SWF]


  (let [CHAN-FACTORY (base/chan-factory (atom {}))
        *dict (rum/cursor-in *SWF [:state ::dict])
        *categorizator (rum/cursor-in *SWF [:state ::categorizator])
        ]
    {

     :title       "Scraping command center"
     :explanation [:div.explanation
                   [:p "Analyze scraped data here"]]

     ;; this state will be added to a wf?
     :state {

             ::categorizator {}

             ;; just data for the ui
             ::dict {
                     :streets []
                     :raw-streets []
                     }

             }

     :init-fns    [

                   (fn [params]
                     (ws/GET "http://localhost:9500/s/yt/channel-tags-mapping.edn" (fn [data]
                                                                                     (reset! *categorizator data)))

                     ;; todo: maybe returning channel and proceeding with init when the data is ready?
                     {
                      ::categorizator *categorizator
                      ::*dict *dict
                      }
                     )

                   (base/build-init-chan-factory-fn CHAN-FACTORY)
                   (evt-loop/build-evt-loop-init-fn (base/make-chan CHAN-FACTORY (base/rand-sid "evt-")))
                   ]
     ;;
     :ctx-fns     [
                   evt-loop/evt-loop-ctx-fn

                   woof-browser/common-ctx ;; re-use common browser step handlers
                   wdom/dom-ctx

                   parse-ctx
                   ]

     ;;
     :steps-fns   [(fn [params]
                     {
                      ::#EVT-LOOP# [:evt-loop (evt-loop/&evt-loop params)]

                      ;; ::hello [:log "Hello"]
                      ;; :CSS/custom-css-file [:css-file "http://localhost:9500/css/t.css"]
                      })
                   parse-renamings-steps
                   ]

     :opt-fns     [
                   (base/build-opts-chan-factory-fn CHAN-FACTORY)
                   ;; uncomment to display wf results (esp. if there was an error)
                   (base/build-opt-on-done (fn [params result]
                                             (.warn js/console params result)))
                   ]

     :ui-fn       (partial wf-ui/<wf-UI> (partial <scraping-root>))

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
