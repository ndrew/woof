(ns woof.client.playground.wf.listing
  (:require
    [cljs.core.async :as async]
    [cljs.reader]

    ;; why this gives a warning?
    [goog.net.XhrIo :as xhrio]

    [rum.core :as rum]

    [woof.client.stateful :as st-wf]
    [woof.client.playground.ui :as ui]
    [woof.client.playground.ui.wf :as wf-ui]

    [woof.base :as base]
    [woof.data :as d]
    [woof.utils :as u]
    [woof.wf :as wf]
    )
  (:import [goog.net.XhrIo])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



(defn GET [url handler]
  (xhrio/send url (fn [event]
                  (let [response (.-target event)]
                       (handler (.getResponseText response)))
                  ))
  )

;; CTX
(defn listing-ctx-fn [params]
  {
   ; network
   :GET  {:fn (fn [url]
                (let [chan (base/make-chan (base/&chan-factory params) (wf/rand-sid "GET-"))]
                     (GET url (fn [response]
                                (let [edn (cljs.reader/read-string response)]
                                     ;; (.log js/console edn)
                                     (async/put! chan edn)
                                     )
                                ))
                     chan
                     )
                )
          }

   ; common

   :v {:fn (fn [v] v)}

   :log  {:fn (fn [v]
                (.log js/console v)
                v)}



   }
  )


(defn avg [coll]
  (/ (reduce + coll) (count coll)))

(defn brand-grouping
  ([] {:markName {}})
  ([row] [:markName (:markName row)])
  ([row aggr]
   (if aggr
     (-> aggr
         (update-in [:idxs] conj (::curr-idx row))
         ; (update :count inc)
         )
     {
      :idxs  [(::curr-idx row)]
      }
     )
   )
  )

(defn model-grouping
  ([] {})
  ([row]
   [(:modelName row)])
  ([row aggr]
   (if aggr
     (->
       aggr
       (update-in [:idxs]  conj (::curr-idx row))
       (update-in [:prices] conj (:mainPrice row))
       )
     {
      :prices [(:mainPrice row)]
      :idxs   [(::curr-idx row)]
      })
   ))

(defn model-grouping-1
  ([] {:modelName {}})
  ([row]
   [:modelName (:modelName row)])
  ([row aggr]
   (if aggr
     (->
       aggr
       (update-in [:idxs]  conj (::curr-idx row))
       (update-in [:prices] conj (:mainPrice row))
       )
     {
      :prices [(:mainPrice row)]
      :idxs   [(::curr-idx row)]
      })
   ))

(defn location-grouping
  ([]
   {:location (sorted-map)})
  ([row]
   [:location (:location row)])
  ([row aggr]
   (if aggr
     (do
       (->
         aggr
         (update-in [:idxs] conj (::curr-idx row))
         )
       )
     {
      :idxs [(::curr-idx row)]
      }
     )
   )
  )

(defn group-session-data [joined-data grouppers]

  ;; grouping to nested maps
  (let [

        *i (volatile! -1) ;;


        sub-aggr (fn [row+state aggr top-ks group-f]
                   (let [aggr-1 (merge (group-f) aggr)

                         ks-1 (group-f row+state)
                         ks-full (vec (concat top-ks (group-f row+state)))
                         ]

                        (let [zzz (update-in aggr-1
                                             ks-1
                                             (fn [v]
                                               (group-f row+state v)))]

                         ;;  (.warn js/console zzz)
                          zzz
                          )


                        )
                   )

        top-groupper (first grouppers)
        rest-grouppers (rest grouppers)

        full_stats (reduce
                     (fn [a row]

                       ;(.log js/console "update-in: " (brand-grouping row))

                       (let [top-ks (top-groupper row)
                             upd-result (update-in a (top-groupper row)
                                                   (fn [v]
                                                     ;; enrich state
                                                     (let [row+state (assoc row
                                                                       ::curr-idx (vswap! *i inc)
                                                                       )]

                                                          (let [aggr (top-groupper row+state v)]
                                                            (reduce (fn [nu-aggr group-f]
                                                                      (let [tmp (sub-aggr row+state nu-aggr top-ks group-f)]
                                                                           tmp
                                                                           )


                                                                      )
                                                                    aggr rest-grouppers)

                                                            )


                                                          )

                                                     ))]
                            ;(.log js/console "=>" upd-result)

                            upd-result
                            )

                       )
                     (top-groupper)
                     joined-data)


        ;; todo: sorting step, if needed
        brand-model (:markName full_stats)
        stats (assoc full_stats
                :markName (into (sorted-map-by (fn [k1 k2]
                                                 (compare
                                                   (:count (get brand-model k2))
                                                   (:count (get brand-model k1))
                                                   )
                                                 ))
                                brand-model)
                )

        ]

    full_stats
    ;stats
    )

  )

(defn prepare-aria-session [full-session]
  (let [session (get full-session "auto.ria.com")

        joined-data (reduce (fn [a d]
                              (concat a (:data d))
                              ) [] (:data session))
        ;; do we need to concat summaries also?
        ]

      {
       :all-data (vec joined-data)

       :groupped-data (group-session-data
                        joined-data
                        [
                         brand-grouping
                         model-grouping
                         ;; location-grouping
                         ]

                        #_[
                           {:markName "A" :modelName "MN" :location "AAA"}
                           {:markName "A" :modelName "MN" :location "BBB"}
                           {:markName "A" :modelName "MN" :location "AAA"}
                           ]
                        ;(filter #(= "Volkswagen" (:markName %) ) joined-data)
                        )

       :groupping [:tbd]
       }
    )
  )




;; todo: define grid by specifying simple grid config
(defn prepare-aria-columns [_]
  [

   [:id (fn [v]
      [:a {:href
                   (if-let [l (:linkToView v)]
                     (str "https://auto.ria.com" l)
                     (:link2View v))
           :target "_blank"} (pr-str (:id v))]
      )
    ]
   [:car
    (fn [v]

      (if-let [t (:title v)]
              t
              (str
                (:markName v) " " (:modelName v) " " (:year v) " " (:location v)
                )
              )
      )
    ]

   [:price
    (fn [v]
      (if-let [main-price (:mainPrice v)]
              (str main-price " " (:mainCurrency v))

              (if (js/isNaN (:USD v))
                (str (.toFixed (/ (:UAH v) 27) 2) " USD")
                (str (:USD v) " USD")
                )

              )

      )
    ]

   [:mileage]
   [:fuel]
   [:akp (fn [v]
           (if-let [akp (:akp v)]
                   (pr-str akp)
                   (pr-str (:transmission v))
                   )
           )]
   [:desc]

   [:dbg
    (fn [v]
      (let [clean-up-v (dissoc v
                               :id :label :title :advertisementData :template_name :akp :desc :fuel :level :levelExpireDate :link2View :linkToView :location :mainCurrency :mainPrice :markName :mileage :modelName :transmission :UAH :USD :userId :year
                               ;:addDate
                               ;:expireDate
                               ;:updateDate
                               ; :paid?
                               ;:raw-updated
                               )]
           (d/pretty! clean-up-v))
      )
    ]

   ]

  )


(declare stateful-init!)

(defn initialize! [*WF-STATE-MAP]
  (merge
    {
     :init-fns  [

                 (fn [params]
                   {
                    ::listings-url "http://localhost:9500/data/auto-ria.edn"
                    }
                   )
                 ;; todo: add event loop for handling ui events (for filters), but now start without it
                 ;init-evt-loop

                 ;; channel factory
                 st-wf/chan-factory-init-fn
                 ]

     :ctx-fns   [
                 listing-ctx-fn
                 (fn [params]
                   {
                    :prepare-session {:fn prepare-aria-session}
                    :column-cfg {:fn prepare-aria-columns}
                    }
                   )
                 ]

     :steps-fns [(fn [params]
                   {
                    :scraping/raw-session  [:GET (::listings-url params)]
                    :scraping/session      [:prepare-session :scraping/raw-session]

                    :grid/columns      [:column-cfg nil]

                    :data/session     [:v :scraping/session]
                    })

                 ]

     :opt-fns   [;; ls/ls-opts-fn
                 st-wf/chan-factory-opts-fn
                 ]

     }
    (stateful-init! *WF-STATE-MAP)
    ))


;; UI

(rum/defc <scraping-table> < rum/static
  [joined-data ks]
  (let [ths (reduce (fn [a [k]]
                      (conj a
                            [:th (str k)]
                            )
                      )
                    [:tr] ks
                    )]

    [:div


     [:table.auto-ria-results {:border "1"}
      ths

      (map (fn [d]
             (reduce (fn [a [k f]]
                       (conj a
                             [:td
                              (if-not f
                                (d/pretty! (get d k))
                                (f d)
                                )
                              ]
                             )
                       )
                     [:tr {:key (str (:id d))}] ks)
             )
           ;(take 10 joined-data)
           joined-data
           )

      ]
     ]
    )
  )


(rum/defcs <table> < rum/reactive
                     (rum/local nil ::k)
  [local session-data column-cfg]

  (let [g (get-in session-data [:groupped-data :markName])
        all-data (:all-data session-data)]

    [:div


     (if-not @(::k local)
       (reset! (::k local) (first (keys g))))

     ;; pass 'flex-wrap: wrap;'
     (ui/menubar "Brands"
                 (reduce
                   (fn [a k]
                     (conj a
                           [k (fn []
                                (reset! (::k local) k)
                                )]
                           )
                     )
                   [] (keys g))
                 )

     ;;
     (let [idxs (:idxs (get g @(::k local)))
           table-data (reduce (fn [a i]
                                (conj a (nth all-data i))
                                ) [] idxs)
           ]

       [:div

        (map (fn [[k v]]
               [:ul.auto-ria-stats-tree {:key (str k )}
                k " " (count (:idxs v))

                (map
                  (fn [mk]
                    (let [mstats (get v mk)
                          prices (map #(js/parseInt % 10) (:prices mstats))
                          sorted-prices (sort prices)

                          ;; (:idxs mstats)
                          ]
                         [:li {:key (str mk )}

                          (pr-str mstats)

                          ;mk " " (count (:idxs mstats))
                          ;" - "

                          ;"min: $" (.toFixed (first sorted-prices) 2)
                          ;" avg: $" (.toFixed (avg prices) 2)
                          ;" max: $" (.toFixed (last sorted-prices) 2)

                          ;" of " (count prices) " prices"

                          ;;" " (pr-str (:idxs mstats))
                          ]
                         )

                    )
                  (keys v)
                  )
                ]
               )
             g
             )

        (<scraping-table> table-data column-cfg)
        ]
       )
     ]
    )
  )


(defn td-fn [grouped-row a x]
  (conj a
        [[:td (pr-str x)]]
        )
  )

(defn group-to-trs [grouped-items _td-fn]
  (reduce (fn [a grouped-row]
            (let [tds (reduce (partial _td-fn grouped-row) [] (:rows grouped-row))

                  grouped-tr (into [
                                    [:td {:row-span (count (:rows grouped-row))}
                                     (pr-str (:group-v grouped-row))
                                     ]]
                                   (first tds))

                  wrap-trs (map #(vec (concat [:tr] %))
                                (concat [grouped-tr] (rest tds)))
                  ]

                 (concat
                   a wrap-trs)

                 )
            ) [] grouped-items)
  )

(defn td-def-fn [_td-defs grouped-row a x]
    (conj a
          (map (fn [tdef]
                 [:td (pr-str (get x (:k tdef)))]
                 )
               (filter #(not (:group? %)) _td-defs))
          ))

(defn table-1 [_td-defs trs]
  [:table {:border "1"}
   [:tr
    (map (fn [tdef]
           [:th (pr-str (:h tdef))]
           )  _td-defs)
    ]
   (into [:tbody] trs)]
  )

;; grouped items to tds
(defn trr [item-td group-td a item]
  (if (:group-v item)
    ;; grouped item
    (let [new-items (reduce (partial trr item-td group-td) [] (:rows item))
          group-row (into [(group-td new-items item)] (first new-items))

          rest-rows (rest new-items)
          ]
      (concat a [group-row] rest-rows)
      )
    ;; non-grouped item
    ;; how to pass custom implementation here?
    (conj a (item-td item))
    )
  )

(defn selector-group [full-data selector data]
    (let [k1 (first selector)
          next-selector (into [] (rest selector))]
         (map (fn [[k v]]
                {
                 :group-v k
                 :group-k k1

                 :next next-selector
                 :rows (if (seq next-selector)
                         (selector-group full-data next-selector v)
                         (vals (select-keys full-data (get v :idxs [])))
                         )
                 }
                )

              (get-in data [k1]))
         )
    )

(rum/defc <custom-wf-ui> < rum/reactive [*wf]

  (let [wf @*wf
        result (get wf :result (array-map))
        ;; get listings (scraping session) from workflow results
        session-data (get result :data/session)
        column-cfg (get result :grid/columns)

        data [
         {:markName "Ford" :modelName "Mondeo" :location "Kyiv" :mainPrice 1000}
         {:markName "Ford" :modelName "Mondeo" :location "Kyiv" :mainPrice 1500}
         {:markName "Ford" :modelName "Fiesta" :location "Lviv" :mainPrice 2000}
         {:markName "Ford" :modelName "Fiesta" :location "Lviv" :mainPrice 2200}
         {:markName "Ford" :modelName "Fiesta" :location "Lviv" :mainPrice 3000}
         {:markName "Kia"  :modelName "Sportage" :location "Kyiv" :mainPrice 4000}
         {:markName "Kia"  :modelName "Sportage" :location "Kyiv" :mainPrice 4500}
         {:markName "Kia"  :modelName "Sportage" :location "Lviv" :mainPrice 4500}
         {:markName "Kia"  :modelName "Sportage" :location "Harkiv" :mainPrice 4500}
         {:markName "Kia"  :modelName "Sportage" :location "Odesa" :mainPrice 4500}
         ]
        group (partial group-session-data data)

        ]

    [:div

     #_[:pre
      (d/pretty! (take 2 (:all-data session-data)))
      ]

     (let [bldata (group [ location-grouping model-grouping-1
                          ;brand-grouping
                          ])

           ;; todo handle :markName :location

           ;; data

           ;selector [:markName :location]
           selector [:location :modelName]

           res (map (fn [[k v]] ;; first level
                  (let [
                        ;; second level of grouping
                        rows (map (fn [[k1 v1]]
                                    {:group-v k1
                                     ;; last level retrieve the rows
                                     :rows (vals (select-keys data (get v1 :idxs [])))
                                     }
                                    ) (get-in v [:location]))]
                       {:group-v k
                        :rows rows}
                       ))
                    (get-in bldata [:markName]))

           res1 (selector-group data selector bldata)
           ]
       [:div
        [:pre "DATA:\n=============\n" (d/pretty bldata)]

        #_[:pre "GROUPED DATA:\n=============\n"
         ;(d/pretty (first res))
         ;[:hr ]
         (d/pretty (first res1))
         ]

        (let [col-cfg [
                       ;{:h "markName"} {:h "location"} {:h "body"}
                       ]
              item-td  (fn [item]
                         [[:td (pr-str item)]]
                         )
              group-td (fn [new-items item]
                [:td
                 {:row-span (count new-items)}
                 (pr-str (dissoc item :rows))])

              ;trs (map #(into [:tr %])
              ;         (reduce (partial trr item-td group-td) [] res))

              trs1 (map #(into [:tr %])
                       (reduce (partial trr item-td group-td) [] res1))

              ]

          [:pre
           ;; (d/pretty trs)

           ;; hardcoded version
           ;(table-1 col-cfg trs)

           (table-1 col-cfg trs1)



           ;(d/pretty [(second (:rows (first res)))])
           ]


          #_[:pre (d/pretty trs)]
          )


        ]
       )

     [:hr]

     #_(let [cols [{:h "brand" :k :markName  :group? true}
                 {:h "model" :k :modelName}
                 {:h "price" :k :mainPrice}
                 {:h "location" :k :location }
                 ]

           bdata (group [ brand-grouping ])
           bselector [:markName]

           ;; list of groupped rows
           res (reduce (fn [a [k v]]
                         (conj a
                               {
                                ; :group-k bselector
                                :group-v k
                                ;; todo: pass indexes inside?
                                :rows    (vals (select-keys data (get v :idxs [])))
                                }))
                       [] (get-in bdata bselector))


           ]


       [:div
        ;[:pre (d/pretty bdata)]
        [:pre (d/pretty res)]
        [:pre (d/pretty (group-to-trs res (partial td-def-fn cols)))]
        (table-1 cols
                 (group-to-trs res
                               (partial td-def-fn cols)))]
       )

     ;; model grouping

     #_(let [cols [{:h "model" :k :modelName :group? true}
                 {:h "brand" :k :markName}
                 {:h "price" :k :mainPrice}
                 {:h "location" :k :location }
                 ]

           mdata (group [ model-grouping ])
           mselector []

           ;; list of groupped rows
           res (reduce (fn [a [k v]]
                         (conj a
                               {:group-k mselector
                                :group-v k
                                ;; todo: pass indexes inside?
                                :rows    (vals (select-keys data (get v :idxs [])))
                                }))
                       [] (get-in mdata mselector))


           ]


       [:div
        ; [:pre (d/pretty mdata)]
        ; [:pre (d/pretty (group-to-trs res (partial td-def-fn cols)))]
        (table-1 cols
                (group-to-trs res
                              (partial td-def-fn cols)))]
       )

     ;; location grouping

     #_(let [ldata (group [ location-grouping ])
           lselector [:location]

           ;; list of groupped rows
           res (reduce (fn [a [k v]]
                         (conj a
                               {:group-k lselector
                                :group-v k
                                :rows (vals (select-keys data (get v :idxs [])))
                                }))
                       [] (get-in ldata lselector))

           cols [{:h "location" :group? true }
                 {:h "brand" :k :markName}
                 {:h "model" :k :modelName}
                 {:h "price" :k :mainPrice}]
           ]


       (table-1 cols
                (group-to-trs res
                              (partial td-def-fn cols))
                )


       )

     [:pre
      ;brand-grouping
      ;model-grouping

      "location\n" (d/pretty!
                     (:location (group [ location-grouping ])))

      "\nlocation + brand \n" (d/pretty! (group [ location-grouping brand-grouping]))

      "\nlocation + model \n" (d/pretty! (group [ location-grouping model-grouping]))

      "\nlocation + brand + mode  \n" (d/pretty! (group [ location-grouping brand-grouping model-grouping]))

      [:hr]

      "\nmodel\n" (d/pretty! (group [ model-grouping ]))

      "\nbrand\n" (d/pretty! (group [ brand-grouping ]))

      ]


     ;; fixme: return UI
     #_(when (and column-cfg session-data)
       (<table> session-data column-cfg)
       )





     ]

    )

  )


(defn stateful-init! [*wf]
  {
   ;; for we provide a ui fn
   :ui-fn       (partial wf-ui/<wf-UI> <custom-wf-ui>)


   :title       "Display listings as a customizable table"
   :explanation ""

   :wf-actions  {}

   }
  )
