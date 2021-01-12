(ns woof.client.browser.yt.nu-wf-ui
  (:require
    [cljs.core.async :as async :refer [go go-loop]]
    [goog.dom.classes :as classes]

    [rum.core :as rum]

    [woof.base :as base :refer [rand-sid]]

    [woof.client.dom :as woof-dom]

    [woof.client.playground.ui :as ui]

    [woof.client.dbg :as dbg :refer [__log]]

    [woof.client.browser.scraper.rum-ui :as rum-wf]
    [woof.client.browser.scraper.actions :as api-wf]

    [woof.client.browser.scraper.scrape :as scrape]

    [woof.client.browser.yt.parser :as parser]

    [woof.wfs.alpha :as alpha]
    [woof.wfs.evt-loop :as evt-loop]
    [woof.client.ws :as ws]
    [woof.core.protocols :as protocols]
    [woof.data :as d]
    [woof.utils :as u]))


;; WORKFLOW UI
;;

(rum/defc <section> < rum/static
  [i dom]
  (let [{nodes :nodes
         stats :stats
         root :root
         } dom

        level-nodes (group-by :l nodes)
        ]
    [:div
     [:h3 "section" (str i)
      [:button {:on-click (fn [_]
                            (classes/add root "PROCESSED-SECTION")

                            ;woof-dom/remove-class*
                            ;;.PROCESSED-SECTION

                            )} "PROCESS"]
      ]


     (if-let [el (woof-dom/q root "#header #title")]
       [:h4 (pr-str (woof-dom/txt el))])

     (if-let [el (woof-dom/q root "#contents")]
       (let [videos (woof-dom/q* el "#contents ytd-video-renderer")
             imgs (woof-dom/q* el "#thumbnail #img")]
         [:div
          (if (and
                (= (count imgs) (count videos))
                (every? #(.-complete %) imgs))
            "LOADED"
            "LOADING!!!"
            )


          #_[:.html

             (pr-str (= (count imgs) (count videos)))
             [:hr]

             (pr-str (map (fn [img ]
                            (pr-str (.-complete img))
                            ) imgs))
             ]

          ]
         )
       [:div "IN PROGRESS"]
       )

     ; ytd-item-section-renderer

     #_[:div.flex
        (map
          (fn [[k vs]]
            [:div

             [:header (pr-str k)]

             (count vs) " "

             #_[:ul
                (map (fn [node]
                       (let [el (:el node)]
                         [:li (.-tagName el)]
                         )
                       ) vs)
                ]

             ]
            )
          level-nodes)
        ]

     ]
    )
  )


(rum/defc <scraping-ui> < rum/static
  [*state STATE]

  [:div.woof-scraper-control

   ;; api
   (when (seq (:api STATE))
     (ui/menubar "API" (:api STATE)))


   (let [upd (get STATE :upd 0)]
     [:span (pr-str upd)]
     )

   (if-let [F (:SCRAPE-FN STATE)]
     (ui/btn "SCRAPE!!!" (fn []
                           (F (:SCRAPE-SELECTOR STATE))
                           ))
     )

   ;; clean the dom from parsed elements
   (if-let [upds (:upd-class STATE)]
     (let [t (u/now)]
       [:div
        (ui/menubar ""
                    (map
                      (fn [_t]
                        [(str _t) (fn []
                                    (let [$els (woof-dom/q* (str ".ttt-" _t) )]
                                      (.log js/console $els)
                                      (doseq [$el $els]
                                        (woof-dom/html! $el ""))
                                      (swap! *state update :upd-class disj _t)

                                      )
                                    )]
                        )
                      (sort upds)
                      )
                    )
        ]
       )

     )

   (if-let [results (:RESULTS STATE)]
     [:div {:style {:font-size "6pt" :line-height 1}} (d/pretty! (sort (keys results)))])


   (if-let [_dom (:DOM STATE)]
     [:div
      [:header "DOM"]
      (map-indexed (fn [i dom]
                     (rum/with-key (<section> i dom) i)
                     ) _dom)
      ])


   ])
