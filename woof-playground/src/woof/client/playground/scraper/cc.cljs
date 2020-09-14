(ns woof.client.playground.scraper.cc
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

    [woof.client.dom :as woof-dom]
    [woof.client.playground.ui :as pg-ui]
    [woof.client.playground.ui.wf :as wf-ui]

    [woof.client.ws :as ws]

    [woof.wfs.evt-loop :as evt-loop]
    [clojure.string :as str])

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

   ;; process raw-streets

   (when-let [raw-streets (:raw-streets dict)]
     [:div
      ;; uncomment the checks

      [:div {:style {:max-width "75%"}}


       [:p {:align "JUSTIFY"} [:a {:name "2398"}] [:b "Толбухіна вулиця"] "; Толбухина, ул.; Tolbukhina, vul. 11690 (Шевченківський р-н). Рішення виконавчого комітету Київської міської ради депутатів трудящих \"Про найменування міських вулиць\" від 29.12.53 N 2610. Пролягає від Баумана, вул. (двічі, утворюючи півколо). Прилучаються: Магістральна вул., Калинова вул., Баумана, пров., Толбухіна, пров."]

         (pg-ui/menubar "street english name contains cyryllic letters" [["copy 📋" (fn []

                                      (let [clipboard js/navigator.clipboard

                                            v (filter (fn [street]
                                                        (re-find #"[а-яА-Я]" (:en street))
                                                        ) raw-streets)

                                            copy-handler (fn []
                                                           (-> (.writeText clipboard (d/pretty! v))
                                                               (.then (fn [response] (.log js/console "Copied to clipboard - " response))
                                                                      (fn [err]      (.warn js/console "Failed to copy to clipboard" err))))
                                                           )
                                            ]

                                        (copy-handler)
                                        )

                                      )]])

         (->> ;raw-streets
              (take 40 raw-streets)
              (filter (fn [street]
                        (re-find #"[а-яА-Я]" (:en street))
                        ))
              (map (fn [s]
                     (let [en (:en s)
                           district (:district s)
                           idx (:idx s)
                           other (:other s)

                           ]
                       (assoc s
                         :test (str en " " idx))

                       )

                     ))
              (map <street>)
              )
         ]



      ;; streets in several districts
      #_[:div
       [:p "find streets that occur more than once in parsed list: streets that span to multiple districts, etc"]

       (->> (group-by :ua raw-streets)
            (filter (fn [[k vs]] (> (count vs) 1)))
            (map (fn [[k vs]]
                   [:div
                    [:header
                     (pr-str (count vs)) " — "
                     (pr-str k)]

                    #_(map <street> vs)
                    ]
                   ))
            )

       ]



      #_[:div




       #_(let [all-districts (reduce (fn [a x]
                                     ;; take all
                                     ;(into a (:districts x))
                                     ;; take main one
                                      (conj a (:district x))
                                     ) #{} (take 3100 raw-streets))]
        [:div {:style {:width "25%"}}
         [:p "get all districts, clean-up if needed"]
         (str (count all-districts) " total") [:hr]

         (map (fn [x]
                [:div (pr-str x)]
                )
              (sort all-districts)
              )
         ]
         )

       ;; find districts with enters
       #_[:div {:style {:max-width "75%"}}
        (->> raw-streets
             ; (take 100 raw-streets)
             (filter (fn [street]
                       (let [district (:district street)]
                         (re-find #"\n" district)
                         )
                       ) )
             (map <street>)
             )
        ]

       ;; Подільський р-н
       #_[:div {:style {:max-width "75%"}}
        (->> raw-streets
             ; (take 100 raw-streets)
             (filter (fn [street]
                       (let [district (:district street)]
                         (and (= "Подільський р-н" district)
                              ;(not= ["Подільський р-н"] (:districts street))
                              )
                         )
                       ) )
             (map <street>)
             )
        ]


       #_[:div {:style {:max-width "75%"}}
        (->> (group-by :idx
                       (filter #(= (:district %) "Подільський р-н") raw-streets)
                       ; raw-streets

                       )
             ;(filter (fn [[k vs]] (> (count vs) 1)))
             (sort-by first)
             (map (fn [[k vs]]
                    [:pre
                     (pr-str k)
                     "\t"
                     (pr-str (count vs))

                     (map <street> vs)
                     ]
                    ))
             )
        ]






       #_[:pre
        (pr-str
          (reduce (fn [a x]
                    (into a (:districts x))
                    ) #{} (take 3100 raw-streets))
          )
        ]

       #_(->> (group-by :district (take 10 raw-streets))
            ;(filter (fn [[k vs]] (> (count vs) 1)))
            (map (fn [[k vs]]
                   [:pre
                    (pr-str k)
                    "\t"
                    (pr-str (count vs))

                    #_(map <street> vs)
                    ]
                   ))
            )

       ]



      ]

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

       (let [*dict (rum/cursor-in *wf [:state ::dict])]
         [:div {:style {:padding "1rem"}}

          (<streets> (get-in wf [:state ::dict]) *dict)

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
        *dict (rum/cursor-in *SWF [:state ::dict])]
    {

     :title       "Scraping command center"
     :explanation [:div.explanation
                   [:p "Analyze scraped data here"]]

     ;; this state will be added to a wf?
     :state {
             ::dict {
                     :streets []
                     :raw-streets []
                     }


             }

     :init-fns    [

                   { ;; pass modifiable zipper
                    ::*dict *dict
                    }

                   (base/build-init-chan-factory-fn CHAN-FACTORY)
                   (evt-loop/build-evt-loop-init-fn (base/make-chan CHAN-FACTORY (base/rand-sid "evt-")))
                   ]
     ;;
     :ctx-fns     [
                   evt-loop/evt-loop-ctx-fn

                   woof-browser/common-ctx ;; re-use common browser step handlers


                   woof-dom/dom-ctx

                   (fn [params]
                     {
                      ;;

                      }
                     )
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
