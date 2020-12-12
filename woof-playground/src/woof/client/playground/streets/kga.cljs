(ns woof.client.playground.streets.kga
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
    [woof.data.core :as data]
    [woof.utils :as utils]

    [woof.browser :as woof-browser]
    [woof.client.dom :as wdom]

    [woof.client.dom :as woof-dom]
    [woof.client.playground.ui :as pg-ui]
    [woof.client.playground.ui.wf :as wf-ui]
    [woof.client.playground.streets.ui :as s-ui]

    [woof.client.playground.streets.ds :as ds]
    [woof.client.ws :as ws]

    [woof.wfs.evt-loop :as evt-loop]


    [clojure.set :as set]
    [woof.client.common :as common]
    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;;
;; UI

(defn- load-edn [*dict url k]
  (ws/GET url
          (fn [response]
            (let [edn (cljs.reader/read-string response)]
              (swap! *dict assoc k edn)))))


(defn- load-json [*dict url k]
  (ws/GET url
          (fn [response]
            (let [edn (js->clj (.parse js/JSON response) :keywordize-keys true)]
              (swap! *dict update k
                     (fn [a b]
                       (if (seq a)
                         (concat a (:features b))
                         (:features b)))
                     edn)))))






(rum/defc <raw-kga-streets> < rum/static
  [kga-streets]

  (let [
        streets (into []
                      (comp
                        (map-indexed (fn [i a]
                                       (let [attr (:attributes a)
                                             trim (fnil str/trim "")
                                             &v (fn [k]
                                                  (trim (get attr k ""))
                                                  ;(get attr k "")
                                                  )
                                             ]

                                         (array-map
                                           :_orig attr

                                           :i i

                                           :oid (:OBJECTID attr)
                                           :code (:StrCodeObj attr)

                                           :ID (:StrCodeObj attr)

                                           :t (&v :TypeNameF)

                                           :cua (&v :LblStreetName)
                                           :ua (&v :UkrNameF)
                                           :ru (&v :RusNameF)
                                           :en (&v :LatNameF)

                                           :district (&v :Districts)

                                           :other (&v :OpysRozt)
                                           :alias (vec (filter #(not (or (nil? %) (= "" %)))
                                                               (into #{}
                                                                     (map trim (vals (select-keys attr [:UkrNameS :RusNameS :LatNameS]))))))
                                           )
                                         )

                                       ))

                        )
                      kga-streets
                      )
        ]
    [:kga-streets.html

     ;(pr-str (keys streets))
     ;"\n"
     ;(pr-str (count (:features streets)))

     ;(pg-ui/<edn-list> features "STREETS:")


     (pg-ui/<transform-list>
       s-ui/<street> streets {}
       :id-fn :ID
       :copy-fn #(dissoc % :i :_orig)
       :sort-fn (fn [a b]
                  (compare (:code a) (:code b))
                  )
       )

     ]
    )

  )


(rum/defc <street> < rum/static {
                                 :key-fn (fn [street] (str (:ID street)))}
  [street]

  (let [{t         :t
         ua        :ua
         ru        :ru
         en        :en
         idx       :idx
         district  :district
         districts :districts
         other     :other
         alias     :alias
         :or       {alias []}
         } street]

    [:div.street-row
     {:on-click (fn [e] (.log js/console street))
      :class    (get street :css "")
      }
     [:div
      [:div
       [:span.tag.small-tag.idx (get street :i -1)]
       [:span.tag.small-tag.idx idx]
       [:span.tag.small-tag.district district]
       #_[:span.districts
          (map (fn [d] [:span.small-tag.district {:key (pr-str d)} d]) districts)]
       [:span.aliaes
        (map (fn [d] [:span.tag.small-tag.alias {:key (pr-str d)} d]) alias)]

       ]

      [:.langs
       (if t [:.t t])
       [:.ua ua]
       [:.ru ru]
       [:.en en]
       ]
      ]

     [:.other other]

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





(rum/defcs <streets-cc> < rum/reactive
  [st *dict]

  (let [dict @*dict
        ;;
        enrich-district "shevchenkivskyi"
        ]
    [:div.streets-pg

     [:.panel
      (pg-ui/menubar "Kadastr Data   "

                     [["load streets (MAIN DS)" (fn []
                                                  (load-edn *dict (str "/s/kga/streets.edn") :kga-streets)
                                                  )]

                      [(str "load house addrs (" enrich-district ")") (fn []
                                                                        (load-edn *dict (str "/s/kga/district/kga-houses-" enrich-district ".edn") :house-map))]

                      ["load :drv-buildings-per-street" (fn []
                                                          (load-edn *dict "/s/drv/_buildings.edn" :drv-buildings-per-street))]
                      ]


                     ;; uncomment to
                     #_(into []
                             (map (fn [d]
                                    [(str "load :" d) (fn [] (load-json *dict (str "/s/kga/district/" d ".json") :d1))]
                                    ) districts)
                             )
                     )
      ]



     ;; concating kga streets for each district
     ;; preparing initial version of main street data source
     #_(when-let [kga-streets (:d1 dict)]
         (<raw-kga-streets> kga-streets))


     (when-let [kga-streets (:kga-streets dict)]
       (let [district (ds/district-k :fs :kga enrich-district)
             xs (comp
                  (filter #(= (:district %) district))
                  (map :ID)
                  )
             district-ids (into [] xs kga-streets )]
         [:.html
          [:header "List all street ids for district=" district]
          "\n"
          (pr-str district-ids)
          ]
         )
       )

     ;; extracting houses for each street
     #_(let [houses #{"13/9" "26" "4" "57-59" "28" "20" "19" "45/48" "42" "44" "18"
                      "38/44"
                      "62"
                      "35"
                      "50/49"
                      "27/44"
                      "53/55"
                      "38"
                      "70" "12-В" "17/25" "25/49" "2" "16" "40" "64" "23" "52" "28/58"}
             ]
         [:.html
          "compare house numbers in DRV dataset to KGA dataset "
          [:.flex
           [:div
            (d/pretty! (sort houses ))
            ]

           [:div
            (d/pretty!
              (map :n '({:n "13/9", :c "", :idx "04070", :flats "83"} {:n "16", :c "", :idx "04070", :flats "11"} {:n "18", :c "", :idx "04070", :flats "20"} {:n "19", :c "", :idx "04070", :flats "18"} {:n "23", :c "", :idx "04070", :flats "58"} {:n "25/49", :c "", :idx "04071", :flats "65"} {:n "26", :c "", :idx "04070", :flats ""} {:n "35", :c "", :idx "04070", :flats "86"} {:n "40", :c "", :idx "04070", :flats "12"} {:n "44", :c "", :idx "04070", :flats "30"} {:n "45", :c "", :idx "04071", :flats "28"} {:n "50/49", :c "", :idx "04070", :flats "53"} {:n "52", :c "", :idx "04070", :flats "40"} {:n "53/55", :c "", :idx "04070", :flats "53"} {:n "57/59", :c "", :idx "04070", :flats "142"} {:n "62", :c "", :idx "04070", :flats "36"} {:n "64", :c "", :idx "04070", :flats "54"} {:n "70", :c "", :idx "04070", :flats "62"})))
            ]
           ]
          ]
         )



     ;; join houses to a street
     (when-let [house-map (:house-map dict)]
       (let [
             drv-buildings (get dict :drv-buildings-per-street {})

             *asserts (volatile! [])

             __no-houses (fn [item] (if (empty? (:houses item))  {:ID (:ID item) :class #{"no-house"}}))

             ;; generate drv names
             _t-mapping {"вулиця" "вул."
                         "провулок" "пров."
                         "проспект" "просп."
                         "площа" "пл."
                         "узвіз" "узвіз "
                         "проїзд" "проїзд "}

             add-drv (fn [item]
                       (let [_t (get _t-mapping (:t item) "")
                             ua (:ua item)
                             words (str/split ua " ")
                             s (str _t (:ua item))

                             combs (into #{} (map #(str _t
                                                        (str/join " " %))) (combo/permutations words))
                             matches (select-keys drv-buildings combs)
                             ]

                         ;;(.log js/console (keys drv-buildings) combs matches)

                         (if (empty? matches)
                           item
                           (let [k (first (keys matches))

                                 hs (get item :houses {})
                                 drv-hs (group-by :n (get matches k))

                                 h-ks (into #{} (keys hs))

                                 {
                                  nu-houses :kga-houses
                                  used :used
                                  no-matches :no-matches
                                  } (ds/enrich-kga-houses hs drv-hs)

                                 ]

                             #_(if-not (empty? no-matches)
                               (.log js/console
                                     k no-matches
                                     ;;drv-diff
                                     h-ks
                                     )
                               )

                             (let [ret-item (assoc item
                                              :drv k
                                              ;:drv-h drv-ks
                                              ;:h h-ks
                                              :houses nu-houses
                                              )]

                               (if (empty? no-matches)
                                 ret-item
                                 (assoc ret-item
                                   :test
                                   (str
                                     "not matched: "
                                     (pr-str no-matches)
                                     "\n\n"
                                     (d/pretty! (sort h-ks))
                                     )
                                   )
                                 )
                               )

                             )

                           )
                         )
                       )

             __drv-street (fn [item]
                            (if (:drv item)
                              {:ID (:ID item) :class #{"drv-street"}}))

             __test-street (fn [item]
                            (if (:test item)
                              {:ID (:ID item) :class #{"test-street"}}))


             add-houses (fn [street]
                          (if-let [h (get house-map (:ID street))]
                            (assoc street :houses (reduce (fn [a h]
                                                            (assoc a h {}))
                                                    {} h))
                            street))

             kga-streets (:kga-streets dict)
             district (ds/district-k :fs :kga enrich-district)
             xs (comp
                  ;;
                  (filter #(= (:district %) district))

                  ;; enrich with houses + gather meta
                  (data/z-map-1
                    (data/juxt-mapper
                      __no-houses
                      __drv-street
                      __test-street)
                    #(vswap! *asserts into %)
                    (comp ;; note that order is reversed here
                      add-drv
                      add-houses
                      )
                    )

                  ;; enrich with house addrs
                  ; (map add-houses)
               )
             districts (into [] xs kga-streets )

             ]

         [:.flex

          ;;
          ;; uncomment this to export enriched streets
          #_(let [district-map (group-by :ID districts)]

            [:div

             ;[:.html (d/pretty! drv-buildings)]

             [:header "All streets enriched with " district "district houses. "]

             (pg-ui/<transform-list>
               s-ui/<street>
               (map (fn [a]
                      (if-let [nu (get district-map (:ID a))]
                        (first nu)
                        a
                        )
                      ) kga-streets)
               {}
               :copy-fn #(ds/street2export (dissoc % :test :oid))
               :sort-fn :code
               )
             ]
            )

          ;;
          ;; kga to drv mapping
          (pg-ui/<transform-list>
            s-ui/<street>
            districts
            ;; filters
            (group-by :ID @*asserts)
            :id-fn :ID
            :copy-fn #(dissoc % :test)
            ;:sort-fn (fn [a b] (compare (:code a) (:code b)))
            :filter-map {
                         "test-street" (partial pg-ui/_marker-class-filter "test-street")
                         "drv-street" (partial pg-ui/_marker-class-filter "drv-street")
                         "non drv-street" (partial pg-ui/_marker-except-class-filter "drv-street")
                         "no-house" (partial pg-ui/_marker-class-filter "no-house")
                         }
            )
          [:div

           ;;[:.html (d/pretty (get drv-buildings "вул.Андріївська"))]
           ;;[:hr]

           (pg-ui/<transform-list>
             #(vector :div (pr-str %))
             (keys (get dict :drv-buildings-per-street {}))
             {}
             )
           ]
          ]
         )

       )

     ]
    )
  )



(rum/defcs <WF> < rum/reactive
  [local *wf]

  (let [wf @*wf
        not-started? (= :not-started (:status wf))]
    [:div.wf-root
     (if not-started?
       [:div "WF is not running."]
       (let [*data (rum/cursor-in *wf [:state ::data])]
         (try
           ;; your wf is here
           (<streets-cc> *data)
           (catch js/Error e [:pre (pr-str e)]))))
     ]))


;;;;;;;;;;;;;;;;;;;;

;;
;; WF definition
(defn wf! [*SWF]

  (let [CHAN-FACTORY (base/chan-factory (atom {}))
        *dict (rum/cursor-in *SWF [:state ::data])]
    {

     :title                               "kadastr"
     :explanation                         [:div.explanation
                                           [:p "process data scraped from " [:a {:href "https://mkk.kga.gov.ua/map/" :target "_blank"} "kadastr"]]
                                           ]

     ;; this state will be added to a wf?
     :state                               {
                                           ::data {
                                                   ;; just pile of data
                                                   }
                                           }

     :init-fns                            [
                                           {::*data *dict}

                                           (base/build-init-chan-factory-fn CHAN-FACTORY)
                                           (evt-loop/build-evt-loop-init-fn (base/make-chan CHAN-FACTORY (base/rand-sid "evt-")))
                                           ]
     ;;
     :ctx-fns                             [
                                           evt-loop/evt-loop-ctx-fn
                                           ;; re-use common browser step handlers
                                           common/common-ctx
                                           wdom/dom-ctx


                                           ]
     ;;
     :steps-fns                           [
                                           (fn [params] {::#EVT-LOOP# [:evt-loop (evt-loop/&evt-loop params)]})
                                           (partial woof-dom/_add-style-once-steps-fn "http://localhost:9500/css/apt.css")

                                           ]

     :opt-fns                             [
                                           (base/build-opts-chan-factory-fn CHAN-FACTORY)
                                           ;; uncomment to display wf results (esp. if there was an error)
                                           (base/build-opt-on-done (fn [params result]
                                                                     (.warn js/console params result)))
                                           ]

     :ui-fn                               (partial wf-ui/<wf-UI> (partial <WF>))


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
