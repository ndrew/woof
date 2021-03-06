(ns woof.client.playground.streets.pg
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

    [woof.client.playground.streets.ds :as ds]
    [woof.client.ws :as ws]

    [woof.wfs.evt-loop :as evt-loop]

    [clj-fuzzy.metrics :as metrics]

    [clojure.core.reducers :as r]
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
              (swap! *dict assoc k edn)
              )))

  )

;;
(def jaro (memoize metrics/jaro))



;;
;; streets


(rum/defc <street> < rum/static {
                                 :key-fn (fn [street] (str (:ua street) (:idx street)))}
  [street]

  (let [{t :t
         ua :ua
         ru :ru
         en :en
         idx :idx
         district :district
         districts :districts
         other :other
         alias :alias
         :or {alias []}
         } street]

    [:div.street-row
     {:on-click (fn[e] (.log js/console street))
      :class (get street :css "")
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

;; from map




(def ua-geonim-2-short
  ;; (clojure.set/map-invert kyiv-abbr-map)
  {
   "озеро" "оз.",
   "переулок" "пер.",
   "улица" "ул.",
   "господарство" "госп-во",
   "проспект" "просп.",
   "Авіаційний науково-технічний комплекс" "АНТК",
   "тупик" "туп.",
   "річка" "р.",
   "кооперативне дачне товариство" "КДТ",
   "товариство індивідуальних забудовників" "ТІЗ",
   "садове товариство колективу індивідуальних забудовників" "СТ КІЗ",
   "вулиця" "вул.",
   "садове товариство" "СТ",
   "провулок" "пров.",
   "садово-дачне товариство" "СДТ",
   "набережна" "наб.",
   "чверть" "чв.",
   "затока" "зат.",
   "район" "р-н",
   "бульвар" "бульв.",
   "половина" "пол.",
   "острів" "о.",
   "площа" "пл.",
   "алея" "ал.",
   "середина" "серед.",
   "узвіз" "узвіз"
   "шосе" "шосе"
   "дорога" "дорога"
   "проїзд" "проїзд"
   "станція" "ст."
   }
  )

(def short-2-full (assoc (clojure.set/map-invert ua-geonim-2-short)
                    "тупик" "тупик"
                    "смт" "смт"
                    "набережна" "набережна")
  )


(def ru-geonim-2-short
  ;; (clojure.set/map-invert kyiv-abbr-map)
  {
   "озеро" "оз.",
   "переулок" "пер.",
   "улица" "ул.",
   "господарство" "госп-во",
   "проспект" "просп.",
   "Авіаційний науково-технічний комплекс" "АНТК",
   "тупик" "туп.",
   "річка" "р.",
   "кооперативне дачне товариство" "КДТ",
   "товариство індивідуальних забудовників" "ТІЗ",
   "садове товариство колективу індивідуальних забудовників" "СТ КІЗ",
   "вулиця" "вул.",
   "садове товариство" "СТ",
   "провулок" "пров.",
   "садово-дачне товариство" "СДТ",
   "набережна" "наб.",
   "чверть" "чв.",
   "затока" "зат.",
   "район" "р-н",
   "бульвар" "бульв.",
   "половина" "пол.",
   "острів" "о.",
   "площа" "пл.",
   "алея" "ал.",
   "середина" "серед.",
   "узвіз" "узвіз"
   "шосе" "шосе"
   "дорога" "дорога"
   "проїзд" "проїзд"
   }
  )




(rum/defc <rename> < rum/static
                     { :key-fn (fn [m] (apply str (sort (keys m))))}
  [m]
  [:div.rename
   (map (fn [[k v]]
          [:div [:span (pr-str k)] [:span "→"] [:span (pr-str v) ]]
          ) m)
   ]
  )


;; initial version of rename mapping
(rum/defcs <street-renaming-nu> < rum/static
  [st renamed-streets cstreet-names]

  (let [canonical-names (into #{} cstreet-names)


        *added-streets (atom [])
        ;{:ua "Садова 54 вулиця",
        ; :ru "Садовая 54 ул.",
        ; :en "Sadova 54 vul.",
        ; :idx "12250",
        ; :district "Дарницький р-н",
        ; :districts ["Дарницький р-н" "СТ \"Автомобіліст\"" "\"Восход\"" "\"Дніпро\"" "\"Здоров'я\"" "\"Злагода\"" "\"Медпрепарат\"" "\"Метрополітеновець\"" "\"Мир\"" "\"Райдуга\"" "\"РайФО\"" "\"Рибпроект\"" "\"Ромашка\"" "\"Сонячний\""],
        ; :other "Пролягає від безіменного проїзду поруч з оз. Підбірна до кінця забудови. Прилучаються: Центральна вул., Садова 74 вул., проїзд до Колекторної вул."}
        ]

    [:.html

     "true false - old street name is in canonical list, new one not present — rename it\n"
     "false false - old name is missing, new name is not present — maybe add new street, or normalization error\n"
     "true true - both names are present — migrate old one to an alias\n"

     ; (pr-str renamed-streets)
     (map (fn [[distr rename-map]]
            [:div
             [:header distr]

             ; canonical-names
             [:div.rename
              (map (fn [[k v]]
                     (let [_c-old-name (ds/normalize-ua-geonim k)


                           words (str/split _c-old-name " ")
                           last-w (last words)

                           matches (reduce (fn [a s]
                                             (let [k (str/join " " (conj (vec s) last-w))]
                                               (if (contains? canonical-names k)
                                                 (assoc a k true)
                                                 a
                                                 )
                                               )

                                             ) {} (combo/permutations (drop-last words)))

                           alt-k (first (keys matches))


                           _has-old-name? (contains? canonical-names _c-old-name)

                           has-old-name? (if _has-old-name?
                                           true
                                           (contains? canonical-names alt-k)
                                           )


                           c-old-name (if-not _has-old-name?
                                        (if alt-k
                                          alt-k
                                          _c-old-name
                                          )
                                        _c-old-name)

                           c-new-name (ds/normalize-ua-geonim v)
                           has-new-name? (contains? canonical-names c-new-name)



                           ]
                       [:div (cond
                               ;(and has-old-name? (not has-new-name?)) {:style {:color "red"}}
                               (and has-old-name?       has-new-name?) {:style {:color "magenta"}}
                               (and (not has-old-name?) has-new-name?) {:style {:color "blue"}}
                               (and (not has-old-name?) (not has-new-name?)) {:style {:color "red"}}
                               (and has-old-name?       (not has-new-name?)) {:style {:color "black" :opacity ".33"}}
                               )


                        (if
                          ;(and has-old-name? has-new-name?) ;; both streets are in list, one prev should be removed and become an alias
                          ;(and (not has-old-name?) (not has-new-name?)) ;; add new street, as it not in the canonical street list
                          ;(and (not has-old-name?) has-new-name?) ;; only new is present, only update alias
                          (and has-old-name?       (not has-new-name?))
                          (do (swap! *added-streets conj
                                     {:ua c-new-name
                                      :ru ""
                                      :en ""
                                      :idx ""
                                      :district distr
                                      :other (str "renamed from '" c-old-name "'" )
                                      :alias [c-old-name]
                                      })
                              nil
                              )
                          )


                        (if-not has-old-name?
                          [:.html (d/pretty! matches)])

                        [:span.tag (str has-old-name? " " has-new-name?)]
                        [:span (pr-str k)] [:span "→"] [:span (pr-str c-old-name)] [:span {:style {:margin "0 3rem"}} "=>"]
                        [:span (pr-str v) ] [:span "→"] [:span (pr-str c-new-name)]
                        ]
                       ))
                   rename-map
                   )
              ]

             ;(<rename> rename-map)

             ]
            )
          renamed-streets
          )

     [:hr]
     (d/pretty! @*added-streets)
     ]
    )

  )


(defn full-street-name [geonim-model]
  (str/trim (str (:s geonim-model) " " (:t geonim-model))))




;; handle genonims
(defn _drv-guess-geonim [short-2-full s]

  (let [dot-parts (str/split s ".")
        space-parts (str/split s " ")]

    (merge {
            :str s
            }
           (cond

             ;; вул. - type in prefix position
             (> (count dot-parts) 1)
             (let [[short-g short-name] dot-parts
                   geonim (get short-2-full (str short-g "."))]
               (if geonim
                 {
                  :g      geonim
                  :street (str/trim (str short-name " " geonim))
                  :short (str/split short-name " ")
                  }
                 {
                  :no-guess? :dot
                  }
                 ))

             ;;  шосе - tyoe in a suffix position
             (> (count space-parts) 1)
             (let [first_geonim (get short-2-full (first space-parts))
                   last_geonim (#{"шосе" "узвіз" "бульвар" "набережна" "проспект" "дорога"} (last space-parts))
                   geonim-2-use (cond
                                  (not (nil? first_geonim)) first_geonim
                                  (not (nil? last_geonim)) last_geonim)]
               #_(if geonim-2-use
                   {
                    :g geonim-2-use
                    }
                   {
                    :no-guess? :space
                    })
               (cond
                 (not (nil? first_geonim))
                 {:prefix? true
                  :street (str/join " " (conj (rest space-parts) first_geonim ))
                  :short (rest space-parts)
                  :g  first_geonim
                  }

                 (not (nil? last_geonim))
                 { :g last_geonim
                  ;;:street (str )
                  :street (str/join " " (conj (vec (drop-last space-parts)) last_geonim))
                  :short (drop-last space-parts)
                  :suffix? true
                  }
                 :else {
                        :no-guess? :space
                        }
                 )

               )
             :else {
                    :no-guess? :any
                    })
           )


    ))



(rum/defc <street-1> < rum/static
  [cstreet-map street]

  (let [parts (:short street)
        gt (:g street)
        multi-word? (> (count parts) 1)
        matching-perms (:matches street)
        has-matches? (> (count matching-perms) 0)
        ]
    [:div.extract-street  {:style {} #_{:color (if has-matches?
                                 "grey"
                                 "red"
                                 )}
           }

     [:header [:span.tag gt] (:str street) " " (:street street)]


     (<rename> (reduce (fn [a s]
                         (let [canonical (str  (str/join " " s) " " gt)]
                           (if-let [link (get cstreet-map canonical)]
                             (assoc a (:str street) link)
                             (assoc a canonical false)
                             ))) {} (combo/permutations parts)))


     ;; (pr-str street)

     #_(if-not has-matches?
       [:.html {:style {:color "red"}}
        (d/pretty!
          (reduce (fn [a s]
                    (let [canonical (str  (str/join " " s) " " gt)]
                      (if-let [link (get cstreet-map canonical)]
                        (assoc a (:str street) link)
                        (assoc a canonical false)
                        ))) {} (combo/permutations parts))
          )
        ])



     #_(map (fn [[k v]]
            [:div (pr-str k) " -> " (pr-str v)]
            )
          matching-perms
          )
     #_(if multi-word?
         )
     ; (pr-str street)

     ])
  )

(rum/defcs <drv-extract-streets> < rum/static
                                   (rum/local :all ::filter)
  [st drv-builings cstreet-map]

  (let [streets-to-canonize (keys drv-builings)

        candidates (->>
                     streets-to-canonize
                     (map (partial _drv-guess-geonim short-2-full))

                     (map #(let [guess-street (:street %)

                                 gt (:g %)
                                 parts (:short %)

                                 multi-word? (> (count parts) 1)
                                 matching-perms (reduce (fn [a s]
                                                          (let [canonical (str  (str/join " " s) " " gt)]
                                                            (if-let [link (get cstreet-map canonical)]
                                                              (assoc a (:str %) link)
                                                              a ;(assoc a canonical false) ;; a
                                                              ))) {} (combo/permutations parts))

                                 ]
                             (merge
                               %
                               {:matches matching-perms}
                               (if guess-street
                                 {:f/guess? :guessed}
                                 {:f/guess? :not-guessed})

                               (if (empty? matching-perms)
                                 {:f/canonical :non-canonical}
                                 {:f/canonical :canonical}
                                 )
                               )
                             )))

        guesses-map (group-by :f/guess? candidates )
        ]

    [:.flex

     (let [{guessed :guessed
            not-guessed :not-guessed} guesses-map
           total-count (count candidates)
           ]

       [:div

        #_[:.html
           (pr-str (first cstreet-map))
           ]

        ;; review by eyes there and skip these for now
        ;[:header "Not guessed: " (str (count not-guessed) " vs " total-count) ]
        #_(map #(do
                  [:.html (:str %) "\n"
                   (d/pretty! %)

                   "\n"]
                  ) not-guessed)
        [:hr]
        [:header "Guessed: " (str (count guessed) " vs " total-count) ]


        (let [canonical-map (group-by :f/canonical guessed)
              {matching :canonical
               non-matching :non-canonical
               } canonical-map
              ]
          [:.html
           ;; for matching we can extract building numbers
           #_[:header "matching"]
           #_(map #(do
                     [:p
                      (pr-str %)]) matching)

           #_[:.html
              (d/pretty! (keys cstreet-map))
              ]


           [:header "non matching " (str "(" (count non-matching) ")")]
           (map (partial <street-1> cstreet-map) non-matching)

           ;(pr-str (count matching))
           ]
          )
        ]
       )



     #_(let [not-matching (get candidates :not-matching)]
         [:.html

          (str "not matching: " (count not-matching) " vs all: " (count streets-to-canonize))

          #_(map
              #(if-let [geonim (get cstreet-map %)]
                 [:p "[OK] " %  ;(pr-str geonim)
                  ]
                 [:p {:style {:color "red"}} %]
                 )
              (get candidates true)
              )
          ]
         )


     #_(let [matching (get candidates :canonical)]
         [:.html

          (str "matching: " (count matching) " vs all: " (count streets-to-canonize))

          #_(map
              #(if-let [geonim (get cstreet-map %)]
                 [:p "[OK] " %  ;(pr-str geonim)
                  ]
                 [:p {:style {:color "red"}} %]
                 )
              (get candidates true)
              )
          ]
         )

     ]
    )
  )


(defn drv-street-buildings-aggr [buildings-list]
  (reduce (fn [a b]
            (-> a
                (update :idx conj (int (:idx b)))
                (update :flats-total + (int (:flats b)))
                )
            )
          {:houses-total (count buildings-list)
           :flats-total 0
           :idx #{}}
          buildings-list)
  )

(rum/defc <drv-house> < rum/static
                        {:key-fn (fn [street h] (str street (:n h) "_" (:idx h)) )}
  [street house]


  (let [s_flats (:flats house)
        c (int s_flats)
        ]
    [:div.h (cond
              (= 0 c) {:class "non-living"}
              (> c 100) {:class "big-h"}
              )
     [:header (:n house)]
     s_flats]
    )

  )

(rum/defcs <drv-street> < rum/static
                         {:key-fn (fn [[str]] str)}
                          (rum/local false ::show-buildings?)

  [st [street buildings-list]]

  (let [aggr (drv-street-buildings-aggr buildings-list)]

    [:.drv-street
     [:nav
      (pg-ui/menu-item ">" (fn []
                             (swap! (::show-buildings? st) not)
                             ))
      [:.street street]

      [:span.tag.flats (str "total flats: " (:flats-total aggr))]
      [:span.tag.houses (str "total houses: " (:houses-total aggr))]
      [:span.tag.idx (str "idx: " (:idx aggr))]

      ]
     (if @(::show-buildings? st)
       [:.houses.flex
        (map (partial <drv-house> street) buildings-list)
        ]
       )

     ]
    )

  )

(rum/defcs <drv-buildings-list> < rum/static
  [st
   geonims
   drv-street-map]

  ;; sorting and filtering

  [:div
   (map <drv-street> drv-street-map)]
  )


(defonce OPENING-BRACKETS
  {"cljs.core/PersistentTreeSet" "#{"
   "cljs.core/PersistentHashSet" "#{"
   "cljs.core/PersistentHashMap" "{"
   "cljs.core/List"              "["
   "cljs.core/EmptyList"         "["
   "cljs.core/LazySeq"           "("
   "cljs.core/KeySeq"            "("
   "cljs.core/IndexedSeq"        "("
   "cljs.core/PersistentVector"  "["
   })

(defonce CLOSING-BRACKETS
         {"cljs.core/PersistentTreeSet" "}"
          "cljs.core/PersistentHashSet" "}"
          "cljs.core/PersistentHashMap" "}"
          "cljs.core/List"              "]"
          "cljs.core/EmptyList"         "]"
          "cljs.core/PersistentVector"  "]"
          "cljs.core/KeySeq"            ")"
          "cljs.core/LazySeq"           ")"
          "cljs.core/IndexedSeq"        ")"
          })








(defn extract-ru-renamings_17_19 [*dict]
  (let [c1 (rest (wdom/q* "#renamings_2017-2019 tr > td:nth-child(2)"))
        c2 (rest (wdom/q* "#renamings_2017-2019 tr > td:nth-child(3)"))
        c3 (rest (wdom/q* "#renamings_2017-2019 tr > td:nth-child(4)"))

        t (comp str/trim wdom/txt)

        old (map t c1)
        nu (map t c2)
        ds (map t c3)

        ;#{"Солом'янський р-н" "Подільський р-н" "Голосіївський р-н" "Шевченківський р-н" "Деснянський р-н" "Дарницький р-н" "Печерський р-н" "Оболонський р-н" "Святошинський р-н" "Дніпровський р-н"}
        d-mapping {"Святошинский" "Святошинський р-н"
                   "Соломенский" "Солом'янський р-н"
                   "Печерский" "Печерський р-н"
                   "Голосеевский" "Голосіївський р-н"
                   "Дарницкий"  "Дарницький р-н"
                   "Днепровский" "Дніпровський р-н"
                   "Деснянский"  "Деснянський р-н"
                   "Шевченковский" "Шевченківський р-н"
                   "Подольский" "Подільський р-н"
                   }

        items (reduce
          (fn [a [old nu ds]]
            (into a (map (fn [_d]
                           {:district (get d-mapping (str/trim _d) "")
                            :ru nu :alias [old]}

                           )(str/split ds ",")))
            )
          []
          (partition-all 3
                         (interleave
                           old
                           nu
                           ds
                           )))
        ]

    (swap! *dict assoc :renamed-ru-1 items)

    )
  )

(defn extract-ru-renamings_15_17 [*dict]
  (let [c1 (rest (wdom/q* "#renamings_2015-2017 tr > td:nth-child(2)"))
        c2 (rest (wdom/q* "#renamings_2015-2017 tr > td:nth-child(3)"))

        t (comp str/trim
                wdom/txt)

        old (map t c1)
        nu (map t c2)]

    (swap! *dict assoc :renamed-ru
           (partition-all 2
                          (interleave
                            old
                            nu
                            ))
           )

    ;(.log js/console (count (group-by identity nu)))
    #_(.log js/console (count (map wdom/txt c1)))


    #_(.log js/console  (apply assoc {} (interleave (map wdom/txt c1)
                                                     (map wdom/txt c2))))

    )
  )





;; extend filtering with saving meta-data

(defn z-map
  ([*v v-xf f]
   (let [_meta-results (transient []) ;; always use vector for meta log

         add-meta! (fn [input]
           (let [_metas (v-xf input)
                 metas (if (or (seq? _metas) (vector? _metas)) _metas
                                                               [_metas])]
             (apply conj! _meta-results
                    (filter some? metas))))

         ]
     (fn [rf]
       (fn
         ([] (rf))
         ([result]
          (vswap! *v into (persistent! _meta-results))
          (rf result))
         ([result input]
          (let [nu-v (f input)]
            (add-meta! nu-v)
            (rf result nu-v)
            )
          )
         ([result input & inputs]
          (let [nu-v (apply f input inputs)]
            (add-meta! nu-v)
            (rf result nu-v))
          ))))
   )
  )



(defn z-filter
  ([*v v-xf pred]
   (let [_meta-results (transient [])] ;; always use vector for meta log
     (fn [rf]
       (fn
         ([] (rf))
         ([result]
          (vswap! *v into (persistent! _meta-results))
          (rf result))
         ([result input]
          (if (pred input)
            (let [_metas (v-xf input)
                  metas (if (or (seq? _metas) (vector? _metas)) _metas
                                                                [_metas])
                  ]
              (apply conj! _meta-results
                     (filter some? metas))

              (rf result input)
              )
            result))))
     )))


(defn z-group
  [f rf coll]
  (persistent!
    (reduce
      (fn [ret x]
        (let [k (f x)]
          (assoc! ret k (rf (get ret k) x))))
      (transient {}) coll)))


(defn z-map-group
  ([*v-map g-f g-rf f]

   (let [grouppings (volatile! {})

         group! (fn [x]
           (let [k (g-f x)
                 v (g-rf (get @grouppings k) x)]

             (vswap! grouppings assoc k v)
             ))
         ]

     (fn [rf]
       (fn
         ([] (rf))

         ([result]
          (vreset! *v-map @grouppings)
          (rf result)
          )
         ([result input]
          (let [nu-v (f input)]
            (group! nu-v)
            (rf result nu-v)
            )
          )
         ([result input & inputs]
          (let [nu-v (apply f input inputs)]
            (group! nu-v)
            (rf result nu-v))
          ))))
   )
  )


(defn z-map-group-1
  ([*v-map kv-f f]

   (let [grouppings (volatile! {})

         group! (fn [x]
                  (let [[k v] (kv-f @grouppings x)]

                    (vswap! grouppings assoc k v)
                    ))
         ]

     (fn [rf]
       (fn
         ([] (rf))

         ([result]
          (vreset! *v-map @grouppings)
          (rf result)
          )
         ([result input]
          (let [nu-v (f input)]
            (group! nu-v)
            (rf result nu-v)
            )
          )
         ([result input & inputs]
          (let [nu-v (apply f input inputs)]
            (group! nu-v)
            (rf result nu-v))
          ))))
   )
  )


;; unique street id - comination of post index + canonical street name
(defn gen-street-id [street]
  (str (:idx street) "_" (:t street) "_" (:ua street)))



(defn street2export [street]
  (let [{t :t
         ua :ua
         ru :ru
         en :en
         alias :alias
         idx :idx
         district :district
         districts :districts
         other :other} street]
    (array-map
      :t t
      :ua ua
      :ru ru
      :en en
      :alias (vec alias)
      :idx idx
      :district district
      :districts districts
      :other (if (nil? other) "" other)
      )
    )
  )


(rum/defc <MAIN-DATA> < rum/static
  [raw-streets]

  [:div

   [:p "[MAIN DATA SOURCE] list all streets, view: visually ensure properties, like uniqueness, correctness, etc."]

   (let [
         ;; external modifiable asserts list, for storing failed assertion
         *asserts (volatile! [])

         ;; assert fns, should return an assertion record if some assertion is failing
         __no-ru-label (fn [item]
                         (if (= "" (:ru item))  {:ID (:ID item) :class #{"no-label"}})
                         )
         __no-idx      (fn [item] (if (= "" (:idx item)) {:ID (:ID item) :class #{"no-idx"}}))


         __by-district    (fn [item] {:ID (:ID item) :class #{(:district item)}})

         ;;__dummy-assert (fn [item] (if (= "11823" (:idx item)) {:ID (:ID item) :class "dummy-assert"}))

         ;; map transducer that checks for assertions also. checks can be combined via juxtaposition (not comp)
         ;assert-filter (partial z-filter *asserts __no-ru-label)

         ;; external grouping for duplicates
         *dups (volatile! {})



         *multi-idx (volatile! {})
         i-rf (fn [a x] (if (nil? a) #{(:i x)} (conj a (:i x))))


         *grannular-streets (volatile! [])
         *all-renamings (volatile! #{})

         ;; *alias
         ;; *all-renamings (volatile! #{})

         ;; single-pass processing of the street list, that can build some additinal meta data via special transducers
         transduced-streets (into [] ; (sorted-set)
                                  (comp
                                    ;; generate unique ID for each street
                                    (map-indexed #(assoc %2 :i %1))
                                    ;
                                    #_(z-map *asserts (apply juxt
                                                             [__no-ru-label
                                                              __no-idx])
                                             #(assoc % :ID (gen-street-id %)))
                                    ;

                                    (data/z-map-1
                                      (data/juxt-mapper __no-ru-label
                                                   __no-idx
                                                   __by-district
                                                   )
                                      #(vswap! *asserts into %)
                                      #(assoc % :ID (:i %)
                                                   ;(gen-street-id %)
                                                ))



                                    #_(data/z-map-1
                                      (data/juxt-mapper
                                        (fn [item]
                                          ;(not (empty? (:alias item)))
                                          true
                                          )
                                        (fn [x]

                                          (let [add-assert! #(vswap! *asserts conj {:ID (:ID x) :class %})]
                                            (if (not (empty? (:alias x)))
                                              (do
                                                ;(.log js/console (:ID x) (:alias x))
                                                (vswap! *all-renamings into (:alias x))
                                                )
                                              )
                                            x
                                            )
                                          #_(let [add-assert! #(vswap! *asserts conj {:ID (:ID x) :class %})
                                                [t s] (ds/ua-geonim (:ua x))
                                                [rt rs] (ds/ru-geonim (:ru x))
                                                [et es] (ds/en-geonim (:en x))

                                                district (:district x)

                                                nu-aliases (map (fn [a]

                                                                  (let [
                                                                        [at uas] (ds/ua-geonim a)
                                                                        [rt rus] (ds/ru-geonim a)
                                                                        [et ens] (ds/en-geonim a)
                                                                        ]

                                                                    (cond
                                                                      ;; should try the other langs
                                                                      (not= "" at)  (do
                                                                                      (if (not= at t)
                                                                                        (add-assert! #{"alias-different-street-type"}))
                                                                                      uas)
                                                                      (not= "" rt) (do
                                                                                     (if (not= rt t)
                                                                                       (add-assert! #{"alias-different-street-type"}))
                                                                                     rus)
                                                                      (not= "" et) (do
                                                                                     (if (not= et t)
                                                                                       (add-assert! #{"alias-different-street-type"}))
                                                                                     ens)
                                                                      :else a
                                                                      )
                                                                    )
                                                                  ) (get x :alias))
                                                ]


                                            (if (not (empty? (:alias x)))
                                              ;
                                              (do
                                                ;(.log js/console (:ID x) (:alias x))
                                                (vswap! *all-renamings into (:alias x))
                                                )

                                              ;(add-assert! #{"ru-empty-street-type"})
                                              )


                                            ;; todo: clean-up
                                            #_(cond
                                                (= "" t) (add-assert! #{"empty-street-type"})
                                                (and (= "" rt) (not= "" (:ru x))) (add-assert! #{"ru-empty-street-type"})
                                                (and (= "" et) (not= "" (:en x))) (add-assert! #{"en-empty-street-type"})

                                                (and (not= t rt) (not= "" (:ru x))) (add-assert! #{"different-steet-types"} )
                                                (and (not= t et) (not= "" (:en x))) (add-assert! #{"different-steet-types"} )
                                                )


                                            (array-map
                                              :t t
                                              :ua s
                                              :ru rs
                                              :en es

                                              :alias (vec nu-aliases)
                                              :idx (:idx x)
                                              :district district
                                              :districts (vec (filter #(not= district %) (:districts x)))
                                              :other (:other x)
                                              )

                                            )
                                          )
                                        )
                                      #(vswap! *grannular-streets into %)
                                      identity)

                                    ;; map-group-dupes
                                    ;(z-map-group *dups :ID (fn [a x] (if (nil? a) 1 (+ a 1))) identity)

                                    (z-map-group-1 *dups
                                                   (fn [groups x]
                                                     (let [z (get groups (:ua x) {:i 0
                                                                                :IDs #{}})]

                                                       [(:ua x) (-> z
                                                                    (update :i inc)
                                                                    (update :IDs conj (:ID x)))]
                                                       )
                                                     ) identity)
                                    ;;
                                    ;;(z-map-group *multi-idx :ua i-rf identity)

                                    ) raw-streets)

         ;; after process - find out duplicated streets and convert this to assertions
         dup-markers (reduce (fn [a [k v]]
                               (if (> (:i v) 1)
                                 (concat a (map #(hash-map :ID % :class #{"dup"}) (:IDs v)))
                                 a)
                               )
                             [] @*dups)

         ;; todo: copy indexes
         all-renamings @*all-renamings
         deletion-markers (reduce
                            (fn [a x]
                              (if (all-renamings (:ua x))
                                (conj a {:ID (:ID x) :class #{"to-be-deleted"}})
                                a
                                )
                              )
                            []
                            transduced-streets
                            )
         ;multi-idx-markers (into []
         ;                        (comp
         ;                          (filter (fn [[k v]] (> (count v) 1)))
         ;                          (mapcat second)
         ;                          (map (fn[x] { :ID (gen-street-id (get-in raw-streets [x])) :class #{"long-street"}}))
         ;                          )
         ;                           @*multi-idx)
         ]
     [:div.flex

      ;; todo:
      #_[:div
         (pg-ui/<edn-list>  ;map <street>
           (sort (data/locale-comparator :district :ua) @*grannular-streets) "")
       ]
      ;(<edn-list> @*grannular-streets "extracted grannular streets")

      #_[:.html (pr-str
                dup-markers
                @*dups
                )]

      (pg-ui/<transform-list> <street> transduced-streets
                        (group-by :ID (concat
                                        deletion-markers
                                        dup-markers
                                        ; multi-idx-markers
                                        @*asserts))
                        :id-fn :ID
                        :copy-fn #(street2export (dissoc % :ID :_rename :i))
                        :sort-fn (data/locale-comparator :ua)
                        :filter-map (merge
                                      {
                                       "Подільський р-н" (partial pg-ui/_marker-class-filter "Подільський р-н")
                                       "Дарницький р-н" (partial pg-ui/_marker-class-filter "Дарницький р-н")
                                       }
                                      {

                                     "dup" (partial pg-ui/_marker-class-filter "dup")
                                     "to-be-deleted" (partial pg-ui/_marker-class-filter "to-be-deleted")
                                     "all-except-deleted" (partial pg-ui/_marker-except-class-filter "to-be-deleted")

                                     })
                        )


      ;; example of simplest reduce
      (let [
              reduced-districts (reduce
                                  (fn [a x]
                                    (conj a (:district x)))
                                  #{} raw-streets
                                  )

              ;; reduce street names but for certain district
              ]
          (pg-ui/<edn-list> reduced-districts "REDUCED DISTRICTS")
          )

      ;; other assertions
      ;; - streets in several districts - "find streets that occur more than once in parsed list: streets that span to multiple districts, etc"
      ;; - multilines in district

      ;; todo: pass groupings into transform list, not for filtering, but for visual grouping


      #_(let [
              ua-geonims-list (->>
                                (group-by :ua raw-streets)
                                (map first)
                                (sort)
                                (map (partial ds/match-geonim ua-geonim-2-short))
                                )

              ;ru-geonims-list (->>
              ;                  (group-by :ru raw-streets)
              ;                  (map first)
              ;                  (sort)
              ;                  (map (partial street-geonim ru-geonim-2-short))
              ;                  )
              ]
          [:div.zzz
           (pg-ui/menubar "extract geonim types" [["copy 📋" (partial wdom/copy-to-clipboard ua-geonims-list)]])
           ; (pr-str geonim-2-short)

           (pg-ui/<edn-list> ua-geonims-list "---")

           ;; check that shortened + geonim is the same as cana

           ;; [:hr]
           #_(let [words (str/split (first (vals street)) #" ")
                   capitalized? (every? (fn [w]
                                          ;(= w (str/capitalize w))
                                          (re-find #"^[А-ЩЬЮЯҐЄІЇ]" w)
                                          ) words)

                   not-a-name-by-first-word  (#{""} (first words))
                   not-a-name-by-second-word (#{"Вал" "Комісарів"} (second words))

                   has-numbers? (some (fn [w]
                                        (re-find #"^\d" w)
                                        ) words)
                   ]

               [:span
                {:style {:color
                         (cond
                           (and has-numbers? capitalized?) "blue"
                           (and capitalized? not-a-name-by-second-word) "red"
                           capitalized? "green"
                           :else "#000"
                           )
                         }}
                (pr-str
                  words
                  capitalized?
                  )]
               )
           ]
          )
      ]
     )
   ]
  )


;;
;; renamings

(defn match-geonim [street-mapping ua]
  (let [[t s] ua]

    (let [words (str/split
                  (->
                    s
                    (str/replace #"’" "'")
                    )
                  " ")

          perms (reduce #(if-let [m (get street-mapping [t (str/join " " %2 )])]
                     (conj %1 m)
                     %1)
                  [] (combo/permutations words))
          ]

      #_(if (> (count perms) 0)
        (.log js/console ua))

      (first perms)
      )
    )
  )

;;


(rum/defc <DRV> < rum/static
  [dict]


  ;; DRV
  (let [

        ;; pre-process streets
        all-streets (get dict :raw-streets [])
        *all-street-ids (volatile! {})
        ref-id-fn (juxt :t :ua)
        enriched-streets (into []
                               (comp
                                 (map-indexed
                                   (fn [i x]
                                     (let [ID (ref-id-fn x)]
                                       (vswap! *all-street-ids update ID (fn [a b]
                                                                           (if a
                                                                             (conj a b)
                                                                             #{b})
                                                                           ) i)
                                       (assoc x
                                         :i i
                                         :ID ID))))
                                 )
                               all-streets)
        street-mapping @*all-street-ids

        ;; drv stuff

        drv-street-map (:drv-buildings-per-street dict)

        ;raw-geonims (:ua-geonims dict)
        ;canonical-streets-map (group-by :id raw-geonims)
        street-aliases (:drv-renamed-streets dict)


        *asserts (volatile! [])

        drv-streets (into []
                          (comp
                            (map-indexed #(hash-map
                                            :i %1
                                            :drv %2
                                            :v (get drv-street-map %2)
                                            :drv-idxs (reduce (fn [a b] (conj a (:idx b))) #{} (get drv-street-map %2))
                                            ))
                            ;;
                            (data/z-map-1
                              (data/juxt-mapper (fn [item]
                                             ;(.log js/console item)
                                             (let [[t s] (:ua item)]
                                               (if (= "" t)
                                                 {:drv (:drv item) :class #{"not-guessed"}}
                                                 #_(if-not (get (:drv-idxs item) (:idx item))
                                                   {:drv (:drv item) :class #{"wrong-idx"}}
                                                   )
                                                 )
                                               )
                                             )
                                           (fn [item]
                                             ;(.log js/console item)
                                             (if-let [ua (:ua item)]
                                               (if-not (:x item)
                                                 {:drv (:drv item) :class #{"cant-match"}}
                                                 (if (> (count (:x item)) 1)
                                                   {:drv (:drv item) :class #{"multi-match"}}
                                                   )
                                                 )
                                               ))
                                           )
                              #(vswap! *asserts into %)
                              (fn [x]

                                (let [ua (ds/ua-geonim (:drv x))]
                                  (assoc x :ua ua
                                           :x (match-geonim street-mapping ua)
                                           )))
                              )

                            )
                          (keys drv-street-map))
        ]

      [:div
       [:h3 "DRV"]

       ;(pg-ui/<edn-list> street-mapping "index of all streets")
       ;[:hr]
       ;(pg-ui/<edn-list> [(first enriched-streets)] "enriched ex")

       (pg-ui/<transform-list> (fn [x]
                                 (let [houses-list (:v x)]
                                   [:.html

                                    #_(if (seq houses-list)
                                      (d/pretty!
                                        (reduce (fn [a b] (conj a (:idx b))) #{} houses-list)
                                        ;(reduce #(into %1 (:idx %2)) {} houses-list)
                                        )
                                      )

                                    [:span.old (:drv x)] " → " [:span.nu (str/join " " (:ua x))] " → " [:span.nu (pr-str (:x x))]
                                    (map
                                      (fn [street]
                                        [:div
                                         (<street> street)
                                         ]
                                        )
                                        (map all-streets (get x :x []))
                                        )
                                    ;(pr-str (select-keys x [:drv :ua]))
                                    ]
                                   )

                                 )
                               drv-streets
                               (group-by :drv @*asserts)
                               :id-fn :drv
                               :filter-map (array-map
                                             ;"wrong idx" (partial pg-ui/_marker-class-filter "wrong-idx")

                                             "geonim: not-guessed" (partial pg-ui/_marker-class-filter "not-guessed")
                                             "geonim: guessed" (partial pg-ui/_marker-except-class-filter "not-guessed")

                                             "streets: cant-match" (partial pg-ui/_marker-class-filter "cant-match")
                                             "streets: matched" (partial pg-ui/_marker-except-class-filter "cant-match")

                                             "streets: multi-match" (partial pg-ui/_marker-class-filter "multi-match")

                                             )
                               )
       ;
       ;(pg-ui/<edn-list> (keys drv-street-map)  "___")
       ;[:hr]
       ;(<edn-list> drv-renamed-streets "___")

       ;[:header "Convert DRV-street to canonical one"]
       ;(<drv-extract-streets> drv-street-map canonical-streets-map)

       ;[:header "List of all buildings per street"]
       #_(<drv-buildings-list> canonical-streets-map
                             (take 100 drv-street-map)
                             )

       ]
      )
  )

(rum/defc <RENAMING-UI> < rum/static
  [dict]

  [:.flex

   ;; STREETS
   (let [

         ;; renamings data source
         ;; renamed-streets-list (get dict :renamed-streets-delta []) ;; -- loaded from file
         renamed-streets-list (reduce

                                 (fn [a [k vs]]
                                   (into a (map (fn [[old nu]]
                                                  (let [[nu-t nu-short] (ds/ua-geonim nu)
                                                        [old-t old-short] (ds/ua-geonim old)]
                                                    (street2export {:t nu-t
                                                                    :ua nu-short
                                                                    :ru ""
                                                                    :en ""
                                                                    :alias [old-short]
                                                                    :idx ""
                                                                    :district k
                                                                    :other (str "renamed from '" old "' to '" nu "'")})
                                                    )
                                                  ) vs))
                                   )
                                 [] (get dict :renamed-streets [])
                                 )

         ; get renaming from alias
         map--add-old (map #(let [cid (:ua %)
                                  [renamed] (:alias %)]
                              (assoc %
                                :cid cid
                                :old renamed
                                )))

         ;; old -> current mapping
         *prev-map (volatile! {})
         prev-k-fn (juxt :district :t :cid)

         ;; current -> old mapping
         *old->nu (volatile! {})
         curr-k-fn (juxt :district :t :old)

         rename-id-fn #(str (:old %) "->" (:cid %))

         i-rf (fn [a x] (if (nil? a) #{(:i x)} (conj a (:i x))))

         renamings (into []
                        (comp
                          map--add-old

                          (map-indexed #(assoc %2 :i %1
                                                  :ID (rename-id-fn %2)))

                          ;; build current/old mappings
                          (z-map-group *prev-map prev-k-fn i-rf identity)
                          (z-map-group *old->nu curr-k-fn i-rf identity)
                          )
                        renamed-streets-list)

         ;; note that we don't filter out data, so the number of items should stay consnant

         group-by-i-xs (fn [f]
                (comp
                  (mapcat second)
                  (map (fn [i]
                         (let [street (get-in renamings [i])]
                           (f street))))
                  ))

         renamed-streets-map @*prev-map


         new-street-ids (into #{} (keys renamed-streets-map))


         ;; MAIN DATA SOURCE
         all-streets (get dict :raw-streets [])

         *all-street-ids (volatile! #{})
         ref-id-fn (juxt :district :t :ua)


         *asserts (volatile! [])
         add-assert! #(vswap! *asserts conj %)


         *alias-i (volatile! {})
         *no-idx-i (volatile! {})


         enriched-streets (into []
                                (comp
                                  (map-indexed
                                    (fn [i x]
                                      (let [ID (ref-id-fn x)]
                                        (vswap! *alias-i assoc (str (:t x) " " (:ua x)) i)

                                        (when (= "" (:idx x))
                                          ;(.log js/console ID (:idx x))
                                          (vswap! *no-idx-i assoc (str (:t x) " " (:ua x)) i)
                                          (add-assert! {:ID ID :class #{"no-idx"}}))

                                        (assoc x
                                          :i i
                                          :ID ID)
                                        )
                                      )
                                    )


                                  #_(data/z-map-1 (fn
                                                  ([] #{})
                                                  ([input] (:ID input))
                                                  ([trans-col input]
                                                   (conj trans-col input)))
                                                #(vswap! )
                                                #(assoc % )
                                                )

                                  ;; for now only new-old-mapping
                                  (data/z-map-1 (fn
                                             ([] #{})
                                             ([input] (:ID input))
                                             ([tran-col input]
                                              (conj! tran-col input)))
                                           #(vswap! *all-street-ids into %)
                                           #(assoc %
                                              :_rename (select-keys renamings (get renamed-streets-map (:ID %))))
                                           )
                                  )
                                all-streets)

         ALL-IDS @*all-street-ids

         obsolete-streets-map @*old->nu
         obsolete-street-ids (into #{} (keys obsolete-streets-map))

         streets2add-ids (set/difference new-street-ids ALL-IDS)

         renamings-2-add (data/map-1-1 renamed-streets-list (select-keys renamed-streets-map streets2add-ids)
                                  #(assoc % :ID (ref-id-fn %)))

         ;; markers for newly added streets
         newly-added__m (into []
                            (map (fn [[[d t _nu] v]]
                                   {:ID [d t _nu] ;[d (first (:alias v))]
                                    :class #{"to-be-added"}} ))
                      renamings-2-add)

         should-be-renamed__m (into []
                              (map (fn [[[d t _nu] v]]
                                     {:ID [d t (first (:alias v))]
                                      :class #{"should-be-renamed"} }))
                              renamings-2-add)

         curr-markers (into []
                            (group-by-i-xs #(hash-map
                                              :ID    (curr-k-fn %)
                                              :class #{"renamed_street_already_in_list"}
                                              ))
                            @*old->nu)

         ;;;
         ;;

         a-index @*alias-i
         upd-no-idx-streets (map (fn [[ID i]]
                                   (when-let [x (get enriched-streets i)]

                                     (let [_ID (str (:t x) " " (get-in x [:alias 0]))]
                                       (if-let [z (get a-index _ID)]
                                         (let [prev-x (get-in enriched-streets [z])

                                               ]
                                           ;; (str ID " -> " _ID " (" i " | " z ") => " (d/pretty! (get-in enriched-streets [z])))
                                           (assoc x
                                             :idx (:idx prev-x)
                                             :alias (concat (:alias x) (filter #(not= "" %) [(:ru prev-x) (:en prev-x)]))
                                             :other (str (:other x) "; " (:other prev-x))
                                             :districts (vec (concat (get x :districts []) (get prev-x :districts [])))
                                             )
                                           )
                                         x
                                         )
                                       )
                                     )
                                   )
                                 @*no-idx-i)

         ]

     [:div.flex


      [:.html

       [:.html
        [:header "upd-no-idx-streets - renamed streets enriched with idx from previous street names"]

        ;(map <street> (sort (data/locale-comparator :district :ua) upd-no-idx-streets))
        ;(pg-ui/<edn-list> (sort (data/locale-comparator :district :ua) upd-no-idx-streets) " copied prev idx")
        (pg-ui/<transform-list> <street>
                          upd-no-idx-streets
                          {}
                          :id-fn :ID
                          :copy-fn #(dissoc % :ID :_rename)
                          :sort-fn (data/locale-comparator :district :ua)
                          )
        ]

       ;; shows old street names present in
       ;(pg-ui/<edn-list> (sort (set/intersection obsolete-street-ids ALL-IDS)) "streets both in MAIN DATA-SOURCE and OLD NAMES")
       ;; already migrated, need to ensure that there is an alias to an old street name
       ;[:hr]
       ;(pg-ui/<edn-list> (sort (set/intersection new-street-ids ALL-IDS)) "new (renamed) streets ALREADY in MAIN DATA-SOURCE")
       ;[:hr]
       ;(pg-ui/<edn-list> (sort streets2add-ids) "new (renamed) streets NOT in MAIN DATA-SOURCE")

       ]

      #_[:.flex
         [:.html
          ;(d/pretty! renamings-2-add)
          (pg-ui/<edn-list> should-be-renamed__m " should-be-renamed__m")

          (pg-ui/<edn-list> newly-added__m " newly-added__m")
          ]
         ;[:.html (<edn-list>  renamings-2-add "street names that were actually renamed \n")]
         ]


      #_[:.html
         "renamed streets that were not in master street list:\n"
         (<transform-list> <street>
                           (vals renamings-2-add)
                           (concat
                             newly-added__m
                             should-be-renamed__m
                             )
                           :id-fn :ID
                           :sort-fn (data/locale-comparator :district :ua)
                           )
         ]


      ;(pg-ui/<edn-list> renamed-streets-list "generated street deltas")

      #_(pg-ui/<edn-list>  (concat enriched-streets
                                 (vals renamings-2-add)
                                 ) "enriched street deltas")
      ;(pg-ui/<edn-list> renamings "enriched street deltas")

      ;[:.html (d/pretty! renamed-streets-map)]
      ;[:.html (d/pretty! (first enriched-streets))]
      ;[:.html (d/pretty! (first (vals renamings-2-add)))]
      ;[:.html (d/pretty! renamings-2-add)]
      ;[:.html (d/pretty! streets2add-ids)]

      [:.html
       [:header "MAIN DATA SOURCE (all streets)"]
       ;; show lit of streets linked with renamed
       (pg-ui/<transform-list> <street>
                         (reduce (fn [all x]
                                   (assoc all (:i x) x))
                                 enriched-streets upd-no-idx-streets)

                         (group-by :ID (concat
                                         newly-added__m
                                         should-be-renamed__m
                                         @*asserts
                                         ;;curr-markers
                                         ))
                         :id-fn :ID
                         ;; ugly, but works
                         :copy-fn #(street2export (dissoc % :ID :_rename :i))
                         :sort-fn (data/locale-comparator :district :ua)
                         :filter-map {"no-idx" (partial pg-ui/_marker-class-filter "no-idx")}
                         )
       ]



      #_[:.html
       [:header "MAIN DATA SOURCE:"]

       (<transform-list> #(do [:p (pr-str %)])
                         ALL-IDS
                         []
                         :id-fn identity
                         :sort-fn compare
                         )
       ;(<edn-list> (sort ALL-IDS) "all available IDS")
       ]
      ]
     )

   #_(let [


           as-geonim (fn [street geonim-map]
                       (let [ks (keys geonim-map)]
                         (merge
                           {

                            :id street
                            }
                           (if (= (count ks) 0)
                             {:t ""
                              :s street
                              }
                             {:t (first (keys geonim-map))
                              :s (first (vals geonim-map))
                              })
                           )

                         )
                       )

           street-geonim (fn [geonim-2-short street]
                           (let [extracted-geonims (reduce (fn [a [geonim sh]]
                                                             (if (nil? (str/index-of street (str " " geonim)))
                                                               a
                                                               (assoc a geonim (str/trim (str/replace street geonim "")))))
                                                           {} geonim-2-short)]

                             ;; todo: how to split
                             #_(if (empty? extracted-geonims)
                                 (.log js/console street)
                                 )
                             (as-geonim street extracted-geonims)
                             )
                           )

           ua-geonims-list (->>
                             (group-by :ua all-streets)
                             (map first)
                             (sort)
                             (map (partial street-geonim ua-geonim-2-short))
                             )

           ru-geonims-list (->>
                             (group-by :ru all-streets)
                             (map first)
                             (sort)
                             (map (partial street-geonim ru-geonim-2-short))
                             )
           ]
       [:div.zzz
        (pg-ui/menubar "extract geonim types" [["copy 📋" (partial copy-geonims ua-geonims-list)]])


        (map <street-name> ua-geonims-list)
        ]
       )

   ;; process raw-streets
   #_(when-let [renamed-streets (:renamed-streets dict)]
       (let [drv-renamed (:drv-renamed-streets dict)
             cstreet-list (:ua-geonims dict)
             cstreet-map (group-by :id cstreet-list)
             ]
         [:.html
          ;(d/pretty! drv-renamed)
          ;;
          (<street-renaming-nu> renamed-streets (keys cstreet-map))
          ]
         ))

   #_(when-let [new-renamed (:renamed-streets-delta dict)]
       (let [raw-streets (:raw-streets dict)]
         [:.html

          (pg-ui/<edn-list> new-renamed)

          ;(pr-str new-renamed)
          ]
         )
       )

   ;"================="


   ;[:hr]


   ;; guessing street names - very slow
   #_(let [
           ks (into #{} (map :ru renamings))

           street-xf  (comp
                        (map (fn [s]
                               (assoc s :matches
                                        (reduce (fn [a n]
                                                  (let [d (jaro (:ua s) n)]
                                                    (if (>= d 0.76 )
                                                      (assoc a n d)
                                                      a
                                                      )
                                                    )
                                                  ) {} ks)
                                        )

                               ))
                        (filter (fn [s] (not (empty? (:matches s)))))
                        )
           ]
       (map (fn [s]
              [:div (pr-str (first (keys (:matches s)))) " -> " (pr-str (:ua s)) ])

            (sequence street-xf
                      (:raw-streets dict)
                      )

            )

       )

   ;[:hr]

   #_(<rename> (apply assoc {}
                      (interleave old nu)))

   ;(d/pretty! (count (interleave old nu)))
   ]
  )



;;
;; command centre

(rum/defcs <streets-cc> < rum/reactive
                          (rum/local
                                ;:RENAME
                                :MASTER-DATA__FULL
                                ; :DRV
                            :UI)
  [st *dict]

  (let [dict @*dict
        *ui (:UI st)
        ui @*ui
        ]

    [:div.streets-pg

     [:.panel
      (pg-ui/menubar "Master Data   "
                     [
                      ["load :raw-streets"    (fn [] (load-edn *dict "/s/streets/streets.edn" :raw-streets))]
                      ["load :ua-geonims"     (fn [] (load-edn *dict "/s/streets/ua_geonims.edn" :ua-geonims))]
                      ])
      ]

     [:.panel
      (pg-ui/menubar "Renaming Data "
                     [
                      ["load :renamed-streets"       (fn [] (load-edn *dict "/s/streets/renamed-streets.edn" :renamed-streets))]
                      ["load :renamed-streets-delta" (fn [] (load-edn *dict "/s/streets/streets_delta.edn" :renamed-streets-delta))]

                      ["load :renamed-ru (2015-2017)" (fn [] (extract-ru-renamings_15_17 *dict))]
                      ["load :renamed-ru (2017-2019)" (fn [] (extract-ru-renamings_17_19 *dict))]
                      ])
      ]

     [:.panel
      (pg-ui/menubar "DRV Data      "
                     [
                      ["load :drv-buildings-per-street" (fn [] (load-edn *dict "/s/drv/_buildings.edn" :drv-buildings-per-street))]
                      ["load :drv-renamed-streets" (fn [] (load-edn *dict "/s/drv/street-renamings_n_alternate_names.edn" :drv-renamed-streets))]
                      ])
      ]

     [:hr]

     (pg-ui/menubar (str (pr-str (into #{} (keys dict))) " UI: ")
                    [
                     [(name ui) (fn []
                                  (swap! *ui {:MASTER-DATA__FULL :EXPORT
                                              :EXPORT :RENAME
                                              :RENAME :DRV
                                              :DRV :MASTER-DATA__FULL})
                                  )]
                     ])
     [:hr]

     [:.flex


      (cond
        (= ui :EXPORT)
        (pg-ui/<edn-list>
          (vec (sort
                 (data/locale-comparator :district :ua) ;; or by :idx
                 (get dict :raw-streets [])))
          " streets EDN, sorted by district and name")

        (= ui :RENAME)            (<RENAMING-UI> dict)

        (= ui :MASTER-DATA__FULL) (<MAIN-DATA>   (get dict :raw-streets []))

        (= ui :DRV) (<DRV> dict)
        )


      #_(when-let [raw-geonims (:ua-geonims dict)]
        (let [canonical-streets-map (group-by :id raw-geonims)
              find-str "Василя Стуса вулиця"
              dist-map (reduce (fn [a n]
                        (let [d (metrics/levenshtein-distance find-str n)]
                          (if (>= d 80)
                            (assoc a n d)
                            a
                            )
                          )
                        ) {} (keys canonical-streets-map))
              ]
          [:.html

           (<rename> dist-map)
           ;(count canonical-streets-map)
           ;(<rename> (take 100 canonical-streets-map))
           ]
          )
        )
      ]
     ]
    )

  )



(rum/defcs <cc-root> < rum/reactive
                       (rum/local {} ::inline-results?)
  [local *wf]

  (let [wf @*wf]
    [:div
     (if (= :not-started (:status wf))
       [:div "wf is not running"]
       (let [*dict (rum/cursor-in *wf [:state ::dict])]
         (try
           (<streets-cc> *dict)
           (catch js/Error e
             [:pre (pr-str e)]))))
     ]))


;;;;;;;;;;;;;;;;;;;;

(defonce *styles-added? (atom false))

;;
;; WF definition
(defn wf! [*SWF]

  (let [CHAN-FACTORY (base/chan-factory (atom {}))
        *dict (rum/cursor-in *SWF [:state ::dict])]
    {

     :title       "Apartment command center"
     :explanation [:div.explanation
                   ;[:p "Analyze scraped data here"]
                   ]

     ;; this state will be added to a wf?
     :state {

             ;; just data for the ui
             ::dict {}
             }

     :init-fns    [
                   { ::*dict *dict }

                   (base/build-init-chan-factory-fn CHAN-FACTORY)
                   (evt-loop/build-evt-loop-init-fn (base/make-chan CHAN-FACTORY (base/rand-sid "evt-")))
                   ]
     ;;
     :ctx-fns     [
                   evt-loop/evt-loop-ctx-fn

                   common/common-ctx ;; re-use common browser step handlers
                   woof-dom/dom-ctx
                   ]

     ;;
     :steps-fns   [
                   (fn [params] { ::#EVT-LOOP# [:evt-loop (evt-loop/&evt-loop params)]})

                   (fn [params]   ;; add css styles only once
                     (if-not @*styles-added?
                       (do (reset! *styles-added? true)
                         { :CSS/custom-css-file [:css-file "http://localhost:9500/css/apt.css"]})
                       {}))
                   ]

     :opt-fns     [
                   (base/build-opts-chan-factory-fn CHAN-FACTORY)
                   ;; uncomment to display wf results (esp. if there was an error)
                   (base/build-opt-on-done (fn [params result]
                                             (.warn js/console params result)))
                   ]

     :ui-fn       (partial wf-ui/<wf-UI> (partial <cc-root>))

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
