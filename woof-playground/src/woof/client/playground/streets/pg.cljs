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
    [woof.data.stat :as stat]
    [woof.utils :as utils]

    [woof.browser :as woof-browser]
    [woof.client.dom :as wdom]

    [woof.client.dom :as woof-dom]
    [woof.client.playground.ui :as pg-ui]
    [woof.client.playground.ui.wf :as wf-ui]
    [woof.client.ws :as ws]

    [woof.wfs.evt-loop :as evt-loop]

    [clj-fuzzy.metrics :as metrics]

    [clojure.core.reducers :as r]
    [clojure.set :as set])

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


(defn safe-compare [k a b]
  (let [ka (k a)
        kb (k b)]
    (if (or (nil? ka)
            (nil? kb))
        (do
          (.log js/console  "can't compare" a ka b kb "key-fn" k)
          0)
        (.localeCompare ka kb)
        )
    )
  )

(defn locale-comparator [k & ks]
  (fn [a b]

    (loop [c1 (safe-compare k a b)
           ks ks]
      (if (not= 0 c1)
        c1
        (if-not (seq ks)
          c1
          (let [k (first ks)]
            (recur (safe-compare k a b) (rest ks)))
          )
        )
      )
    )
  )


;;
;; streets


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
         alias :alias
         :or {alias []}
         } street]

    [:div.street-row
     {:on-click (fn[e] (.log js/console street))
      :class (get street :css "")
      }
     [:div
      [:div
       [:span.tag.small-tag.idx idx]
       [:span.tag.small-tag.district district]
       #_[:span.districts
        (map (fn [d] [:span.small-tag.district {:key (pr-str d)} d]) districts)]
       [:span.aliaes
        (map (fn [d] [:span.tag.small-tag.alias {:key (pr-str d)} d]) alias)]

       ]
      [:.langs
       [:.ua ua]
       [:.ru ru]
       [:.en en]
       ]
      ]

     ;[:.other other]




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
(def kyiv-abbr-map {
                    "ал." "алея"
                    "бульв." "бульвар"
                    "вул." "вулиця"
                    "госп-во" "господарство"
                    "зат." "затока"
                    "ім." "імені"
                    "КДТ" "кооперативне дачне товариство"
                    "кін." "кінця"
                    "м." "міста"
                    "наб." "набережна"
                    "о." "острів"
                    "оз." "озеро"
                    ;;"пл." "площа (площадь)"
                    "пл." "площа"
                    "пер." "переулок"
                    "пол." "половина"
                    "поч." "початок"
                    "пров." "провулок"
                    "просп." "проспект"
                    "р." "річка"
                    "р-н" "район"
                    "рр." "роки"
                    "серед." "середина"
                    "ст." "століття"
                    "СТ" "садове товариство"
                    "СТ КІЗ" "садове товариство колективу індивідуальних забудовників"
                    "СДТ" "садово-дачне товариство"
                    "ТІЗ" "товариство індивідуальних забудовників"
                    "трет." "третина"
                    "туп." "тупик"
                    "ул." "улица"
                    "чв." "чверть"
                    "АТП" "Автотранспортне підприємство"
                    "АНТК" "Авіаційний науково-технічний комплекс"
                    "ВВВ" "Велика вітчизняна війна"
                    "ДВРЗ" "(Дарницький р-н) вагоноремонтний завод"
                    "КБУ-521" "Київське будівельне управління-521"
                    "КЕВРЗ" "Київський електровагоноремонтний завод"
                    "КІБІ" "Київський державний Університет будівництва і архітектури (КІБІ)"
                    "КМЗ" "Київський машинобудівний завод"
                    "РВК" "Районний військовий комісаріат (Райвоєнкомат, РВК)"
                    "РЦАУК" "Республіканський Центральний Автоучкомбінат (РЦАУК)"
                    } )



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
  ) ;; todo:


;; these can be generated automatically from map

(def vul-regex #"вул\.\s" )
(def vul-regex-1 #"вулиця\s" )

(def pl-regex #"пл\.\s" )
(def pl-regex-1 #"площа\s" )
(def prov-regex #"пров\.\s" )
(def prosp-regex #"просп\.\s" )

(def bulv-regex #"бульв\.\s" )

(def prosp-regex-1 #"проспект\s" )
(def shose-regex #"шосе\s" )

;; move get type to be last
(defn noramalize-geonim [street-name]

  ;; maybe this could be optimized by spliting string to words and checking the substitutions on first/last words

  (cond
    (re-find vul-regex street-name) (str (str/replace street-name vul-regex "") " вулиця")
    (re-find vul-regex-1 street-name) (str (str/replace street-name vul-regex-1 "") " вулиця")

    (re-find pl-regex street-name) (str (str/replace street-name pl-regex "") " площа")
    (re-find pl-regex-1 street-name) (str (str/replace street-name pl-regex-1 "") " площа")

    (re-find prov-regex street-name) (str (str/replace street-name prov-regex "") " провулок")

    (re-find prosp-regex street-name) (str (str/replace street-name prosp-regex "") " проспект")
    (re-find prosp-regex-1 street-name) (str (str/replace street-name prosp-regex-1 "") " проспект")

    (re-find bulv-regex street-name) (str (str/replace street-name bulv-regex "") " бульвар")

    (re-find shose-regex street-name) (str (str/replace street-name shose-regex "") " шосе")
    :else street-name
    )
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
                     (let [_c-old-name (noramalize-geonim k)


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

                           c-new-name (noramalize-geonim v)
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





(rum/defc <edn-list> < rum/static
  [edn h]

  (let [t (pr-str (type edn))
        EDN-STR (reduce
                  str
                  ""
                  (concat
                    (get OPENING-BRACKETS t (str "!!!" t)) "\n"
                    (map
                      #(str " " (pr-str %) "\n") edn)
                    (get CLOSING-BRACKETS t (str "!!!" t))
                    )

                  )
        ]
    [:.html
     (pg-ui/menu-item "copy" (partial woof-dom/copy-to-clipboard EDN-STR))
     (if h (str " ;; " h "\n") "\n")
     EDN-STR
     ]
    )
  )

(rum/defc <street-name> < rum/static
                          { :key-fn (fn [m] (str (:idx m) "_" (:ua m) "_" (:district m) ))}
  [street & {:keys [check-fn]}]

  [:div.street-row

   #_(if-let [{old-name :old-name
             nu :new} (:rename street)]

     (cond
       ;(and old-name nu) [:span.tag.idx (str (pr-str old-name) "->" (pr-str nu))]
       (and (not old-name) nu) [:span.tag.houses (str "->" (pr-str nu))]
       (and old-name (not nu) ) [:span.tag.idx (str (pr-str old-name) "->" )]
       )

     )

   (if check-fn
     (check-fn street))

   (if-let [alias (:alias street)]
     (map
       #(do [:span.tag.idx %]) alias)
      )

   (:ua street)
   ;(pr-str street)

   ;; check that shortened + geonim is the same as cana
   #_(if (not= (full-street-name street)
               (:id street))
       [:.html {:style {:color "blue"}}
        (full-street-name street) "\n"
        (pr-str street)
        ]
       )

   ;;


   ;[:.html (pr-str (:g geonim-map))]

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
          )
        ]

       )


   #_(<rename> (reduce (fn [a [geonim sh]]
                         (if (not= (str/index-of street geonim) -1 )
                           (assoc a geonim street )
                           a)
                         ) {} ua-geonim-2-short))

   #_(map (fn [[sh geonim]]
            (<rename>)
            )
          ua-geonim-2-short
          )

   ;(pr-str (vals geonim-2-short))

   ]
  )


(defn extract-ru-renamings [*dict]
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

(defn z-map-1
  ([meta-xf persist-fn f]
   (let [_meta-results (transient (meta-xf)) ;; always use vector for meta log
         add-meta! (fn [input] (meta-xf _meta-results (meta-xf input)))
         ]
     (fn [rf]
       (fn
         ([] (rf))
         ([result]
          (persist-fn (persistent! _meta-results))
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





(rum/defcs <transform-list> < rum/static
                              (rum/local nil ::filter)
  [st <item> items markers & {:keys [id-fn sort-fn copy-fn] :or {id-fn identity
                                                              copy-fn identity}}]

  (let [style-map (z-group id-fn (fn [a x]
                                   (if (nil? a)
                                     (:class x)
                                     (into a (:class x))
                                     )
                                   )  markers)

        available-styles_comb (filter
                                #(or (string? %) (not= 1 (count %)))
                                (apply conj (vals style-map)))

        *filter (::filter st)
        _ (if (nil? @*filter) (reset! *filter (first available-styles_comb)))
        filter-id @*filter


        &style (fn [item]
                 (get style-map (id-fn item)))


        show-all? (= :all filter-id)

        filter-rule (if (string? filter-id)
                      (filter (fn [item] (or show-all? (contains? (&style item) filter-id))))
                      (filter (fn [item]
                                (let [st (&style item)]
                                  (or show-all? (empty? (set/difference filter-id st))))))
                      )

        sorted-items (if sort-fn
                       (sort sort-fn items)
                       items)

        ]


    [:div.list

     ;; todo: combinations
     (pg-ui/menubar "filters: "
                    (into
                      [
                       ["copy" (fn []
                                 (woof-dom/copy-to-clipboard
                                   (str "[\n" (str/join "\n"
                                                        (into []
                                                              (comp filter-rule
                                                                    (map copy-fn)
                                                                    (map pr-str))
                                                              sorted-items)) "\n]")
                                   )
                                 )]
                       []
                       ["all" (fn [] (reset! *filter :all))][]]
                      (map #(do [(pr-str %) (fn [] (reset! *filter %)) ]) available-styles_comb)
                      )
                    )

     #_[:.html
      (pr-str filter-id)
      ;(<edn-list> markers "markers")
      ;"\n===\n"
      ;(d/pretty! style-map)
      ;"\n====\n"
      ;(d/pretty! available-styles)
      ]

     (into [:.items]
           (comp
             filter-rule
             (map (fn [item]
                    ;[:..html
                    ; (str "item:\t" (pr-str item) "\n")
                    ; (str "ID:\t" (id-fn item) "\n")

                    (if-let [c (&style item)]
                      [:.item {:class (str/join " "  c)}
                       (<item> item)]

                      (<item> item))
                    ; ]
                    )
             ))
           sorted-items
           )
     ]
    )
  )


;; unique street id - comination of post index + canonical street name
(defn gen-street-id [street]
  (str (:idx street) "_" (:ua street)))


(rum/defc <MAIN-DATA> < rum/static
  [raw-streets]

  (let [*inv (volatile! [
                         ;; event { :id ... , :check "zzz" }
                         ])]
    [:div

     [:p "[street.. ] view: visually ensure properties, like uniqueness, correctness, etc."]

     (let [
           podil? #(= "Подільський р-н" (:district %))
           podil-xf (filter podil?)

           ;; external modifiable asserts list, for storing failed assertion
           *asserts (volatile! [])

           ;; assert fns, should return an assertion record if some assertion is failing
           __no-ru-label (fn [item] (if (= "" (:ru item))  {:ID (:ID item) :class #{"no-label"}}))
           __no-idx      (fn [item] (if (= "" (:idx item)) {:ID (:ID item) :class #{"no-idx"}}))

           ;;__dummy-assert (fn [item] (if (= "11823" (:idx item)) {:ID (:ID item) :class "dummy-assert"}))

           ;; map transducer that checks for assertions also. checks can be combined via juxtaposition (not comp)
           assert-map (partial z-map *asserts (apply juxt
                                                     [__no-ru-label
                                                      __no-idx]))

           ;assert-filter (partial z-filter *asserts __no-ru-label)


           ;; external grouping for duplicates
           *dups (volatile! {})

           groupping-rf (fn [a x] (if (nil? a) 1 (+ a 1)))
           map-group-dupes (partial z-map-group *dups :ID groupping-rf)

           *multi-idx (volatile! {})
           i-rf (fn [a x] (if (nil? a) #{(:i x)} (conj a (:i x))))


           ;; single-pass processing of the street list, that can build some additinal meta data via special transducers
           transduced-streets (into [] ; (sorted-set)
                                    (comp
                                      ;; generate unique ID for each street
                                      (map-indexed #(assoc %2 :i %1))
                                      (assert-map #(assoc % :ID (gen-street-id %))) ;; at the same time

                                      ;; normal filter (filter podil?)
                                      ;; ;; or special one with logging
                                      ;; (assert-filter podil?) ;; todo: add useful assertion

                                      (map-group-dupes identity)

                                      (z-map-group *multi-idx :ua i-rf identity)

                                      ) raw-streets)


           ;; after process - find out duplicated streets and convert this to assertions
           dup-markers (reduce (fn [a [k v]]
                                 (if (> v 1) (conj a {:ID k :class #{"dup"}})
                                             a))
                               [] @*dups)

           multi-idx-markers (into []
                                   (comp
                                     (filter (fn [[k v]] (> (count v) 1)))
                                     (mapcat second)
                                     (map (fn[x] { :ID (gen-street-id (get-in raw-streets [x])) :class #{"long-street"}}))
                                     )
                                      @*multi-idx)
           ]
       [:div.flex

        ;; example of simplest reduce
        #_(let [
              reduced-districts (reduce
                                  (fn [a x]
                                    (conj a (:district x)))
                                  #{} raw-streets
                                  )

              ;; reduce street names but for certain district
              ]
          (<edn-list> reduced-districts "REDUCED DISTRICTS")
          )

        ;; other assertions
        ;; - streets in several districts - "find streets that occur more than once in parsed list: streets that span to multiple districts, etc"
        ;; - multilines in district

        ;; todo: pass groupings into transform list, not for filtering, but for visual grouping


        (let [
              as-geonim (fn [street geonim-map]
                          (let [ks (keys geonim-map)]
                            (merge
                              {
                               :id street
                               }
                              (if (= (count ks) 0)
                                {:t       ""
                                 :s street
                                 }
                                {:t (first (keys geonim-map))
                                 :s (first (vals geonim-map))
                                 })
                              )

                            )
                          )

              extract-geonims (fn [geonim-map street]
                                (reduce (fn [a [geonim sh]]
                                          (if (nil? (str/index-of street (str " " geonim)))
                                            a
                                            (assoc a geonim (str/trim (str/replace street geonim "")))))
                                        {} geonim-map)
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
                                (group-by :ua raw-streets)
                                (map first)
                                (sort)
                                (map (partial street-geonim ua-geonim-2-short))
                                )


              ru-geonims-list (->>
                                (group-by :ru raw-streets)
                                (map first)
                                (sort)
                                (map (partial street-geonim ru-geonim-2-short))
                                )
              ]
          [:div.zzz
           (pg-ui/menubar "extract geonim types" [["copy 📋" (partial wdom/copy-to-clipboard ua-geonims-list)]])
           ; (pr-str geonim-2-short)

           (<edn-list> ua-geonims-list "---")
           ;(map <street-name> ua-geonims-list)
           ]

          )


        #_(<transform-list> <street> transduced-streets
                          #_(concat dup-markers
                                  multi-idx-markers
                                  @*asserts)
                          @*asserts
                          :id-fn :ID
                          :sort-fn (locale-comparator :ID))

        ]
       )
     ]
    )
  )


;;
;; renamings



(def ru-ua-mapping {
                    "б-р." "бульвар"
                    "пл." "площа"
                    "пер." "провулок"
                    "пр-т." "проспект"
                    "пр." "проспект"
                    "ул." "вулиця"
                    })

(def ru-canonized-gt-mapping {
                              "б-р." "бульвар"
                              "пл." "пл."
                              "пер." "пер."
                              "пр-т." "просп."
                              "пр." "просп."
                              "ул." "ул."
                              })


(defn map-1-1 [source-vector index-map f]
  (reduce (fn [a [k v]]
            (let [vs (map f (vals (select-keys source-vector v)))]
              (if (= 1 (count vs))
                (assoc a k (first vs))
                (do
                  (.log js/console "non 1-1 mapping" k vs)
                  (assoc a k vs)
                  )

                )
              )

            ) {} index-map)
  )

(rum/defc <DRV> < rum/static
  [dict]


  ;; DRV
  (let [drv-street-map (:drv-buildings-per-street dict)

          ;raw-geonims (:ua-geonims dict)
          ;canonical-streets-map (group-by :id raw-geonims)
          street-aliases (:drv-renamed-streets dict)

          ]

      [:div
       [:h3 "DRV"]
       ;(<edn-list> (keys drv-street-map)  "___")
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

   #_[:.html
    "DATA:\n"

    "\n"
    [:h3 "what renamings are already actualized?"]
    "\n"

    "\n:renamed-streets - \n"
    "\tdistrict {old-ua => new-ua}\n"
    "\told-ua/new-ua - non-canonical\n"


    "\n:renamed-streets-delta - \n"
    "\t..."
    "\t:ua - renamed canonical name"
    "\t:alias - prev canonical name"
    ; (<edn-list> (get dict :renamed-streets-delta []) "")


    "\n:renamed-ru - list extracted RU street names (old-RU->new-RU)"
    "\n\told-RU => in canonic format"
    "\n\tnew-RU => non-canonic"
    ;;(<edn-list> rename-pairs "")

    ]



   #_(let [rename-pairs (get dict :renamed-ru [])
         xf (comp
              (filter (fn [[old nu]] (re-find #"\(.+\)$" nu)))
              (map (fn [[old nu]] (re-find #"\(.+\)$" nu)))
              )


         ;; todo: do smth with these guesses
         guesses {
                  }

         renamings (map (fn [[old nu]]
                          (let [_distr (re-find #"\(.+\)$" nu)
                                distr (if _distr
                                        ({"(Дарницкий район)"   "Дарницький р-н"
                                          "(Дарницкий)"         "Дарницький р-н"
                                          "(Деснянский район)"  "Деснянський р-н"
                                          "(Деснянский)"        "Деснянський р-н"
                                          "(Днепровский район)" "Дніпровський р-н"
                                          "(Печерский)"         "Печерський р-н"
                                          "(Подол)"             "Подільський р-н"
                                          "(Соломенский район)" "Солом'янський р-н"
                                          "(Соломенский)"       "Солом'янський р-н"
                                          } _distr)
                                        )
                                nu-1 (if _distr
                                       (str/trim (str/replace nu #"\(.+\)$" ""))
                                       nu)

                                parts (str/split old " ")
                                raw-gt (last parts)

                                ]
                            ;(last parts)

                            {
                             :distr distr
                             :gt (get ru-ua-mapping raw-gt)
                             :ru-gt (get ru-canonized-gt-mapping raw-gt)
                             :ru nu-1
                             ; :ru1 (str nu-1 " " (get ru-canonized-gt-mapping raw-gt))
                             ;; :_old old

                             :guess (get guesses nu-1)
                             :old (str/join " " (drop-last parts))
                             }
                            )
                          ) rename-pairs)

         ;rrr (into [])
         ]


     [:.html

      "process extracted RU names:\n"
      "\textract street type\n"
      "\textract district from street name\n\n"

      (<edn-list> renamings "AA")

      (map
        (fn [z]
          [:div.html.street-row
           (.padEnd (pr-str (:guess z) ) 40)
           " "
           (pr-str (str (:ru z) " " (:ru-gt z) ) )]
          )
        (sort-by :guess
                 (filter #(not (empty? (:guess %))) renamings))
        )

      #_(pr-str (reduce #(conj %1 (:district %2)) #{} (:raw-streets dict)))
      #_(<edn-list> (sequence xf rename-pairs))
      #_#{"Солом'янський р-н" "Подільський р-н" "Голосіївський р-н" "Шевченківський р-н" "Деснянський р-н" "Дарницький р-н" "Печерський р-н" "Оболонський р-н" "Святошинський р-н" "Дніпровський р-н"}


      ]

     )


   ;; STREETS
   (let [

         ;; renamings data source

         renamed-streets-list (get dict :renamed-streets-delta [])

         ; get renaming from alias
         map--add-old (map #(let [cid (:ua %)
                                  [renamed] (:alias %)]
                              (assoc %
                                :cid cid
                                :old renamed
                                )))

         ;; old -> current mapping
         *prev-map (volatile! {})
         prev-k-fn (juxt :district :cid)

         ;; current -> old mapping
         *old->nu (volatile! {})
         curr-k-fn (juxt :district :old)

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
         ref-id-fn (juxt :district :ua)
         enriched-streets (into []
                                (comp
                                  (map-indexed #(assoc %2 :i %1
                                                          :ID (ref-id-fn %2)))

                                  ;; for now only new-old-mapping
                                  (z-map-1 (fn
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

         renamings-2-add (map-1-1 renamed-streets-list (select-keys renamed-streets-map streets2add-ids)
                                  #(assoc % :ID (ref-id-fn %)))

         ;; markers for newly added streets
         newly-added__m (into []
                            (map (fn [[[d _nu] v]]
                                   {:ID [d _nu] ;[d (first (:alias v))]
                                    :class #{"to-be-added"}} ))
                      renamings-2-add)

         should-be-renamed__m (into []
                              (map (fn [[[d _nu] v]]
                                     {:ID [d (first (:alias v))]
                                      :class #{"should-be-renamed"} }))
                              renamings-2-add)

         curr-markers (into []
                            (group-by-i-xs #(hash-map
                                              :ID    (curr-k-fn %)
                                              :class #{"renamed_street_already_in_list"}
                                              ))
                            @*old->nu)


         ]

     [:div.flex


      #_[:.html
       [:header "RENAME & MAIN STREETS:"]

       ;; shows old street names present in
       (<edn-list> (sort (set/intersection obsolete-street-ids ALL-IDS)) "streets both in MAIN DATA-SOURCE and OLD NAMES")

       ;; already migrated, need to ensure that there is an alias to an old street name
       [:hr]
       (<edn-list> (sort (set/intersection new-street-ids ALL-IDS)) "new (renamed) streets ALREADY in MAIN DATA-SOURCE")

       ;;
       [:hr]
       (<edn-list> (sort streets2add-ids) "new (renamed) streets NOT in MAIN DATA-SOURCE")

       ]

      #_[:.flex
         [:.html
          ;(d/pretty! renamings-2-add)
          (<edn-list> should-be-renamed__m " should-be-renamed__m")

          (<edn-list> newly-added__m " newly-added__m")
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
                           :sort-fn (locale-comparator :district :ua)
                           )
         ]


      ;; show lit of streets linked with renamed
      (<transform-list> (fn [street]
                          [:div.html
                           ;(pr-str (:_rename street))
                           ; (pr-str (:ID street))
                           (<street> street)
                           ]
                          )
                        (concat enriched-streets
                                (vals renamings-2-add))

                        (concat
                          newly-added__m
                          should-be-renamed__m
                          ;;curr-markers
                          )
                        :id-fn :ID
                        :copy-fn #(dissoc % :ID)
                        ;:sort-fn (locale-comparator :district :ua)
                        )


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

          (<edn-list> new-renamed)

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
                                ;; :RENAME
                                :MASTER-DATA__FULL
                                ;:DRV
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

                      ["load :renamed-ru (2015-2017)" (fn [] (extract-ru-renamings *dict))]
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
        (<edn-list>
          (vec (sort
                 (locale-comparator :district :ua) ;; or by :idx
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
                        (let [d (stat/levenshtein-distance find-str n)]
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



(rum/defcs <scraping-root> < rum/reactive
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
             [:pre (pr-str e)]
             )

           )
         )
       )
     ]
    )
  )


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
             ::dict {
                     }
             }

     :init-fns    [
                   { ::*dict *dict }

                   (base/build-init-chan-factory-fn CHAN-FACTORY)
                   (evt-loop/build-evt-loop-init-fn (base/make-chan CHAN-FACTORY (base/rand-sid "evt-")))
                   ]
     ;;
     :ctx-fns     [
                   evt-loop/evt-loop-ctx-fn

                   woof-browser/common-ctx ;; re-use common browser step handlers
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
