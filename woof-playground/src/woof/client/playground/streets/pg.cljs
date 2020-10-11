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




(defn copy-geonims [t-map]
  (let [
        clipboard js/navigator.clipboard
        copy-handler (fn []
                       (-> (.writeText clipboard (d/pretty! t-map))
                           (.then (fn [response] (.log js/console "Copied to clipboard - " response))
                                  (fn [err] (.warn js/console "Failed to copy to clipboard" err)))))
        ]
    (copy-handler)))






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


(rum/defc <edn-list> < rum/static
  [edn]
  [:.html

   "[\n"
   (map
     #(str " " (pr-str %) "\n")
     edn
     )
   "]"

   ;(pr-str new-renamed)
   ]
  )

(rum/defc <street-name> < rum/static
                          { :key-fn (fn [m] (str (:idx m) "_" (:ua m) "_" (:district m) ))}
  [street]

  [:div.street-row

   #_(if-let [{old-name :old-name
             nu :new} (:rename street)]

     (cond
       ;(and old-name nu) [:span.tag.idx (str (pr-str old-name) "->" (pr-str nu))]
       (and (not old-name) nu) [:span.tag.houses (str "->" (pr-str nu))]
       (and old-name (not nu) ) [:span.tag.idx (str (pr-str old-name) "->" )]
       )

     )

   (if-let [alias (:alias street)]
     (map
       #(do [:span.tag.idx %]) alias)
      )

   (pr-str street)

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


(defn extract-ru-renamings []
  (let [(wdom/q "#") ])
  )

(rum/defcs <streets-cc> < rum/reactive
  [st *dict]
  (let [dict @*dict]
    [:div  {:style {:padding-top "1rem"}}
     (pg-ui/menubar "Streets: "
                    [
                     ["MAIN: load streets" (fn [] (load-edn *dict "/s/streets/streets.edn" :raw-streets))]
                     ["MAIN: load renamed streets" (fn [] (load-edn *dict "/s/streets/renamed-streets.edn" :renamed-streets))]

                     ["MAIN: load renamed streets DELTA" (fn [] (load-edn *dict "/s/streets/streets_delta.edn" :renamed-streets-delta))]


                     ["MAIN: load parsed geonims" (fn [] (load-edn *dict "/s/streets/ua_geonims.edn" :ua-geonims))]

                     ["DRV: load buildings" (fn [] (load-edn *dict "/s/drv/buildings.edn" :drv-buildings-per-street))]
                     ["DRV: load renamings" (fn [] (load-edn *dict "/s/drv/street-renamings_n_alternate_names.edn" :drv-renamed-streets))]

                     ["RENAME: get ru names" (fn []
                                               (extract-ru-renamings)
                                               )]
                     ])

     [:.flex

      ;; STREETS
      ;; city streets to canonical form
      (when-let [raw-streets (:raw-streets dict)]

        (let [streets-delta (get dict :renamed-streets-delta [])

              delta (map #(let [cid (:ua %)
                          [renamed] (:alias %)
                          district (:district %)]
                      {:district district
                       :cid      cid
                       :old      renamed})
                streets-delta)

              cid->old (reduce (fn [a [k v]]
                                                  (assoc a k (:old (first v))))
                                       {} (group-by (juxt :district :cid) delta))

              old->cid (reduce (fn [a [k v]]
                                         (assoc a k (:cid (first v))))
                                       {} (group-by (juxt :district :old) delta))



              all-streets (into #{} (group-by :ua raw-streets))


              upd-streets (map
                #(let [{d :district
                        cstreet :ua} %
                       prev (get cid->old [d cstreet])

                       rename (get old->cid [d cstreet])
                       ]
                   (cond
                     rename (assoc % :alias (vals (select-keys % [:ua :ru :en]))
                                     :ua rename
                                     :ru ""
                                     :en ""
                                     )
                     prev   (assoc % :alias [prev] )
                     :else %
                     )
                   )
                raw-streets)
              ]
          [:div.html


           ;(<rename> district-str-new)
           ;[:hr]
           ;(<rename> district-str-old)


           (let [untranslated-streets (filter (fn [sm]
                                                (= "" (:ru sm))
                                                ) upd-streets)]

             (map #(do
                     [:div (:ua %) "\n"]
                     ) untranslated-streets)
             )


           #_(map <street-name> (filter (fn [sm]
                                        (= "" (:ru sm))
                                        ) upd-streets))


           ;(<edn-list> upd-streets)

           #_(<edn-list>
             (map
               #(let [{d :district
                       cstreet :ua} %
                      renamed (get cid->old [d cstreet])
                      ]
                  (if renamed
                    (assoc % :rename renamed)
                    %
                    )
                  ;[cstreet renamed (contains? all-streets renamed)]
                  )
               raw-streets))
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
             (pg-ui/menubar "extract geonim types" [["copy 📋" (partial copy-geonims ua-geonims-list)]])


             (map <street-name> ua-geonims-list)
             ]
            )
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


      ;; DRV
      #_(let [drv-street-map (:drv-buildings-per-street dict)

            raw-geonims (:ua-geonims dict)
            canonical-streets-map (group-by :id raw-geonims)

            street-aliases (:drv-renamed-streets dict)

            ]

        [:div
         [:h3 "DRV"]

         [:header "Convert DRV-street to canonical one"]
         (<drv-extract-streets> drv-street-map canonical-streets-map)

         [:header "List of all buildings per street"]
         (<drv-buildings-list> canonical-streets-map
                               (take 100 drv-street-map)
                               )

         ]
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



     #_(when-let [raw-streets (:raw-streets dict)]
         [:div
          ;; uncomment the checks

          (let [streets (->> raw-streets
                             ;(take 40 raw-streets)
                             (filter (fn [street]
                                       ;(= "Подільський р-н" (:district street))
                                       (= "Дарницький р-н" (:district street))
                                       ))
                             )]
            [:div {:style {:max-width "75%"}}


             (pg-ui/menubar "podil streets"
                            [["copy 📋" (fn []
                                          (let [clipboard js/navigator.clipboard
                                                copy-handler (fn []
                                                               (-> (.writeText clipboard (d/pretty! streets))
                                                                   (.then (fn [response] (.log js/console "Copied to clipboard - " response))
                                                                          (fn [err]      (.warn js/console "Failed to copy to clipboard" err))))
                                                               )
                                                ]
                                            (copy-handler)))]
                             ])

             (map <street> streets)

             ]
            )



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


          ;; list all districts
          (let [all-districts (reduce (fn [a x]
                                        ;; take main one
                                        ;(conj a (:district x))
                                        ;; take all
                                        (into a (:districts x))
                                        ) #{} raw-streets)]
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


          #_[:div

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

  )



(rum/defcs <scraping-root> < rum/reactive
                             (rum/local {} ::inline-results?)
  [local *wf]

  (let [wf @*wf]
    [:div
     (if (= :not-started (:status wf))
       [:div "wf is not running"]
       (let [*dict (rum/cursor-in *wf [:state ::dict])]
         (<streets-cc> *dict))
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

     :title       "Streets command center"
     :explanation [:div.explanation
                   ;[:p "Analyze scraped data here"]
                   ]

     ;; this state will be added to a wf?
     :state {

             ;; just data for the ui
             ::dict {
                     :raw-streets []

                     :streets []
                     ;:renamed-streets []

                     :drv-buildings-per-street {}
                     :drv-renamed-streets {}

                     :ua-geonims {}
                     }

             }

     :init-fns    [

                   {
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
                   ]

     ;;
     :steps-fns   [(fn [params]
                     {
                      ::#EVT-LOOP# [:evt-loop (evt-loop/&evt-loop params)]

                      ;; ::hello [:log "Hello"]
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
