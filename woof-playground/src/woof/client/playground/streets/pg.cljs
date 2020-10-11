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
(def pl-regex #"пл\.\s" )
(def pl-regex-1 #"площа\s" )
(def prov-regex #"пров\.\s" )


(defn noramalize-geonim [geonim]
  (cond
    (re-find vul-regex geonim) (str (str/replace geonim vul-regex "") " вулиця")
    (re-find pl-regex geonim) (str (str/replace geonim pl-regex "") " площа")
    (re-find pl-regex-1 geonim) (str (str/replace geonim pl-regex-1 "") " площа")
    (re-find prov-regex geonim) (str (str/replace geonim prov-regex "") " провулок")
    :else geonim
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


(rum/defc <street-renaming> < rum/static
  [dict renamed-streets raw-streets]
  ; "Подільський р-н"
  ;"Голосіївський р-н"
  ;"Солом’янський р-н"
  ;"Шевченківський р-н"
  ;"Деснянський р-н"
  ;"Дарницький р-н"
  ;"Печерський р-н"
  ;"Оболонський р-н"
  ;"Святошинський р-н"
  ;"Дніпровський р-н"
  (let [district "Дніпровський р-н";"Голосіївський р-н"
        podil-renamed (get renamed-streets district)

        grouped-streets (->>
                          (group-by :district raw-streets))

        podil-streets (get grouped-streets district)

        existing-names (into #{}
                             (map #(get % :ua)
                                  podil-streets)
                             )

        old-geonims (sort (map noramalize-geonim (keys podil-renamed)))
        ]

    [:div
     (map (fn [old-geo]
            (let [old-normalized (noramalize-geonim old-geo)
                  nu (get podil-renamed old-geo)
                  nu-normalized (noramalize-geonim nu)
                  ]
              [:div.flex

               (<rename> {
                          old-geo old-normalized
                          nu nu-normalized

                          old-normalized nu-normalized
                          })

               (if (get existing-names old-normalized)
                 [:div "OK"]
                 (do

                   [:div {:style {:color "red"}}
                    (<rename> (reduce (fn [a n]
                                        (let [d (stat/levenshtein-distance old-normalized n)]
                                          (if (>= d 80)
                                            (assoc a n d)
                                            a
                                            )
                                          )
                                        ) {} existing-names))
                    #_(map (fn [n]
                             [:div
                              n old-normalized
                              (pr-str (levenshtein-distance n old-normalized))]
                             ) existing-names)
                    "X"]
                   )
                 )
               ]
              )
            )
          (keys podil-renamed))
     ]
    )
  )


(defn full-street-name [geonim-model]
  (str/trim (str (:s geonim-model) " " (:t geonim-model))))

(rum/defc <street-name> < rum/static
                          { :key-fn (fn [m] m)}
  [geonim-model]

  [:div.street-row

   ;; check that shortened + geonim is the same as cana
   (if (not= (full-street-name geonim-model)
             (:id geonim-model))
     [:.html {:style {:color "blue"}}
      (full-street-name geonim-model) "\n"
      (pr-str geonim-model)
      ]
     )


   ;;
   (pr-str geonim-model)

   ;[:.html (pr-str (:g geonim-map))]

   ;; [:hr]
   #_(let [words (str/split (first (vals geonim-model)) #" ")
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
  [geonims street]

  (let [parts (:short street)
        gt (:g street)
        multi-word? (> (count parts) 1)
        mathcing-perms (:matches street)
        has-matches? (> (count mathcing-perms) 0)
        ]
    [:div {:style {} #_{:color (if has-matches?
                                 "grey"
                                 "red"
                                 )}

           }

     [:header [:span.tag gt] (:str street) " " (:street street)]

     (if-not has-matches?
       [:.html {:style {:color "red"}}
        (d/pretty!
          (reduce (fn [a s]
                    (let [canonical (str  (str/join " " s) " " gt)]
                      (if-let [link (get geonims canonical)]
                        (assoc a (:str street) link)
                        (assoc a canonical false)
                        ))) {} (combo/permutations parts))
          )
        ])

     (map (fn [[k v]]
            [:div (pr-str k) " -> " (pr-str v)]
            )
          mathcing-perms
          )
     #_(if multi-word?
         )
     (pr-str street)

     ])
  )

(rum/defc <drv-canonalize-streets> < rum/static
  [street-builings geonims]

  (let [streets-to-canonize (keys street-builings)

        short-2-full (assoc (clojure.set/map-invert ua-geonim-2-short)
                       "тупик" "тупик"
                       "смт" "смт"
                       "набережна" "набережна")

        candidates (->>
                     streets-to-canonize
                     (map (partial _drv-guess-geonim short-2-full))

                     (map #(let [guess-street (:street %)

                                 gt (:g %)
                                 parts (:short %)

                                 multi-word? (> (count parts) 1)
                                 mathcing-perms (reduce (fn [a s]
                                                          (let [canonical (str  (str/join " " s) " " gt)]
                                                            (if-let [link (get geonims canonical)]
                                                              (assoc a (:str %) link)
                                                              a ;(assoc a canonical false) ;; a
                                                              ))) {} (combo/permutations parts))


                                 ]
                             (merge
                               %
                               {:matches mathcing-perms}
                               (if guess-street
                                 {:f/guess? :guessed}
                                 {:f/guess? :not-guessed})

                               (if (empty? mathcing-perms)
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
           (pr-str (first geonims))
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
              (d/pretty! (keys geonims))
              ]

           [:header "non matching"]
           (map (partial <street-1> geonims) (take 100 non-matching))

           ;(pr-str (count matching))
           ]
          )
        ]
       )



     #_(let [not-matching (get candidates :not-matching)]
         [:.html

          (str "not matching: " (count not-matching) " vs all: " (count streets-to-canonize))

          #_(map
              #(if-let [geonim (get geonims %)]
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
              #(if-let [geonim (get geonims %)]
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

(rum/defc <drv-buildings-list> < rum/static
  [drv-street-map]

  [:div

   (map <drv-street> drv-street-map)



   ]
  )


(rum/defcs <streets-cc> < rum/reactive
  [st *dict]
  (let [dict @*dict]
    [:div {:style {:padding-top "1rem"}}
     (pg-ui/menubar "Streets: "
                    [
                     ["load pre-parsed streets" (fn [] (load-edn *dict "/s/streets/streets.edn" :raw-streets))]
                     ["load renamed streets" (fn [] (load-edn *dict "/s/streets/renamed-streets.edn" :renamed-streets))]

                     ["load parsed geonims" (fn [] (load-edn *dict "/s/streets/ua_geonims.edn" :ua-geonims))]

                     ["DRV: load buildings" (fn [] (load-edn *dict "/s/drv/buildings.edn" :drv-buildings-per-street))]
                     ["DRV: load renamings" (fn [] (load-edn *dict "/s/drv/street-renamings_n_alternate_names.edn" :drv-renamed-streets))]
                     ])

     [:.html
      ; "довідник:\n"
      ; ":raw-streets -> \n"
      ; ":raw-streets -> \n"
      ]

     ; [:p "scraping list of all streets in the city"]

     (let [drv-street-map (:drv-buildings-per-street dict)

           raw-geonims (:ua-geonims dict)
           geonims (group-by :id raw-geonims)

           street-aliases (:drv-renamed-streets dict)


           short-2-full (assoc (clojure.set/map-invert ua-geonim-2-short)
                          "тупик" "тупик"
                          "смт" "смт"
                          "набережна" "набережна")

           ]

       [:div
        [:h3 "DRV"]

        [:header "List of all buildings per street"]
        (<drv-buildings-list> (take 100 drv-street-map))


        ;[:header "Convert DRV-street to canonical one"]
        ;(<drv-canonalize-streets> drv-street-map geonims)
        ]



       ;; converting drv street to a cannonical representation
       #_[:div.html
          #_(reduce
              (fn [a s]

                a
                )
              {} (keys drv-street-map))

          (map #(do
                  [:p % " - " (pr-str (drv-guess-geonim %))]
                  ) (sort (keys drv-street-map)))
          ]

       #_[:div.html
          (map #(do [:p %]) (sort (keys geonims)))
          ]
       )



     ;; street to canonical name

     #_(when-let [raw-streets (:raw-streets dict)]
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
                                 {:t       (first (keys geonim-map))
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
                                 (if (empty? extracted-geonims)
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
            ; (pr-str geonim-2-short)

            (map <street-name> ru-geonims-list)
            ]

           )
         ;; [:pre (pr-str (count raw-streets)) " streets"]



         )

     ;; process raw-streets
     #_(when-let [renamed-streets (:renamed-streets dict)]
         (<street-renaming> dict renamed-streets (:raw-streets dict))


         )

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


#_(rum/defcs <cc> < rum/reactive
                    (rum/local #{} ::selected-filters)
                    (rum/local ::no-category ::filter-mode)
    [st
     *dict *categorizator]
    ;(get-in wf [:state ::dict])
    ;(get-in wf [:state ::categorizator])
    [:div
     (pg-ui/menubar "streets"
                    [
                     ["load pre-parsed streets" (fn [] (load-edn *dict "/s/streets/streets.edn" :raw-streets))]
                     ["load renamed streets" (fn [] (load-edn *dict "/s/streets/renamed-streets.edn" :renamed-streets))]
                     []
                     ["blago raw" (fn []
                                    (load-edn *dict "/s/blagovist.ua/parsed.edn" :blago-raw))]

                     ["blago->json" (fn []

                                      (woof-dom/save-json

                                        (map
                                          (fn [i]
                                            (dissoc i :region :photos)
                                            )
                                          (:blago-raw dict)
                                          )

                                        )
                                      ;;(clj->js (:blago-raw dict))
                                      )]

                     ["raw yt" (fn [] (load-edn *dict "/s/yt/wl.edn" :yt-raw))]
                     ["yt sorted" (fn [] (load-edn *dict "/s/yt/sorted.edn" :yt-sorted))]
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
                                           (woof-dom/save-edn sorted-m)
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




     ]
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
                     :renamed-streets []

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