(ns woof.client.browser.kga.scraper
  (:require

    [woof.base :as base]
    [woof.client.dom :as woof-dom :refer [q q* html! btn!]]
    [woof.client.dbg :as dbg :refer [__log]]
    [woof.data :as d]
    [woof.utils :as u]

    [goog.dom :as dom]
    [goog.dom.classes :as classes]

    [goog.dom.dataset :as dataset]
    ;; common wf

    [woof.wfs.evt-loop :as evt-loop]
    [woof.wfs.watcher :as watcher]

    [woof.client.ws :as ws]
    [woof.utils :as u]
    [woof.client.browser.scraper.session :as ss]
    [woof.client.browser.scraper.scraping-ui :as sui]

    [cljs.core.async :refer [go] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]
    [clojure.string :as str]))


(defn clean-css-init [params]

  ;; remove previosly added styles
  (woof-dom/remove-added-css)

  ;; remove classes for previously added elements
  (doseq [s* ["woof-el"
              "woof-start"
              "woof-tmp"
              "woof-err"
              "woof-processed"
              ]
          ]
    (doseq [el (q* (str "." s*))]
      (classes/remove el s*)
      )
    )

  {
   ;; ::state *state
   })




(defn wf-ctx [params]
  (let [cf (base/&chan-factory params)
        fetch-opts (js-obj
                     "headers" (js-obj
                                 "accept" "*/*"
                                 "accept-language" "en-US,en;q=0.9,ru;q=0.8"
                                 "sec-fetch-dest" "empty"
                                 "sec-fetch-mode" "cors"
                                 "sec-fetch-site" "same-origin"
                                 )
                     "referrer" "https://mkk.kga.gov.ua/map/"
                     "referrerPolicy" "strict-origin-when-cross-origin"
                     "body" nil,
                     "method" "GET"
                     "mode" "cors"
                     "credentials" "include"
                     )]

    {

     :fetch-houses    {:fn (fn [id]
                              (let [chan (base/make-chan cf (base/rand-sid))


                                    ;url (str "https://mkk.kga.gov.ua/arcgis/rest/services/Search/AddressSearch/MapServer/0/query?f=json&returnGeometry=true&spatialRel=esriSpatialRelIntersects&maxAllowableOffset=0.29858214164761665&objectIds="
                                    ;         id "&outFields=*&outSR=102100")

                                    url (str "https://mkk.kga.gov.ua/arcgis/rest/services/Search/AddressSearch/MapServer/0/query"
                                             "?f=json&where=StrCodeObj" "%3D" id "&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100")
                                    ]

                                (go
                                  (let [req (<p! (js/fetch url fetch-opts))
                                        json (<p! (.json req))

                                        details (js->clj json :keywordize-keys true)]

                                    ;; in order to process full response
                                    ; (async/put! chan details)

                                    ;; extract house nums
                                    (let [features (get details :features [])
                                          houses (into #{} (map #(get-in % [:attributes :AddrNumb1]) features))]

                                      (async/put! chan {id houses})
                                      )

                                    ;;
                                    (async/put! chan details)))
                                chan
                                )
                              )
                        }

     :process*         (base/expand-into :fetch-houses)

     :merge            {
                        :fn       (fn [vs]
                                    (apply merge vs)
                                    )
                        :collect? true
                        }

     :save-edn {
                :fn (fn [edn]
                      (woof-dom/save-edn (str "kga-houses-" (u/now) ".edn") edn)
                      :ok)

                }

     :custom-log {:fn (fn [v]
                        (.groupCollapsed js/console "DETAILS")
                        (.log js/console (d/pretty! v))
                        (.groupEnd js/console)
                        )}

     }
    )
  )



(defonce STREET-IDS
         [10020
          10021
          10033
          10086
          10087
          10093
          10099
          10100
          10112
          10118
          10123
          10124
          10128
          10131
          10132
          10147
          10150
          10157
          10177
          10189
          10200
          10209
          10210
          10213
          10215
          10225
          10258
          10259
          10261
          10263
          10279
          10302
          10334
          10348
          10349
          10350
          10353
          10364
          10383
          10405
          10414
          10429
          10439
          10475
          10476
          10498
          10499
          10518
          10527
          10542
          10559
          10560
          10562
          10572
          10582
          10586
          10597
          10598
          10602
          10603
          10607
          10610
          10625
          10646
          10650
          10668
          10675
          10704
          10705
          10712
          10745
          10747
          10750
          10761
          10769
          10795
          10797
          10798
          10893
          10910
          10937
          10938
          10944
          10945
          10973
          11032
          11033
          11034
          11051
          11068
          11071
          11079
          11087
          11088
          11099
          11118
          11120
          11121
          11133
          11144
          11147
          11164
          11175
          11187
          11188
          11199
          11210
          11212
          11234
          11245
          11247
          11251
          11262
          11274
          11275
          11289
          11299
          11300
          11314
          11318
          11324
          11325
          11342
          11343
          11351
          11352
          11353
          11369
          11399
          11400
          11440
          11441
          11453
          11454
          11455
          11463
          11476
          11480
          11490
          11491
          11492
          11510
          11512
          11542
          11552
          11589
          11590
          11605
          11606
          11640
          11641
          11642
          11645
          11646
          11674
          11681
          11716
          11724
          11728
          11777
          11778
          11781
          11798
          11799
          11818
          11823
          11852
          11869
          11870
          11914
          11919
          11934
          11936
          11937
          11959
          11960
          11970
          11971
          11977
          11997
          12000
          12001
          12006
          12010
          12011
          12019
          12028
          12033
          12043
          12046
          12050
          12054
          12462
          12463
          12464
          12465
          12466
          12467
          12468
          12469
          12470
          12471
          12472
          12473
          12474
          12475
          12476
          12477
          12478
          12479
          12480
          12481
          12482
          12483
          12484
          12485
          12486
          12487
          12488
          12489
          12490
          12491
          12610
          12702
          12709
          12913
          12915
          12918
          12919
          12920
          12922
          13098
          13099]
         )

(defn wf! [*wf-state meta-info]

  {
   :init    [clean-css-init]

   :ctx     [; watcher/watcher-ctx

             wf-ctx
             ]

   :steps   [
             (fn [params]
               {

                :street/ids [:v STREET-IDS]

                :xtract/street-house-kv [:process* :street/ids]
                :xtract/street-house-map [:merge :xtract/street-house-kv]

                :log/streets [:&log :xtract/street-house-map]

                :result/save [:save-edn :xtract/street-house-map]
                :result/log [:custom-log :xtract/street-house-map]

                }

               )


             ;;
             (fn [params]
               {
                ; ::css-1 [:css-rule ".woof-scrape-panel { height: 150px; width: 100%; }"]

                :css/custom-css [:css-file "http://localhost:9500/css/streets.css"]
                }
               )
             ]

   :opts    [(fn [params] {
                           :execute (partial base/_timed-execute-fn 100)
                           ;:execute (partial base/_chunked-execute-fn 10)
                           })]

   :api     (array-map
              "YO" (fn []
                     (.log js/console "YO"))
              )

              :on-stop (fn [state]
              (__log "KGA STREETS EXTRACTOR: ON STOP")

              ;; todo: does copying prev state makes sense here
              #_(when-let [*old-state (get-in state [:WF/params ::state])]
                  (.log js/console "copying prev state")
                  (reset! *state @*old-state)
                  )
              ;; can return channel
              )
   }

  )