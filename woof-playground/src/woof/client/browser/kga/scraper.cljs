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
    [clojure.string :as str]

    [woof.wfs.alpha :as alpha]
    ))


(defonce SEQ-ID ::fetcher)


(defonce STREET-IDS
         [10013 10023 10034 10035 10042 10043 10044 10049 10050 10055 10068 10069 10072 10073 10074 10084 10085 10089 10090 10104 10109 10144 10149 10160 10161 10186 10190 10201 10228 10277 10283 10286 10312 10325 10326 10330 10331 10332 10339 10351 10352 10354 10382 10388 10392 10413 10418 10427 10428 10440 10461 10462 10473 10474 10510 10511 10517 10521 10525 10593 10594 10596 10599 10619 10623 10635 10670 10678 10679 10684 10702 10748 10765 10767 10773 10777 10778 10786 10799 10812 10830 10831 10836 10864 10885 10891 10903 10956 10957 10958 10976 10977 10981 10988 10991 11001 11037 11043 11060 11061 11062 11081 11110 11112 11124 11125 11134 11135 11136 11138 11140 11141 11162 11174 11180 11182 11183 11195 11196 11202 11204 11208 11209 11233 11252 11259 11264 11267 11268 11271 11277 11280 11281 11282 11293 11297 11313 11315 11326 11328 11334 11335 11349 11381 11390 11398 11426 11433 11438 11442 11446 11467 11471 11495 11496 11497 11505 11543 11561 11566 11577 11578 11586 11587 11596 11597 11604 11608 11609 11610 11611 11626 11628 11631 11651 11654 11655 11657 11661 11672 11676 11679 11690 11691 11695 11704 11705 11708 11712 11723 11732 11743 11744 11774 11795 11807 11824 11825 11835 11837 11838 11858 11863 11868 11878 11885 11886 11887 11889 11894 11896 11901 11907 11911 11912 11915 11921 11922 11927 11928 11943 11957 11958 11966 11986 11996 12023 12030 12059 12110 12609 12731 13119]
         )



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
                     )

        fetch-fn (fn [id]
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
        ]

    {

     :merge        {
                    :fn       (fn [vs]
                                (apply merge vs)
                                )
                    :collect? true
                    }

     :save-edn     {
                    :fn (fn [edn]
                          (woof-dom/save-edn (str "kga-houses-" (u/now) ".edn") edn)
                          :ok)

                    }

     :custom-log   {:fn (fn [v]
                          (.groupCollapsed js/console "DETAILS")
                          (.log js/console (d/pretty! v))
                          (.groupEnd js/console)
                          )}

     ;; normal expand
     :fetch-houses {:fn fetch-fn}
     :process*     (base/expand-into :fetch-houses)


     ;; linearized fetch
     :seq-process* {:fn       (partial alpha/_seq-worker-expander
                                       SEQ-ID
                                       (fn [id]
                                         (.log js/console ":process*\tid=" id)
                                         (fetch-fn id)
                                         )
                                       params
                                       )
                    :expands? true
                    }

     }
    )
  )




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


(defn wf! [*wf-state meta-info]

  {
   :init    [clean-css-init
             ;; provide linearization for doing expands
             (fn [params]
               (alpha/_seq-worker-init SEQ-ID params))
             ]

   :ctx     [; watcher/watcher-ctx

             wf-ctx



             ]

   :steps   [
             (fn [params]
               {

                :street/ids [:v STREET-IDS]

                ;; all expand
                ; :xtract/street-house-kv [:process* :street/ids]
                ;; linearized process
                :xtract/street-house-kv [:seq-process* :street/ids]
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