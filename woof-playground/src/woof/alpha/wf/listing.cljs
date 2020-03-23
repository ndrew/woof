(ns ^:figwheel-hooks woof.alpha.wf.listing
  (:require
    [cljs.core.async :as async]
    [cljs.reader]

    [goog.net.XhrIo :as xhr]

    [rum.core :as rum]

    [woof.alpha.ui.wf :as wf-ui]
    [woof.base :as base]
    [woof.utils :as u]
    [woof.client.stateful :as st-wf]
    [woof.wf :as wf]
    )
  (:import [goog.net.XhrIo ResponseType])

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


(declare stateless-init!)
(declare stateful-init!)

(defn initialize! [*WF-STATE-MAP]
  (merge
    (stateless-init!)
    (stateful-init! *WF-STATE-MAP)))


(defn GET [url handler]
  (xhr/send url (fn [event]
                  (let [response (.-target event)]
                       (handler (.getResponseText response)))
                  )))


(defn listing-ctx-fn [params]
  {
   :GET  {:fn (fn [url]
                (let [chan (base/make-chan (base/&chan-factory params) (wf/rand-sid "GET-"))]
                     (GET url (fn [response]
                                (let [edn (cljs.reader/read-string response)]
                                     (.log js/console edn)
                                     (async/put! chan edn)
                                     )
                                ))
                     chan
                     )

                )

          }

   :test {:fn (fn [v] v)}
   :log  {:fn (fn [v]
                (.log js/console v)
                v)}

   ;; http://umap.openstreetmap.fr/en/map/new/#15/50.4665/30.5202
   ;; http://umap.openstreetmap.fr/en/map/anonymous-edit/432608:9zMaPPfNYh5Y6r-R0KeF_-2APlg
   :umap {:fn (fn [listings]
                (reduce
                  (fn [s o]
                    (let [lat (get-in o [:addr :lat])
                          lng (get-in o [:addr :lng])]

                         (if (and (not= "" lat)
                                  (not= "" lng))
                           (str s "\n"
                                lat ","
                                lng ","
                                (str (get-in o [:price :usd]) " USD")
                                )
                           s
                           )

                         )

                    )
                  "lat,lon,description"
                  (vals listings)
                  )

                )}

   }
  )

(defn stateless-init! []
  {
   :init-fns  [

               (fn [params]
                 {
                  ::initial-path "some/dir/"
                  ::listings-url "http://localhost:9500/data/listings.edn"
                  }
                 )
               ;; event loop
               ;init-evt-loop

               ;; channel factory
               st-wf/chan-factory-init-fn
               ]

   :ctx-fns   [listing-ctx-fn]

   :steps-fns [(fn [params]
                 {

                  ::listings     [:GET (::listings-url params)]

                  ::geo-data     [:umap ::listings]

                  ::log-geo-data [:log ::geo-data]
                  ;::initial-path [:v (::initial-path params)]
                  ;::initial-tree [:directory ::initial-path]

                  ;::YO [:test "YO"]
                  })

               ]

   :opt-fns   [;; ls/ls-opts-fn
               st-wf/chan-factory-opts-fn
               ]

   }
  )


(rum/defc <listing-row> < rum/static
                          {:key-fn (fn [listing] (:id listing))}
  [listing]
  [:tr
   [:td (:id listing)]
   [:td (:date listing)]
   [:td (pr-str (:price listing))]
   [:td (pr-str (:addr listing))]
   [:td (pr-str (keys listing))]
   ]
  )

(rum/defc <custom-wf-ui> < rum/static [wf]

  (let [result (get wf :result {})
        kv-listings (get result ::listings {})
        ]
    [:div

     [:button
      {:on-click (fn [e]
                   (let [token "pk.eyJ1Ijoic2VybnlhayIsImEiOiJjazd4Ymd1cXEwYTNmM21vMnluNnNtbTNuIn0.C81D88pOEh_A4S3YMQPhag"
                         map (.setView (.map js/L "map") #js [50.4501, 30.5234] 13)]

                        (.addTo
                          (.tileLayer js/L
                                      "https://api.mapbox.com/styles/v1/{id}/tiles/{z}/{x}/{y}?access_token={accessToken}"
                                      (clj->js {
                                                :attribution "Map data &copy; <a href=\"https://www.openstreetmap.org/\">OpenStreetMap</a> contributors, <a href=\"https://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA</a>, Imagery © <a href=\"https://www.mapbox.com/\">Mapbox</a>"
                                                :maxZoom 18
                                                :id "mapbox/streets-v11"
                                                :tileSize 512,
                                                :zoomOffset -1,
                                                :accessToken token
                                                }))
                                map)

                        (doseq [o (vals kv-listings)]
                          (let [lat (get-in o [:addr :lat])
                                lng (get-in o [:addr :lng])]

                            (if (and (not= "" lat)
                                     (not= "" lng))

                              (let [marker (.marker js/L (clj->js [lat, lng]))]
                                (.addTo marker map)
                                )
                              #_(str s "\n"
                                   lat ","
                                   lng ","
                                   (str (get-in o [:price :usd]) " USD")
                                   )
                              ;s
                              )

                            )
                          )

                        #_(let [(reduce
                                (fn [s o]
                                  (let [lat (get-in o [:addr :lat])
                                        lng (get-in o [:addr :lng])]

                                       (if (and (not= "" lat)
                                                (not= "" lng))
                                         (str s "\n"
                                              lat ","
                                              lng ","
                                              (str (get-in o [:price :usd]) " USD")
                                              )
                                         s
                                         )

                                       )

                                  )

                                )])


                        (.log js/console map)

                        )

                   )}
      "INIT"]

     [:div#map {:style {:height "360px"}}
      #_(let [map (.setView (.map js/L "map") #js [51.505 -0.09] 13)]

        ;; NEED TO UPDATE with your mapID
        (.addTo (.tileLayer js/L "http://{s}.tiles.mapbox.com/v3/thedon73v.k3goc602/{z}/{x}/{y}.png"
                            (clj->js {:attribution "Map data &copy; [...]"
                                      :maxZoom 18}))
                map))
      ]


     (if-not (u/channel? kv-listings)

       (into [:table
              [:tr
               [:th "id"]
               [:th "date"]
               [:th "price"]
               [:th "address"]
               [:th "body"]
               ]
              ]
             (map-indexed (fn [i lstng]
                            ;; pass the atom or just data will be enough
                            (<listing-row> lstng))

                          (sort-by
                            #(get-in % [:price :usd])
                            (vals kv-listings))
                          )
             )
       ;[:pre (d/pretty (keys listings))]
       )



     ]

    )

  )


(defn stateful-init! [*wf]
  {
   ;; for we provide a ui fn
   :ui-fn       (partial wf-ui/<default-wf-ui> <custom-wf-ui>)


   :title       "Present folder contents as web page"

   :explanation "Protoype: for now, use fake/hardcoded data"

   :wf-actions  {
                 :not-started [
                               ["explain what happening (see console)"
                                (fn []
                                  (prn "example, to see whether woof workflow could handle real time scenarios"))
                                ]
                               ]
                 :running [

                           ["send event" (fn []
                                           #_(let [loop-chan (st-wf/&wf-init-param *wf ::evt-loop-chan)]
                                                (async/put! loop-chan
                                                            {(wf/rand-sid "ui-") [:test (u/now)]})
                                                )
                                           )]

                           ]
                 ; :done        []
                 }


   }
  )
