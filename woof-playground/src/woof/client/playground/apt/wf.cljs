(ns woof.client.playground.apt.wf
  (:require
    [cljs.core.async :as async]
    [clojure.string :as str]
    [clojure.set :as set]


    [goog.string :as gstring]
    [cljs.pprint :as pprint]

    [rum.core :as rum]

    [woof.base :as base]
    [woof.data :as d]
    [woof.data.core :as data]
    [woof.utils :as utils]

    [woof.browser :as woof-browser]

    [woof.client.dom :as woof-dom]
    [woof.client.playground.ui :as pg-ui]
    [woof.client.playground.ui.wf :as wf-ui]
    [woof.client.ws :as ws]

    [woof.wfs.evt-loop :as evt-loop]

    [clojure.set :as set]
    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



(rum/defc <tr> < rum/static {
                                  :key-fn (fn [_ x] (str (:id x)))}
  [cfg x]

  (let [ks (:keys cfg)]
    [:.row
     (map (fn [k]
            [:.col (pr-str (get x k))]
            ) ks)
     ]
    )

  )



(def js_USD-fmt (clj->js {:style   "currency"
                         :currency "USD"
                         }))

(def js_UAH-fmt (clj->js {:style   "currency"
                          :currency "UAH"
                          }))

(defn usd-fmt [usd] (.toLocaleString usd "uk-UA" js_USD-fmt))
(defn uah-fmt [uah] (.toLocaleString uah "uk-UA" js_UAH-fmt))


(defn lpad [count s]
  (pprint/cl-format nil (str "~" count ",'" " " "d") s))


(defn listing-url [source id]
  (cond
    (= source :riel) (str "https://rieltor.ua" id)
    :else id
    )
  )

(rum/defcs <listing> < rum/static
                       (rum/local false ::details?)
                       {:key-fn (fn [x] (str (:id x)))}
  [st x]
  (let [{
         id :id
         } x]

    [:div.listing-row.html
     {:on-click (fn[e] (.log js/console x))
      ;:class (get x :css "")
      }


     [:div.flex
      ; header

      (let [usd (usd-fmt (get x :USD 0))
            uah (uah-fmt (get x :UAH 0))
            usd-m2 (usd-fmt (get x :USD_M2 0))
            uah-m2 (uah-fmt (get x :UAH_M2 0))
            padded-usd-m2 (lpad (count usd) usd-m2)
            padded-uah-m2 (lpad (count uah) uah-m2)]
        [:div.prices-block.html
         [:span.tag.small-tag usd " / " uah] "\n"
         [:span.tag.small-tag padded-usd-m2 " / " padded-uah-m2 " м²"]
         ]
        )


      [:span.tag.small-tag.html
       [:a {:href (listing-url (:source x) id)} id]

       "\n"
       (:upd x) " (" (:added x) ")"
       ]

      ]

     [:div.flex
      [:div.addr-block.html
       "\n" (:addr_street x) " " (:addr_house x) "\t" (:addr_district x)
       "\n\n"
       "Поверх: \t" (str (:floor x)) " / " (str (:floor_total x))
       (str "\nПлоща (разом/житл/кухня):\t" (:area_total x) " / " (:area_living x) " / " (:area_kitchen x))

       (if-let [walls (:house_walls x)]
         (str "\nСтіни:\t" walls))

       [:p (get x :descr "")]
       ]
      [:.imgs-block
       (pr-str (:img-n x))
       (map (fn [url]
              [:img {:src url}]
              ) (get x :imgs []))
       ]
      ]


     ;[:span.tag.small-tag.i (get x :i -1)]
     ; [:span.tag.small-tag.idx idx]




     (let [*details? (::details? st)]

       [:.other
        (pg-ui/menubar "" [
                           [(pg-ui/shorten-bool @*details?) (fn [] (swap! *details? not))]
                           ])

        (if @*details?
          [:.html
           (d/pretty! (into (sorted-map) x))]
          )
        ]
       )



     #_(if (:test x)
       [:.html {:style {:outline "1px solid red"}}
        (:test x)
        ]
       )

     #_(if-let [houses (:houses x)]
       (let [hs (keys houses)
             hs (if hs hs [])]
         [:span.houses
          (map (fn [d] [:span.tag.small-tag.house {:key (pr-str d)} d]) hs)]
         )
       )

     ]
    #_[:pre.street-row
       (d/pretty! x)
       ]
    )

  )

(rum/defcs <apt> <
  (rum/local nil ::sort-key)
  rum/reactive

  [st *data]
  [:div
   (pg-ui/menubar "KV"
                  [
                   ["listings" (fn []
                                 (ws/GET (str "http://localhost:8081/kv/get/" ":listings")
                                         (fn [_data]
                                           (let [data (d/to-primitive _data)]
                                             (swap! *data assoc-in [:listings] data ))))
                                 )]

                   ["post" (fn []
                             (let [k (d/to-primitive (js/prompt "k=" "k"))
                                   v (d/to-primitive (js/prompt "v=" "v"))]
                               (ws/POST "http://localhost:8081/kv/put" (fn [])
                                        {:k k
                                         :v v})))]]
                  )

   (when-let [listings (:listings @*data)]

     (let [<item-ui> <listing>

           ;;<item-ui>
           #_(partial <tr> {:keys [:id
                                         :USD
                                         :USD_M2
                                         :UAH
                                         :UAH_M2
                                         :descr
                                         :upd
                                         :added
                                         :floor
                                         :floor_total
                                         :house_walls
                                         :rooms
                                         :area_total
                                         ]})

           sort-fns (array-map
                      :upd↓ (fn [a b] (compare (:upd a) (:upd b)))
                      :upd↑ (fn [a b] (compare (:upd b) (:upd a)))
                      :USD↓ (fn [a b] (compare (:USD a) (:USD b)))
                      :USD↑ (fn [a b] (compare (:USD b) (:USD a)))
                      )


           sorters (into #{} (keys sort-fns))

           sort-fn (get sort-fns @(::sort-key st)
                (get sort-fns :USD↓))

           *asserts (volatile! [])

           __cheap (fn [item]
                          (if (< (:USD item) 80000)
                            {:id (:id item) :class #{"cheap"}}))


           xs (comp
                ;identity
                (map-indexed #(assoc %2 :i %1))

                (data/z-map-1
                  (data/juxt-mapper
                    __cheap
                    ;;__drv-street
                    ;;__test-street
                    )
                  #(vswap! *asserts into %)
                  identity
                  )
                )
           ui-listings (into []
                             xs
                             listings)]

       (pg-ui/<transform-list>
         <item-ui>
         ui-listings
         (group-by :id @*asserts)
         :id-fn :id

         :sort-fn sort-fn

         :filter-map {
                      "cheap" (partial pg-ui/_marker-class-filter "cheap")
                      }
         :api-fns (concat [[]]
                          (map (fn [sk]
                                 [(name sk) (fn []
                                              (reset! (::sort-key st) sk)
                                              )]
                                 ) (sort sorters)))
         )

       )

     ;;[:.html (pr-str listings)]


          #_(pg-ui/<transform-list>
         (fn [item] [:.html (pr-str item)])
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


     )
   ]
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
                    (<apt> *data)
                    (catch js/Error e [:pre (pr-str e)]))


                  ))
              ]))


;;;;;;;;;;;;;;;;;;;;

;;
;; WF definition
(defn wf! [*SWF]
  (let [CHAN-FACTORY (base/chan-factory (atom {}))
        *dict (rum/cursor-in *SWF [:state ::data])]
    {

     :title       "apartment dashboard"
     :explanation [:div.explanation
                   [:p "apartment dashbord"]]

     ;; this state will be added to a wf?
     :state {
             ::data {
                     ;; just pile of data
                     }
             }

     :init-fns    [
                   { ::*data *dict }

                   (base/build-init-chan-factory-fn CHAN-FACTORY)
                   (evt-loop/build-evt-loop-init-fn (base/make-chan CHAN-FACTORY (base/rand-sid "evt-")))
                   ]
     ;;
     :ctx-fns     [
                   evt-loop/evt-loop-ctx-fn
                   ;; re-use common browser step handlers
                   woof-browser/common-ctx
                   woof-dom/dom-ctx
                   ]
     ;;
     :steps-fns   [
                   (fn [params] { ::#EVT-LOOP# [:evt-loop (evt-loop/&evt-loop params)]})
                   (partial woof-browser/_add-style-once-steps-fn "http://localhost:9500/css/apt.css")
                   ]

     :opt-fns     [
                   (base/build-opts-chan-factory-fn CHAN-FACTORY)
                   ;; uncomment to display wf results (esp. if there was an error)
                   (base/build-opt-on-done (fn [params result]
                                             (.warn js/console params result)))
                   ]

     :ui-fn       (partial wf-ui/<wf-UI> (partial <WF>))

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
