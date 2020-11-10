(ns woof.client.playground.apt.wf
  (:require
    [cljs.core.async :as async]
    [clojure.string :as str]
    [clojure.set :as set]

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



(rum/defc <listing> < rum/static {
                                 :key-fn (fn [x] (str (:id x)))}
  [x]

  (let [{
         id :id

         } x]

    [:div.listing-row.html
     {:on-click (fn[e] (.log js/console x))
      ;:class (get x :css "")
      }
     [:div
      [:div


       ;[:span.tag.small-tag.i (get x :i -1)]

       (if id [:span.tag.small-tag.id "id " id])
       ; [:span.tag.small-tag.idx idx]

       [:span.tag.small-tag (str (get x :USD "") "$")]
       ]

      ]

     [:p (get x :descr "")]

     ;;
     [:.html (pr-str (keys x))]

     ;[:.other other]

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

(rum/defcs <apt> < rum/reactive
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


           *asserts (volatile! [])

           __cheap (fn [item]
                          (if (< (:USD item) 80000)
                            {:id (:id item) :class #{"cheap"}}))


           xs (comp
                ;identity
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

         :sort-fn (fn [a b] (compare (:upd a) (:upd b)))

         :filter-map {
                      "cheap" (partial pg-ui/_marker-class-filter "cheap")
                      }
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
