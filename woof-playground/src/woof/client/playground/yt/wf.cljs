(ns woof.client.playground.yt.wf
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

    [woof.client.common :as common]

    [clojure.set :as set]

    [cljs-time.core :as time]
    [cljs-time.format :as time-fmt]
    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))




(rum/defcs <video> < rum/static
                       (rum/local false ::details?)
                       {:key-fn (fn [x] (str (:url x)))}

  [st x]
  [:div.video 
      (pr-str x)
  ]
)

(rum/defcs <yt-vid-list> <
  (rum/local nil ::sort-key)
  rum/reactive

  [st *data]
  [:div

   (pg-ui/menubar "DATA"
                  [
                   ["test data" (fn []
                                 (ws/GET "http://localhost:9500/s/yt/wl.edn"
                                         (fn [_data]
                                           (let [data (d/to-primitive _data)]
                                             (swap! *data assoc-in [:videos] data ))))
                                 )]

                   ]
                  )

   (when-let [listings (:videos @*data)]

      [:pre (pr-str listings)]

     (let [<item-ui> <video>

           sort-fns (array-map
                      :USD↓ (fn [a b] (compare (:USD a) (:USD b)))
                      :USD↑ (fn [a b] (compare (:USD b) (:USD a)))
                      :upd↑ (fn [a b]
                               #_(compare
                                        (.getTime (:d/upd b))
                                        (.getTime (:d/upd a)))
                              0
                              )
                      :upd↓ (fn [a b]
                              #_(compare
                                (.getTime (:d/upd a))
                                (.getTime (:d/upd b)))
                              0
                              )
                      )

           sorters (into #{} (keys sort-fns))

           sort-fn (get sort-fns @(::sort-key st)
                (get sort-fns :USD↓))

           *asserts (volatile! [])

           __cheap (fn [item]
                          (if (< (:USD item) 80000)
                            {:id (:id item) :class #{"cheap"}}))
           __recent (fn [item]
                          (if (< (time/in-days (:d/added-ago item)) 10)
                            {:id (:id item) :class #{"recent"}}))


           date-fmt (time-fmt/formatter "yyyy.MM.dd")
           to-time (partial time-fmt/parse date-fmt)

           curr-t (time/now)

           xs (comp
                ;identity
                (map-indexed #(assoc %2 :i %1))

                (data/z-map-1
                  (data/juxt-mapper
                    __cheap
                    __recent
                    ;;__drv-street
                    ;;__test-street
                    )
                  #(vswap! *asserts into %)
                  (fn [x]
                    (let [upd (to-time (get x :upd))
                          added (to-time (get x :added))]

                      ;(.log js/console (- (.getTime curr-t) (.getTime added)))

                      (-> x
                          (assoc :d/upd upd
                                 :d/added added
                                 ;:d/between-add-and-update (safe-interval added upd )
                                 ;:d/updated-ago (safe-interval upd curr-t   )
                                 ;:d/added-ago (safe-interval added curr-t  )
                                 )

                          )
                      )


                    )
                  )
                )
           ;ui-listings (into []
           ;                  xs
           ;                  listings)

           ]

       
       (pg-ui/<transform-list>
         <item-ui>
         
         listings
          ; ui-listings
         ;(group-by :id @*asserts)
         {}

         :id-fn :url

         :filter-map {}
         ;; :sort-fn sort-fn

         ;:filter-map {
         ;             "recent" (partial pg-ui/_marker-class-filter "recent")
         ;             "cheap" (partial pg-ui/_marker-class-filter "cheap")
         ;             }
         ;:api-fns (concat [[]]
         ;                 (map (fn [sk]
         ;                        [(name sk) (fn []
         ;                                     (reset! (::sort-key st) sk)
         ;                                     )]
         ;                        ) sorters))
         )

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
            (<yt-vid-list> *data)
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

     :title       "YOUTUBE data dashboard"
     :explanation [:div.explanation
                   [:p "work with data scraped from YOUTUBE"]]

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
                   common/common-ctx
                   woof-dom/dom-ctx
                   ]
     ;;
     :steps-fns   [
                   (fn [params] { ::#EVT-LOOP# [:evt-loop (evt-loop/&evt-loop params)]})
                   (partial woof-dom/_add-style-once-steps-fn "http://localhost:9500/css/yt.css")
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
