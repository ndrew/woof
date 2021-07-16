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
  (let [{idx :idx
  							url :url
  							title :title
  							ch-href :channel-href 
  							ch-title :channel-title
  							seen :%
  							img :img-scr
  							} x]
				[:div.video 
  				[:span.idx (pr-str idx)]
     	[:a.channel {:href ch-href} ch-title]
     	[:a.url {:href url} title]

     	(if seen [:span seen])
     	(if img  [:img {:src img}])
     	
   		 ;;(pr-str (keys x))
  		]
  )
  
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

   (when-let [vids (:videos @*data)]

      [:pre (pr-str vids)]

     (let [<item-ui> <video>


           *asserts (volatile! [])

           __seen (fn [item]
           															(if-not (nil? (:% item))
                            (do 
                            	 ;(.log js/console "seen" item)
                            		{:id (:url item) :class #{"seen"}
                            		})))

           
           ;; filter channels that have > 5 vidds
           __channel (fn [item]
                          {:id (:id item) :channel #{ (:channel-title item)}})



           *groups (volatile! {})
           ;; transducer that builds 
           xs (comp
                ;identity
                (map-indexed #(assoc %2 :i %1))

                (data/z-map-1
                  (data/juxt-mapper __seen )
                  #(vswap! *asserts into %) 
                  (fn [x]
 																			(vswap! *groups update (:channel-title x) (fnil conj #{}) (:url x))
                  	 x)
                  ))

           ui-vids (into []
                         xs
                         vids ;(take 50 vids)
                         )

           popular-channels (reduce (fn [a [k v]] 

           																					;{:id (:url item) :class #{"LLLLLL"}}
           																					) [] @*groups)

           ;; sort

           sort-fns (array-map
                      :idx↓ (fn [a b] (compare (:idx a) (:idx b)))
                      :idx↑ (fn [a b] (compare (:idx b) (:idx a)))
                      :channel (fn [a b] (compare (:channel-title b) (:channel-title a)))
                      )

           sorters (into #{} (keys sort-fns))

           sort-fn (get sort-fns @(::sort-key st) (get sort-fns :idx↓))


           ]

           [:div 
			

           ;; todo: document usage of transform list
       (pg-ui/<transform-list>
         <item-ui>
         
         ;; vids
         ui-vids

         (group-by :id @*asserts) 
         

         :id-fn :url

         :filter-map {
													"seen" (partial pg-ui/_marker-class-filter "seen")

													"popular" (fn [item st] 
																		;; todo: how to propagate data fro *groups to st ???
																 (> (count (get @*groups (:channel-title item) [])) 3)
															)

         }
         
         
         :sort-fn sort-fn

         :api-fns (concat [[]]
                          (map (fn [sk]
                                 [(name sk) (fn []
                                              (reset! (::sort-key st) sk)
                                              )]
                                 ) sorters))
         )
           ]
      

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
