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




(def YT-URL "https://youtube.com")

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
  							img :img-src
  							} x]
				[:div.video 
						{:style (if seen {:background (str "linear-gradient(to right, var(--progress-color) " seen ", transparent " seen " 100%)") } {})}

						[:pre (pr-str img)]
  				[:span.idx (pr-str idx)]
     	[:a.channel {:href (str YT-URL ch-href)  :target "_blank"} ch-title]
     	[:a.url {:href (str YT-URL url) 									:target "_blank"} title]

     	(if seen [:span seen])
     	(if img  [:img {:src img}])
     	
   		 ;;(pr-str (keys x))
  		]
  )
  
)

(defn- copy-as-edn-list [items]
	 (let [str-xs (map pr-str)
	 				 s (str "[\n" (str/join "\n" (into [] str-xs items)) "\n]")]

	 (woof-dom/copy-to-clipboard s))
)
	 

(defn- escape-brackets [s]
		(->
  		s
  (str/replace #"\[" "{")
  (str/replace #"\]" "}")
  )
)

;; todo: parse url - and stip playlist 
(defn- copy-as-roam [items]
	 (let [str-xs (map #(str "[" (escape-brackets (:title %) )"](" YT-URL (:url %)")"))
	 				 s (str/join "\n" (into [] str-xs items))]

	 (woof-dom/copy-to-clipboard s))

;TITLE (str 
;							 	 "[" (woof-dom/txt el) "]"
;							 	 "(" (str (.-location js/window)) ")") 
							 
)


(rum/defcs <yt-vid-list> <
  (rum/local true ::group?)
  rum/reactive

  [st *data]
  [:div

   (pg-ui/menubar ""
   														 (let [GET #(ws/GET % (fn [_data] (let [data (d/to-primitive _data)] (swap! *data assoc-in [:videos] data ))))]                                
                  [
                   ["DATA: "]
                   ;; todo: maybe get these from kv?
                   ["load test data" (partial GET "http://localhost:9500/s/yt/wl-1.edn")]
                   ["cooking" (partial GET "http://localhost:9500/s/yt/cooking.edn")]
                   [" UI: "]
                   [(pg-ui/shorten-bool " group" @(::group? st)) (fn [] (swap! (::group? st) not))]
                   ]))
   [:hr]

   (when-let [vids (:videos @*data)]

     ;; [:pre (pr-str vids)]
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
                  ;; todo: how to make a filter based on grouped data
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
                      :channel (fn [a b] 
                      				(let [c (compare (:channel-title a) (:channel-title b))]
                      						(if (= 0 c)
                      							 (compare (:idx a) (:idx b))
                      							 c
                      						)
                      					)
                      				)
                      )

           ]

           [:div 
			

							;;[:pre (d/pretty @*asserts)]

           ;; todo: document usage of transform list
       (pg-ui/<transform-list>
         <item-ui>
         
         ;; vids
         ui-vids

         (group-by :id @*asserts) 
         

         :id-fn :url

         :global-fn-map (array-map "copy EDN" copy-as-edn-list
         																										"copy ROAM" copy-as-roam)

         ;; grouping
         :group-fn-map {"copy EDN" copy-as-edn-list
         														 "copy ROAM" copy-as-roam}
         :group-fn (if @(::group? st) :channel-title)

         ;; 

         :filter-map {
													"seen" (partial pg-ui/_marker-class-filter "seen")

													"popular" (fn [item st] 
																		;; todo: how to propagate data fro *groups to st ???
																 (> (count (get @*groups (:channel-title item) [])) 3)
															)

         }
         
         
         ;; :copy-fn
         :sort-fn-map sort-fns
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
