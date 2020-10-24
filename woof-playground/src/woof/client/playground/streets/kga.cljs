(ns woof.client.playground.streets.kga
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
    [woof.data.core :as data]
    [woof.utils :as utils]

    [woof.browser :as woof-browser]
    [woof.client.dom :as wdom]

    [woof.client.dom :as woof-dom]
    [woof.client.playground.ui :as pg-ui]
    [woof.client.playground.ui.wf :as wf-ui]
    [woof.client.playground.streets.ui :as s-ui]

    [woof.client.playground.streets.ds :as ds]
    [woof.client.ws :as ws]

    [woof.wfs.evt-loop :as evt-loop]


    [clojure.set :as set])

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;;
;; UI

(defn- load-edn [*dict url k]
  (ws/GET url
          (fn [response]
            (let [edn (cljs.reader/read-string response)]
              (swap! *dict assoc k edn)))))


(defn- load-json [*dict url k]
  (ws/GET url
          (fn [response]
            (let [edn (js->clj (.parse js/JSON response) :keywordize-keys true)]
              (swap! *dict assoc k edn)))))






(rum/defc <kga-streets> < rum/static
  [streets]

  (let [features (:features streets)
        ;features (take 10 _features)

        streets (into []
                      (comp
                        (map-indexed (fn [i a]
                                       (let [attr (:attributes a)
                                             trim (fnil str/trim "")
                                             &v (fn [k]
                                                  (trim (get attr k ""))
                                                  ;(get attr k "")
                                                  )
                                             ]
                                         (array-map
                                           :_orig attr

                                           :i i

                                           :oid (:OBJECTID attr)
                                           :code (:StrCodeObj attr)

                                           :ID (:StrCodeObj attr)

                                           :district (&v :Districts)

                                           :cua (&v :LblStreetName)
                                           :ua (&v :UkrNameF)
                                           :ru (&v :RusNameF)
                                           :en (&v :LatNameF)

                                           :other (&v :OpysRozt)
                                           :alias (vec (filter #(not (or (nil? %) (= "" %)))
                                                               (into #{}
                                                                     (map trim (vals (select-keys attr [:UkrNameS :RusNameS :LatNameS]))))))
                                           )
                                         )

                                       ))

                        )
                  features
                  )
        ]
    [:kga-streets.html

     ;(pr-str (keys streets))
     ;"\n"
     ;(pr-str (count (:features streets)))

     ;(pg-ui/<edn-list> features "STREETS:")


     (pg-ui/<transform-list>
       s-ui/<street> streets {}
       :id-fn :ID
       :copy-fn #(dissoc % :i :_orig)
       )

     ]
    )

  )

(rum/defcs <streets-cc> < rum/reactive
  [st *dict]

  (let [dict @*dict]
    [:div.streets-pg

     [:.panel
      (pg-ui/menubar "Kadastr Data   "
                     [
                      ["load :darnytskyi"    (fn [] (load-json *dict "/s/kga/district/darn-vul-only.json" :d1))]
                      ;["load :ua-geonims"     (fn [] (load-json *dict "/s/streets/ua_geonims.edn" :ua-geonims))]
                      ])
      ]

     [:hr]
     ;(pr-str @*dict)

     (when-let [kga-streets (:d1 dict)]
       (<kga-streets> kga-streets)
       )
     ]
    )
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
           (<streets-cc> *data)
           (catch js/Error e [:pre (pr-str e)]))))
     ]))


;;;;;;;;;;;;;;;;;;;;

;;
;; WF definition
(defn wf! [*SWF]

  (let [CHAN-FACTORY (base/chan-factory (atom {}))
        *dict (rum/cursor-in *SWF [:state ::data])]
    {

     :title       "kadastr"
     :explanation [:div.explanation
                   [:p "process data scraped from " [:a {:href "https://mkk.kga.gov.ua/map/" :target "_blank"} "kadastr"]]
                   ]

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
                   wdom/dom-ctx
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
