(ns woof.client.playground.streets.ui
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

    [woof.client.playground.streets.ds :as ds]
    [woof.client.ws :as ws]

    [woof.wfs.evt-loop :as evt-loop]
    [clojure.set :as set])

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



(rum/defc <street> < rum/static {
                                 :key-fn (fn [street] (str (:ID street))
                                                      ;(str (:ua street) (:idx street))
                                              )}
  [street]

  (let [{t :t
         ua :ua
         ru :ru
         en :en
         idx :idx
         id :ID
         district :district
         ; districts :districts
         other :other
         alias :alias
         :or {alias []}
         } street]

    [:div.street-row
     {:on-click (fn[e] (.log js/console street))
      :class (get street :css "")
      }
     [:div
      [:div
       [:span.tag.small-tag.i (get street :i -1)]
       (if id [:span.tag.small-tag.id "ID " id])
       ; [:span.tag.small-tag.idx idx]

       [:span.tag.small-tag.district district]
       #_[:span.districts
          (map (fn [d] [:span.small-tag.district {:key (pr-str d)} d]) districts)]
       [:span.aliaes
        (map (fn [d] [:span.tag.small-tag.alias {:key (pr-str d)} d]) alias)]
       ]

      [:.langs
       (if t [:.t t])
       [:.ua ua]
       [:.ru ru]
       [:.en en]]
      ]

     [:.other other]

     (if (:test street)
       [:div {:style {:outline "1px solid red"}}
        (:test street)
        ]
       )

     (if-let [houses (:houses street)]
       [:span.houses
        (map (fn [d] [:span.tag.small-tag.house {:key (pr-str d)} d]) (sort houses))]
       )

     ]
    #_[:pre.street-row
       (d/pretty! street)
       ]
    )

  )