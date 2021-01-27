(ns woof.client.browser.scraper.scraping-ui
  (:require
    [cljs.core.async :as async]
    [clojure.string :as str]
    [clojure.set :as set]

    [goog.object]
    [goog.dom :as dom]
    [goog.object]
    [goog.dom.classes :as classes]

    [rum.core :as rum]

    [woof.base :as base]
    [woof.data :as d]
    [woof.utils :as u]
    [woof.client.dom :as woof-dom]
    [woof.client.ws :as ws]
    ))



;;
(defn scraping-ui-impl! [meta-info]

  (if (get meta-info :ws? false)
    (let [clear-session-btn-el (dom/createDom "button" "" "clear session")
          get-session-btn-el   (dom/createDom "button" "" "get session")
          panel (dom/createDom "div" "panel")]

      ;;(js-debugger)

      (dom/appendChild panel (dom/createDom "header" "" "SCRAPING SESSION "))

      (dom/appendChild panel get-session-btn-el)
      (dom/appendChild panel clear-session-btn-el)



      (woof-dom/on-click get-session-btn-el
                         (fn [e]
                           (ws/GET "http://localhost:8081/scraping-session"
                                   (fn [raw-edn]
                                     (.log js/console raw-edn)))))

      (woof-dom/on-click clear-session-btn-el
                         (fn [e]
                           (ws/GET "http://localhost:8081/clear-scraping-session"
                                   (fn [raw-edn]
                                     (.log js/console raw-edn)))))

      (woof-dom/ui-add-el! panel)
      )
    )
  )


(defn wf-api-ui! [api]
  (let [panel (dom/createDom "div" "panel")]
    (classes/add panel "woof-api-panel")
    (dom/appendChild panel (dom/createDom "header" "" "API"))
    (doseq [[k v] api]
      (let [btn-el (dom/createDom "button" "" (str k))]
        ;; todo: expose api actions and show them in um

        (woof-dom/on-click btn-el (fn [] (v)))
        (dom/appendChild panel btn-el)
        )
      )
    (woof-dom/ui-add-el! panel)
    )
  )


(defn wf-indicator-ui! []
  (let [panel (dom/createDom "div" "panel")]
    (classes/add panel "woof-wf-indicator")
    (dom/appendChild panel (dom/createDom "span" (js-obj
                                                   "id" "wf-indicator"
                                                   "class" "wf-not-running") ))

    (woof-dom/ui-add-el! panel)
    )
  )


(defn- indicate [color cl blink?]
  ;(.warn js/console "indicate" color cl blink?)
  (when-let [indicator (.querySelector (.-body js/document) "#wf-indicator")]
    (set! (-> indicator .-style .-backgroundColor) color)
    (if blink?
      (classes/addRemove indicator "" "woof-indicator-blink")
      (classes/addRemove indicator "woof-indicator-blink" ""))
    )

  (when-let [indicator-panel (woof-dom/q ".woof-wf-indicator")]
    (let [classes  (into #{} (classes/get indicator-panel))
          cl-diff (set/difference classes #{"panel" "woof-wf-indicator"})]

      (doseq [cl2remove cl-diff]
        (classes/remove indicator-panel cl2remove))
      (classes/add indicator-panel cl)
      )
    )
  )

(defn indicate-wf-started []
  (indicate "rgb(26 232 175)" "woof-wf-started" true))

(defn indicate-wf-done []
  (indicate "rgb(101 189 132)" "woof-wf-done" false))

(defn indicate-wf-error []
  (indicate "rgb(255 0 0)" "woof-wf-error" false))

;;

;; should be in other place
(rum/defc <listing> < rum/static {
                                  :key-fn (fn [item] (str (:id item)))}
  [item]

  (let [z (into (sorted-map) item)]

    [:div.listing-row
     {:on-click (fn [e] (.log js/console item))
      :class    (get item :css "")}

     [:div

      #_(let [usd (usd-fmt (get x :USD 0))
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

      #_[:.langs
         (if t [:.t t])
         [:.ua ua]
         [:.ru ru]
         [:.en en]
         ]
      ]
     [:pre
      (d/pretty! z)



      #_[:div
         [:span.tag.small-tag.idx (get item :i -1)]
         [:span.tag.small-tag.idx idx]
         [:span.tag.small-tag.district district]
         #_[:span.districts
            (map (fn [d] [:span.small-tag.district {:key (pr-str d)} d]) districts)]
         [:span.aliaes
          (map (fn [d] [:span.tag.small-tag.alias {:key (pr-str d)} d]) alias)]

         ]

      #_[:.langs
         (if t [:.t t])
         [:.ua ua]
         [:.ru ru]
         [:.en en]
         ]
      ]

     #_[:.other other]

     #_(if (:test item)
         [:div {:style {:outline "1px solid red"}}
          (:test item)
          ]
         )
     ]
    #_[:pre.street-row
       (d/pretty! item)
       ]
    )

  )
