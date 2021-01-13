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
  (.warn js/console "indicate" color cl blink?)
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

