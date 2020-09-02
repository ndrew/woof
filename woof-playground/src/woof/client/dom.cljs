(ns woof.client.dom
  (:require
    [goog.object]
    [goog.dom :as dom]
    [goog.object]
    [goog.dom.classes :as classes]

    [cljs.core.async :as async]
    [clojure.string :as str]

    [woof.base :as base]
    [woof.data :as d]
    [woof.utils :as u]
    ))


(defn add-script [src on-load-fn]
  "dynamically adds script to the body"

  (let [el (dom/createElement "script")]
    (set! (.-type el) "text/javascript")
    (set! (.-src el) src)
    (set! (.-onload el) on-load-fn)

    (dom/appendChild (.-body js/document) el)))


(defn add-el! [el-present-selector el]
  "dynamically adds or replaces element to dom"

  (let [
        els (array-seq (.querySelectorAll (.-body js/document)
                                          el-present-selector
                                          ))]

    (if-not (empty? els)
      (dom/replaceNode el (first els))          ;; replace el from dom
      (dom/appendChild (.-body js/document) el) ;; append element
      )
    )
  )


(defn add-stylesheet [src]

  (let [el (dom/createElement "link")]
    (set! (.-href el) src)
    (set! (.-rel el) "stylesheet")
    (set! (.-type el) "text/css")

    (set! (.-onload el) (fn []
                          ; todo: check
                          ))

    (dom/appendChild (.-head js/document) el)

    )
  )


(defn css-add-rule! [rule]
  (let [style-el (.createElement js/document "style")]
    (.appendChild (.-head js/document) style-el)

    (let [sheet (.-sheet style-el)]
      (.insertRule sheet rule)
      )
    )
  )


(defn query-selector*
  ([selector]
   (query-selector* (.-body js/document) selector))
  ([el selector]
   (array-seq (.querySelectorAll el selector))))


(defn attr [$el s]
  (if $el (.getAttribute $el s) nil))

(defn href [$el]
  (attr $el "href"))

(defn dataset [el]
  (js->clj (.parse js/JSON (.stringify js/JSON el.dataset))
           :keywordize-keys true
           )
  )


(defn dom-ctx [params]
  {
   ;;
   ;; js
   :add-script {
                :fn (fn [src]
                      (let [chan-factory (base/&chan-factory params)
                            ch (base/make-chan chan-factory (base/rand-sid))]

                           (add-script src (fn []
                                             (async/put! ch {:loaded src})
                                             ))
                           ch
                           )
                      )
                }

   ;;
   ;; gets html elements
   :query-selector {
                    :fn (fn [selector]
                          (.querySelector (.-body js/document) selector))
                    }

   :query-selector-all {
                        :fn (fn [selector]
                              (array-seq (.querySelectorAll (.-body js/document) selector)))
                        }

   :query-selector-all* {
                         :fn (fn [selector]
                               (let [els (array-seq (.querySelectorAll (.-body js/document) selector))]
                                 (reduce (fn [a el]
                                           (assoc a (base/rand-sid "el-")
                                                    [:identity el] )
                                           ) {} els)
                                 )
                               )
                         :expands? true
                         }

   ;;
   ;; CSS
   :css-rule    {
                        :fn (fn [rule]
                              ;; todo: maybe add a style with specific id
                              (css-add-rule! rule)
                              true)
                        }

   ;; newline separated css ruls [".sector" "rule: aaa;\n rule: bbb;" ]
   :css-rules* { :fn (fn [[selector body]]
                       ;; maybe using expands here is overkill - as we'll be adding new style els
                      (let [lines (str/split-lines body)]
                           (reduce (fn [a line]
                                     (assoc a (base/rand-sid "css-") [:css-rule (str selector " { " line " }")])
                                     )
                             {} lines)
                           )
                      )
               :expands? true
               }

   ;; todo: convenience wrapper for working with collection instead single css rule
   }
  )


;; vanilla js

(defn <scraping-ui> []
  (if-let [prev-el (.querySelector (.-body js/document) ".woof-scraper-ui")]
    (do
      ;; what to do with existing scraping ui?
      (set! (. prev-el -innerHTML) "")
      )
    (let [el (dom/createDom "div" "woof-scraper-ui"
                            "")]

      ;; provide default styling
      (set! (-> el .-style .-zIndex) "10000")
      (set! (-> el .-style .-position) "fixed")
      (set! (-> el .-style .-bottom) "0px")
      (set! (-> el .-style .-left) "0px")
      (set! (-> el .-style .-width) "100%")
      (set! (-> el .-style .-paddingLeft) ".5rem")
      (set! (-> el .-style .-backgroundColor) "rgb(241 251 255)")
      (set! (-> el .-style .-borderTop) "1px solid #000")


      (css-add-rule! ".woof-scraper-ui .panel { display: inline-flex;  flex-flow: row wrap; align-items: baseline; }")
      (css-add-rule! ".woof-scraper-ui .panel + .panel { margin-left: .5rem; }")
      (css-add-rule! ".woof-scraper-ui .panel button { margin: .5rem .1rem; }")
      (css-add-rule! ".woof-scraper-ui .panel header { font-weight: bolder; margin-right: .25rem }")

      (css-add-rule! ".woof-scraper-ui #wf-indicator { height: 12px;\n  width: 12px;\n  background-color: #000;\n  border-radius: 50%; }")

      (css-add-rule! "@keyframes dots {\n    0%, 20% {\n        opacity: 20%;\n    }\n    40% {\n        opacity: 30%;\n    }\n    60% {\n        opacity: 70%;\n    }\n    80%, 100% {\n        opacity: 100%;\n    }\n}\n")
      (css-add-rule! ".woof-scraper-ui .woof-indicator-blink { animation: dots 1500ms steps(5, end) infinite; }")
      ;;
      ;; add a placeholder element to dom
      (add-el! ".woof-scraper-ui" el)
      )
    )


  )

(defn ui-add-el! [el]
  (let [scraper-ui (.querySelector (.-body js/document) ".woof-scraper-ui")]
    (dom/appendChild scraper-ui el)
    )
  )

(defn gen-add-css-handler [class]
  {:fn (fn [el]
         (classes/add el class)

         true)
   })


(defn on-click [btn handler]
  (goog.events.listen btn goog.events.EventType.CLICK handler))
