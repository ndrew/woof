(ns woof.client.dom
  (:require
    [goog.object]
    [goog.dom :as dom]
    [goog.object]

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



(defn dom-ctx [params]
  {

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

   ;; gets html elements
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

   :css-rule    {
                        :fn (fn [rule]
                              (let [style-el (.createElement js/document "style")]
                                   (.appendChild (.-head js/document) style-el)

                                   (let [sheet (.-sheet style-el)]
                                     (.insertRule sheet rule)
                                     )
                                   )
                              true)
                        }

   ;; newline separated css ruls [".sector" "rule: aaa;\n rule: bbb;" ]
   :css-rules* { :fn (fn [[selector body]]
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
  (let [el (dom/createDom "div" "woof-scraper-ui"
                          "")
        ]

    ;; provide default styling
    (set! (-> el .-style .-zIndex) "10000")
    (set! (-> el .-style .-position) "fixed")
    (set! (-> el .-style .-bottom) "0px")
    (set! (-> el .-style .-left) "0px")
    (set! (-> el .-style .-width) "100%")
    (set! (-> el .-style .-paddingLeft) ".5rem")
    (set! (-> el .-style .-backgroundColor) "rgba(255, 240, 240, 1)")
    (set! (-> el .-style .-borderTop) "1px solid #000")


    ;;
    ;; add a placeholder element to dom
    (add-el! ".woof-scraper-ui" el)
    )
  )

(defn ui-add-el! [el]
  (let [scraper-ui (.querySelector (.-body js/document) ".woof-scraper-ui")]
    (dom/appendChild scraper-ui el)
    )
  )