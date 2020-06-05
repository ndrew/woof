(ns ^:figwheel-hooks woof.client.dom
  (:require
    [goog.object]
    [goog.dom :as dom]
    [goog.object]
    [goog.dom.query :as query]
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
                          ;(js-debugger)
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

   :mem-k*             {
                        :fn       (fn [o]
                                    {(base/rand-sid "mem-k-") [:identity {:k o}]})
                        :expands? true
                        }

   ;; kv zipping - joins keys with values
   :*kv-zip            {
                        :fn       (fn [[[k] vs]]
                                    (let [ks (:k k)]
                                         (apply assoc {} (interleave ks vs))
                                         ))
                        :collect? true
                        }

   :identity {:fn identity }

   :collect  {
              :fn       (fn [xs]
                          ; (.warn js/console xs)
                          xs)
              :collect? true
              }



   }
  )


(defn <scraping-ui> []
  (let [el (dom/createDom "div" "woof-scraper-ui"
                          "")]

    #_(set! (-> el .-style )
          "")
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