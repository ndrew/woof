(ns ^:figwheel-hooks ^:figwheel-always
  woof.client.browser.dom
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


(defn dom-ctx [params]
  {

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