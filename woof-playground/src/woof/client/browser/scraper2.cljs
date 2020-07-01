(ns ^:figwheel-hooks ^:figwheel-always
  woof.client.browser.scraper2
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

    [woof.client.ws :as ws]
    ))


;; document.querySelectorAll('#root > div > div > main')

(defn children-seq
  "return html elements as a sequence"
  [el]
  (if (nil? el)
    []
    (let [children (array-seq (.-children el))]
      #_(if (> (count children) 0)
          (.warn js/console el)
          )
      children
      )
    )
  )


(defn parse-listing
  "function that tries to extract the listing from html element"
  [el]

  ;; as we don't have proper class names/ids, so we have to rely on the order of children elements.

  (let [[root] (children-seq el)
        [img-el content-el] (children-seq root)
        [_ addr distr details text] (children-seq content-el)

        [price info] (children-seq details)
        [total-price per-meter] (children-seq price)]


    (when-not total-price
      ;; no price, so it's likely that we are parsing add, so mark it as not parsed
      (classes/add root "woof-error"))

    ;;  try extracting price
    (let [url (.getAttribute total-price "href")
          id (get (str/split (first (str/split url "?")) "/") 3)

          price-text (str/trim (dom/getTextContent total-price))
          price-trimmed (str/replace price-text #"\s" "")]

      {
       :url url
       :id id

       ;; todo: proper parse price
       :price-total price-text
       :price-meter (dom/getTextContent per-meter)
       :price-trimmed price-trimmed

       ;:d price
       :street (.getAttribute (.querySelector addr "div[title]") "title")
       :text (dom/getTextContent text)

       ;; todo: extract image
       }
      )
    )
  )


(defn ctx-fn [params]
  {

   :print {
           :fn (fn [v]
                 (.log js/console (d/pretty v))
                 v)
           }

   :str-join {
              :fn       (fn [strings]
                          (str/join strings))
              :collect? true
              }

   :process* (base/expand-into :process)

   :process  {
              :fn (fn [el]
                    (try
                      (parse-listing el)
                      (catch js/Error e
                        (do
                          (.warn js/console
                                 "error during parsing" el
                                 e)
                          {:error :error}
                          ))))
              }

   :filter-errors {
                    :fn (fn [kv]
                          (reduce (fn [a [k v]]
                                    (if-not (:error v)
                                        (assoc a (base/rand-sid "filter-") [:identity k])
                                        a)) {} kv)
                          )
                    :expands? true
                    }
   }
  )

(defn steps-fn [params]
  {

   ::selector [:identity "#root > div > div > main > section > article"]

   ;; add custom css rules to better identify things we will be parsing
   ::custom-css-for-listing [:css-rule ::listing-css-rule]
   ::custom-css-for-error [:css-rule ".woof-error { outline: 5px solid red; }"]

      ::outline [:identity " { outline: 1px solid lime; }"]
      ::listing-css-rule [:str-join [::selector ::outline]]


   ;; get the elements to be parsed
   ::els [:query-selector-all ::selector]

   ;; process the elements similar to pmap
   ::processed-elements [:process* ::els]

   ;; a tricky way for storing all expanded step-ids
   ::k [:mem-k* ::processed-elements]

   ;; zip all listings back to map with {<sid> <listing>}
   ::listings-kv [:*kv-zip [::k ::processed-elements]]

   ;; filter out listings that were not parsed
   ::filtered-listings [:filter-errors ::listings-kv]

   ;; resulting listings
   ::LISTINGS [:collect ::filtered-listings]

  ;; for now do not output the listings within workflow
   ;; ::print-listings [:print ::LISTINGS]
   }
  )


(defn opt-fn [params]
  {
   :op-handlers-map {
                     :done  (fn [result]

                              (.groupCollapsed js/console "Workflow ended.")
                              (.log js/console
                                    "Full results map: " result)

                              (.log js/console
                                    "Parsed listings: "
                                    (::LISTINGS result))

                              (.groupEnd js/console)
                              )

                     :error (fn [result]
                              (.error js/console result))

                     }

   })
