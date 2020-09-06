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




(defn add-el!
  ([el-present-selector el2add]
  "dynamically adds or replaces element to dom"
   (add-el! (.-body js/document) el-present-selector el2add)
  )
  ([root-el el-present-selector el2add]
   (let [els (array-seq (.querySelectorAll root-el
                                           el-present-selector
                                           ))]

     (if-not (empty? els)
       (dom/replaceNode el2add (first els))          ;; replace el from dom
       (dom/appendChild root-el el2add)       ;; append element
       )
     )
   )
  )


(defn add-first-el!
  ([el-present-selector el2add]
   "dynamically adds or replaces element to dom"
   (add-el! (.-body js/document) el-present-selector el2add)
   )
  ([root-el el-present-selector el2add]
   (let [els (array-seq (.querySelectorAll root-el
                                           el-present-selector))]
     (if-not (empty? els)
       (dom/replaceNode el2add (first els))          ;; replace el from dom
       (dom/insertChildAt root-el el2add 0)       ;; append element
       )
     )
   )
  )


(defn add-stylesheet [src class-name]

  (let [el (dom/createElement "link")]
    (set! (.-href el) src)
    (set! (.-rel el) "stylesheet")
    (set! (.-type el) "text/css")

    (if class-name
      (set! (.-className el) class-name))

    (set! (.-onload el) (fn []
                          ; todo: check
                          ))

    (dom/appendChild (.-head js/document) el)

    )
  )


(defn css-add-rule!
  ([rule]
   (css-add-rule! rule ""))
  ([rule class-name]

   (let [style-el (.createElement js/document "style")]
     (if class-name
       (set! (.-className style-el) class-name))

     (.appendChild (.-head js/document) style-el)
     (let [sheet (.-sheet style-el)]
       (.insertRule sheet rule)))
   )
  )


(defn remove-added-css []
  (let [style-els (array-seq (.querySelectorAll js/document ".woof-css"))]
    (doseq [el style-els]
      (dom/removeNode el))
    )
  )

(defn query-selector
  ([selector]
   (query-selector (.-body js/document) selector))
  ([el selector]
   (.querySelector el selector)))


(defn query-selector*
  ([selector]
   (query-selector* (.-body js/document) selector))
  ([el selector]
   (array-seq (.querySelectorAll el selector))))

(def q query-selector)
(def q* query-selector*)


(defn attr [$el s]
  (if $el (.getAttribute $el s) nil))

(defn href [$el]
  (attr $el "href"))

(defn html! [el h]
  (set! (. el -innerHTML) h)  )


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
                              (css-add-rule! rule "woof-css")
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


   :css-file {:fn (fn [src]
                    (add-stylesheet src "woof-css"))}




   ;; todo: convenience wrapper for working with collection instead single css rule

   ;;
   ;; scroll by a page
   :scroll            {:fn (fn [dy]
                             (.scrollBy js/window 0  (* (.-innerHeight js/window)
                                                        dy
                                                        )))}


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

      (css-add-rule! ".woof-scraper-ui #wf-indicator { height: 12px;\n  width: 12px;\n  background-color: #000;\n  border-radius: 50%; border: 1px solid rgba(0,0,0,.133); }")

      (css-add-rule! "@keyframes dots { from { opacity: 0.333; } to { opacity: 1; } } ")
      (css-add-rule! ".woof-scraper-ui .woof-indicator-blink { animation: dots 1500ms infinite; animation-direction: alternate; }")
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

(defn btn!
  ([title handler]
   (let [btn-el (dom/createDom "button" "" title)]
     ;; todo: expose api actions and show them in um

     (on-click btn-el handler)
     btn-el
     )
   )
  ([title handler panel]
   (dom/appendChild panel (btn! title handler))
   )
  )





;; breadth first search of element children and keep the selector and other nice properties, like
;; if the node has text. parsing can be skipped via skip-fn, in order to not traverse svgs, or other stuff
(defn el-map
  [root skip-fn]

  (loop [ret []
         queue (into cljs.core/PersistentQueue.EMPTY
                     (map-indexed
                       (fn [i el]
                         {:$   [{:t (.-tagName el)
                                 :i (inc i)}]
                          :el  el}
                         )
                       (array-seq (.-children root))))]
    (if (seq queue)
      (let [node (peek queue)
            $ (get node :$)
            $el (get node :el)

            children (array-seq (.-children $el))
            child-count (count children)
            text (.-textContent $el)

            has-text? (not= "" text )
            use-text? (volatile! true) ;; ugly, but works

            *i (volatile! 0)

            children-nodes (reduce
                             (fn [a el]
                               (vswap! *i inc)
                               (if-not (skip-fn el $)
                                 (do
                                   (if (and has-text? (str/includes? text (.-textContent el)))
                                     (vreset! use-text? false))
                                   (conj a
                                         {:$   (conj $
                                                     {:t (.-tagName el)
                                                      :i @*i
                                                      :child-count child-count
                                                      })
                                          :el  el}))
                                 a
                                 ))
                             []
                             children)

            new-ret (conj ret
                          (merge
                            {
                             :child-count child-count
                             ;; if at least one of children has same text - ommit
                             :text (if @use-text? text "")
                             }
                            node
                            ))
            new-children (into (pop queue)
                               children-nodes)
            ]
        (recur new-ret new-children)
        )
      ret
      )
    )
  )


(defn to-selector [$]
  (str/join " > "
            (map #(str
                    (:t %)
                    (if (> (:child-count %) 1)
                      (str ":nth-child(" (:i %) ")")
                      "")
                    ) $))
  )

(defn sel-text-el []
  (let [selection (.getSelection js/window)
        node (.-anchorNode selection)]
    (.-parentElement node)
    )
  )

