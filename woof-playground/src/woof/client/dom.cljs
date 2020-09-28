(ns woof.client.dom
  (:require
    [goog.object :as gobj]
    [goog.events]
    [goog.dom :as dom]
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

(defn txt [el]
  (dom/getTextContent el))

(defn tag-only [el]
  (str/replace (.-outerHTML el)
               (.-innerHTML el)
               ""
               )
  )


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
                    (.log js/console "LOADING CSS: " src)
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




(defn to-selector [$]
  (str/join " > "
            (map #(str
                    (:t %)
                    (if (> (:child-count %) 1)
                      (str ":nth-child(" (:i %) ")")
                      "")
                    ) $))
  )

;; breadth first search of element children and keep the selector and other nice properties, like
;; if the node has text. parsing can be skipped via skip-fn, in order to not traverse svgs, or other stuff
(defn el-map
  [root
   & {:keys [skip-fn node-fn] :or {
                                   skip-fn (fn [_ _] false)
                                   node-fn (fn [node]
                                             {}
                                             )
                                   }}
   ]



  ;; todo: make skip-fn optional

  (let [make-node (fn [node]
                 (merge node
                        (node-fn node))
                 )]
    (loop [ret []
           queue (into cljs.core/PersistentQueue.EMPTY
                       (map-indexed
                         (fn [i el]
                           (let [selector-data [{:t (.-tagName el)
                                                 :i (inc i)}]]

                             (make-node
                               {
                                :$          selector-data
                                :tag       (:t (last selector-data))
                                :_$         (to-selector selector-data)

                                :el         el
                                :parent-idx -1
                                })
                             )
                           )
                         (array-seq (.-children root))))
           idx 0
           ]

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
                                   (let [selector-data (conj $
                                                             {:t (.-tagName el)
                                                              :i @*i
                                                              :child-count child-count
                                                              })]

                                     (if (and has-text? (str/includes? text (.-textContent el)))
                                       (vreset! use-text? false))

                                     (conj a
                                           {:$   selector-data
                                            :_$  (to-selector selector-data)
                                            :tag  (:t (last selector-data))
                                            :el  el
                                            :parent-idx idx
                                            }))
                                   a
                                   ))
                               []
                               children)

              new-node (make-node
                         (merge
                           {
                            :idx idx
                            }
                           {
                            ;; if at least one of children has same text - ommit
                            :child-count child-count
                            :text (if @use-text? text "")
                            }
                           node
                           )
                         )

              new-ret (conj ret new-node)
              new-children (into (pop queue)
                                 children-nodes)
              ]
          (recur new-ret new-children
                 (inc idx)))
        ret
        )
      )
    )

  )




(defn el-plan-as-tree
  "converts el-map plan back to a tree form"
  [plan]
  (let [gr (group-by :parent-idx plan)
        roots (sort (keys gr))

        new-plan (loop [plan plan
                        parent-keys (reverse roots)]
                   (if (and (seq parent-keys) (>= (first parent-keys) 0))
                     (let [parent-k (first parent-keys)
                           child-keys (reduce (fn [a node] (conj a (get node :idx))) #{} (get gr parent-k))

                           children (filter (fn [item]
                                              (child-keys (:idx item))) plan)

                           new-plan (update-in plan [parent-k]
                                               update :children concat children)
                           ]

                       ;(.groupCollapsed js/console (pr-str parent-k))
                       ;(.log js/console "updating" parent-k "with" child-keys children)
                       ;(.log js/console new-plan)
                       ;(.groupEnd js/console)

                       (recur new-plan (rest parent-keys))
                       )
                     plan
                     )
                   )
        ]

    (filter (fn [item] (= -1 (:parent-idx item))) new-plan)
    )
  )


(defn sel-text-el []
  (let [selection (.getSelection js/window)
        node (.-anchorNode selection)]
    (.-parentElement node)
    )
  )




(defn remove-class* [el class]
  (doseq [el (q* (str "." class))]
    (classes/remove el class)))




;; todo:
(defn parent-while [el pred]
  (loop [el el]
    (if-not (pred el)
      (recur (.-parentElement el))
      )
    )
  ;el.parentElement.parentElement.childElementCount
  )


(defn remove-added-css
  ([]
   (remove-added-css []))

  ([additional-classes-2-remove]
   "provide list of css-class without dot."
   ;; find better way of dealing with selector <-> class name mapping
   (let [style-els (array-seq (.querySelectorAll js/document ".woof-css"))]
     (doseq [el style-els]
       (dom/removeNode el)))

   (doseq [s* additional-classes-2-remove]
     (doseq [el (q* (str "." s*))]
       (classes/remove el s*)))
   )

  )


(defn save-json [edn]

  (let [a (.createElement js/document "a")]
    (.appendChild (.-body js/document) a)
    (set! (.-style a) "display: none")

    (let [s (.stringify js/JSON (clj->js edn))
          blob (js/Blob. (clj->js [s])
                         (js-obj "type" "octet/stream"))
          url (.createObjectURL (.-URL js/window) blob)]

      (set! (.-href a) url)
      (set! (.-download a) (str (u/now) ".json"))

      (.click a)
      (.revokeObjectURL (.-URL js/window) url)
      ;; todo: remove a element
      )
    )

  )


(defn dataset->clj [el]
  (js->clj (.parse js/JSON (.stringify js/JSON el.dataset))
           :keywordize-keys true
           )
  )


(defn enrich-node [n]
  (let [$ (get n :$)
        curr-tag (:t (last $))
        selector (get n :_$)

        $el (:el n)]

    (merge
      {
       :selector selector
       :tag      curr-tag
       ;; :tag-full (wdom/tag-only $el)

       :data    (dataset->clj $el)
       :text    (:text n)
       }

      (if (.hasAttributes $el)
        (let [attrs (.-attributes $el)
              l (.-length attrs)]
          {:attrs (reduce (fn [a i]
                            (let [kv (gobj/get attrs i)]
                              (assoc a (.-name kv) (.-value kv))
                              )
                            ) {} (take l (range)))}
          )
        {}
        )

      (if (= "IMG" curr-tag) {:img-src (attr $el "src")} {})

      (if (= "A" curr-tag)
        {:href (attr $el "href")} {})

      (if (= "TIME" curr-tag)
        {:datetime (attr $el "datetime")}
        {})

      )
    )
  )


(defn copy-to-clipboard-handler [v]
  (when js/navigator.clipboard.writeText
    (let [clipboard js/navigator.clipboard

          copy-handler (fn []
                         (-> (.writeText clipboard (d/pretty! v))
                             (.then (fn [response] (.log js/console "Copied to clipboard - " response))
                                    (fn [err]      (.warn js/console "Failed to copy to clipboard" err))))
                         )
          ]

      (let [btn-el (dom/createDom "button" ""
                                  "copy results to clipboard")]

        (goog.events.listen btn-el goog.events.EventType.CLICK copy-handler)
        (ui-add-el! btn-el)

        (.focus btn-el)
        )
      )
    )

  )