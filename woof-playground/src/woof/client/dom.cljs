(ns woof.client.dom
  (:require
    [goog.object :as gobj]
    [goog.events]
    [goog.dom :as dom]
    [goog.dom.classes :as classes]
    [goog.dom.dataset :as dataset]

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


(defn txt-only [el]
  (let [ch (array-seq (.-childNodes el))]
    (reduce (fn [s el']
              (if (= "#text" (.-nodeName el'))
                (str s (.-textContent el'))
                s)
              ) "" ch)
    )
  )


(defn tag-only [el]
  ;; todo: brutal
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

(defn <scraping-ui> [options]
  (if-let [prev-el (.querySelector (.-body js/document) ".woof-scraper-ui")]
    (do
      ;; what to do with existing scraping ui?
      (set! (. prev-el -innerHTML) "")
      )
    (let [fixed-side (get options :ui/fixed-side :bottom)
          el (dom/createDom "div" (str "woof-scraper-ui"
                                       " "
                                       "woof-fixed-" (name fixed-side))
                            "")

          ]

      ;; provide default styling
      (set! (-> el .-style .-zIndex) "10000")
      (set! (-> el .-style .-position) "fixed")

      (cond
        (= :bottom fixed-side)
        (do
          (set! (-> el .-style .-bottom) "0px")
          (set! (-> el .-style .-left) "0px")
          (set! (-> el .-style .-width) "100%")

          (let [doc (-> js/document .-documentElement)]
            (css-add-rule! "body { margin-bottom: var(--height); }")
            (css-add-rule! ".woof-scraper-ui { height: var(--height); }")
            (.setProperty (-> doc .-style ) "--height" (str 75 "px")))

          )

        (= :left fixed-side)
        (do
          (set! (-> el .-style .-top) "0px")
          (set! (-> el .-style .-left) "0px")
          (set! (-> el .-style .-height) "100%")

          (let [doc (-> js/document .-documentElement)]
            (css-add-rule! "body { margin-left: var(--width); }")
            (css-add-rule! ".woof-scraper-ui { width: var(--width); }")

            (.setProperty (-> doc .-style ) "--width" (str 200 "px")))
          )

        )


      ; (set! (-> el .-style .-paddingLeft) ".5rem")
      (set! (-> el .-style .-backgroundColor) "rgb(241 251 255)")
      (set! (-> el .-style .-borderTop) "1px solid #000")


      (css-add-rule! ".woof-scraper-ui .panel { display: inline-flex;  flex-flow: row wrap; align-items: baseline; }")
      (css-add-rule! ".woof-scraper-ui .panel + .panel { margin-left: .5rem; }")
      (css-add-rule! ".woof-scraper-ui .panel button { margin: .5rem .1rem; }")
      (css-add-rule! ".woof-scraper-ui .panel header { font-weight: bolder; margin-right: .25rem }")

      (css-add-rule! ".woof-scraper-ui #wf-indicator { height: 12px;\n  width: 12px;\n  background-color: #000;\n  border-radius: 50%; border: 1px solid rgba(0,0,0,.133); }")

      (css-add-rule! "@keyframes dots { from { opacity: 0.5; } to { opacity: 1; } } ")
      (css-add-rule! ".woof-scraper-ui .woof-indicator-blink { animation: dots 1500ms infinite; animation-direction: alternate; }")
      ;;
      ;; add a placeholder element to dom
      (add-el! ".woof-scraper-ui" el)
      )
    )
  )

(defn scraping-ui__inc [delta]
  (if-let [el (.querySelector (.-body js/document) ".woof-scraper-ui")]
    (if (classes/has el "woof-fixed-left")
      (let [h (-> el .-clientWidth)
            doc (-> js/document .-documentElement)
            ]
        (set! (-> el .-style .-minWidth) (str (+ h delta) "px"))
        (.setProperty (-> doc .-style ) "--width" (str (+ h delta) "px"))
        )
      ;; else bottom
      (let [h (-> el .-clientHeight)
            doc (-> js/document .-documentElement)
            ]
        (set! (-> el .-style .-minHeight) (str (+ h delta) "px"))
        (.setProperty (-> doc .-style ) "--height" (str (+ h delta) "px"))
        )
      )

  )
)


(defn scraping-ui__togle-details []
  (classes/toggle (q ".woof_details") "woof_visible")
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
                    (if-let [nth-i (:nth-child %)]
                      (str ":nth-child(" nth-i ")")
                      "")
                    #_(if (> (:child-count %) 1)
                      (str ":nth-child(" (:i %) ")")
                      "")
                    ;;
                    ) $))
  )


(defn shorten-selector-string [selector parent-selector]
  (if (= 0 (.indexOf selector parent-selector))
    (str/trim (subs selector (count parent-selector)))
    selector))


(defn default-$-fn [parent-node-info node-info]
  (let [;
        i (:i node-info)
        prev-selectors (:$ node-info)

        el (:el node-info)

        el-children (array-seq (.-children el))

        child-count (count (get parent-node-info :children []))

        ;; _ (.log js/console node-info)

        $-info (merge
                 {
                  :t         (.-tagName el)
                  :i         i

                  :idx (:idx node-info)
                  :parent-idx (:parent-idx node-info)

                  :classes (into #{} (str/split (attr el "class") #"\s+")) ;; todo: filter out empty?
                  ;;
                  ; :child-count child-count

                  :child-count (count el-children)
                  }
                 (if (> child-count 1)
                   {:nth-child i}
                   {})
                 )
        ]
    ;; append
    (vec (conj prev-selectors
               $-info
               ))
    )
  )

;; breadth first search of element children and keep the selector and other nice properties, like
;; if the node has text. parsing can be skipped via skip-fn, in order to not traverse svgs, or other stuff
;; - flattens the tree
(defn el-map
  [root
   & {:keys [skip-fn node-fn
             to-selector $-fn]
      :or   {
             skip-fn     (fn [_ _] false)
             node-fn     (fn [node] {})

             $-fn        default-$-fn

             to-selector to-selector
             }}
   ]

  (let [;; pass root here? or use .-parent el ?
        make-node (fn [parent-el node]
                    (merge node
                          (node-fn parent-el node)))
        parent-node-info {
                          :el root
                          :children (array-seq (.-children root))
                          ;;
                          }
        ;; fills queue with top level nodes
        initial-queue-fn (fn [i el]
                   ;; todo: handle skip node on the first level also
                   (let [node-info {
                                    :el         el

                                    :$ []

                                    ;;
                                    :idx        (inc i)
                                    :i          (inc i)

                                    :parent-idx 0

                                    ;; el dependant
                                    :tag        (.-tagName el)
                                    ;; todo: handling root node text
                                    :text       ""        ;(.-textContent el)
                                    }

                         selector-data ($-fn parent-node-info node-info)]

                     ;; queue node-info
                     (merge
                       node-info
                       {
                        :$          selector-data     ;; stores info for building selector
                        ;; string selector implementation -
                        ;;:_$         (to-selector selector-data) ;; - can be triggered later, as we may not have all info
                        })
                     )
                   )

        ;; todo: merge initial-queue-fn with impl in loop

        result
        (loop [ret []
               queue (into #queue []
                           (map-indexed initial-queue-fn (array-seq (.-children root))))
               counter (count (array-seq (.-children root)))]

          (if (seq queue)
            (let [node (peek queue)

                  $ (get node :$)

                  $el (get node :el) ;; parent/root

                  children (array-seq (.-children $el))
                  child-count (count children)
                  text (.-textContent $el)

                  has-text? (not= "" text)
                  use-text? (volatile! true)              ;; ugly, but works

                  idx (:idx node)

                  ;; migrate to loop some day
                  *i (volatile! 0)

                  parent-node-info {
                                    :el $el
                                    :children children
                                    :text text
                                    }

                  children-nodes (reduce
                    (fn [a el]
                      (vswap! *i inc)
                      (if (not (skip-fn el $))
                        (let [child-node-info {
                                               :tag        (.-tagName el)
                                               :el         el
                                               :idx        (+ counter @*i)
                                               :parent-idx idx
                                               :i          @*i

                                               :$ $ ;; prev-selectors

                                               }]

                          ;; tricky way to find extract text
                          (if (and has-text? (str/includes? text (.-textContent el)))
                            (vreset! use-text? false))

                          (conj a
                                (merge child-node-info
                                       (let [selector-data ($-fn parent-node-info child-node-info)]
                                         {
                                          :$  selector-data
                                          })
                                       )))
                        a
                        ))
                        [] children)

                  ;;
                  new-node (make-node
                             $el
                             (merge
                               {
                                ;; if at least one of children has same text - ommit
                                :child-count child-count
                                :text        (if @use-text? text "")
                                }
                               node
                               {
                                :_$ (to-selector $)
                                }
                               )
                             )

                  new-ret (conj ret new-node)

                  new-children (into (pop queue)
                                     children-nodes)

                  ]
              (recur new-ret new-children
                     (+ counter (count children-nodes))))
            ret
            )
          )]
    result
    )
  )



(defn _$-enrich [$-stats $]
  (let [tag (:t $)
        sub-$-stats (get $-stats (:parent-idx $))]

    (-> $
        (assoc :unique [])
        ;; globally unique tag
        (update :unique concat (if (= 1 (get $-stats tag)) [tag] []))
        ;; globally unique class
        (update :unique concat
                (reduce (fn [a x]
                          (let [class (str "." x)]
                            (if (= 1 (get $-stats class))
                              (conj a class)
                              a))) [] (:classes $)))
        ;; parent
        (assoc :non-unique [])
        (update :non-unique concat (if (= 1 (get sub-$-stats tag)) [tag] []))
        (update :non-unique concat
                (reduce (fn [a x]
                          (let [class (str "." x)]
                            (if (= 1 (get sub-$-stats class))
                              (conj a class)
                              a))) [] (:classes $)))
        )
    )
  )


;; builds string selector for $ enriched with usage frequency
(defn $-selector [nu-$]
  (loop [$-s (reverse nu-$)
         res ""]
    (if-not (seq $-s)
      res
      (let [$ (first $-s)]

        (if-let [unique (first (get $ :unique []))]
          (str unique " " res)
          (recur (rest $-s)
                 (if-let [non-unique (first (get $ :non-unique []))]
                   (str non-unique " " res)
                   (str res
                        (:t $) (str/join (map #(str "." %)
                                              (get $ :classes #{}))))
                   )
                 )
          )
        )
      )
    )
  )



;;
;; TODO: next version of el-map
;;
(defn nu-el-map
  [root
   & {:keys [skip-fn
             node-fn
             top-selector-fn

             MAX-LEVEL
             ]
      :or {
           skip-fn (fn [_ _] false)
           node-fn (fn [node] {})
           top-selector-fn (fn [base el] {})

           MAX-LEVEL 5
           }}
   ]

  (let [make-node (fn [node] (merge node (node-fn node)))]

    ;(.groupCollapsed js/console "LMAP: root" root)
    (let [*STATS (volatile! {:tags {}

                             })

          *level (volatile! 0)

          ;; add first level to
          result (loop [ret []
                 queue (into cljs.core/PersistentQueue.EMPTY
                   (map-indexed
                     (fn [i el]

                       {:i i

                        :l @*level

                        :el el}
                       #_(let [base-selector {:t (.-tagName el)
                                            :i (inc i)}
                             selector-data [(merge base-selector (top-selector-fn base-selector el))]

                             ]

                         (make-node
                           {
                            :$          selector-data
                            :tag        (:t (last selector-data))
                            :_$         (to-selector selector-data)

                            :el         el
                            :parent-idx 0
                            :idx (inc i)
                            ;; todo: handling root node text
                            :text       ""        ;(.-textContent el)
                            })
                         )
                       )
                     (array-seq (.-children root))))

                 counter (count (array-seq (.-children root)))
                 ]

            (if (seq queue)
              (let [node (peek queue)

                    LEVEL (inc (:l node))

                    ;$ (get node :$)

                    $el (get node :el)
                    children (if (<= LEVEL MAX-LEVEL)
                               (array-seq (.-children $el))
                               []
                               )
                    child-count (count children)
                    ;text (.-textContent $el)
                    ;has-text? (not= "" text)
                    ;use-text? (volatile! true)              ;; ugly, but works
                    ;; migrate to loop some day
                    *i (volatile! 0)

                    ;idx (:idx node)

                    children-nodes (reduce
                                     (fn [a el]
                                       (vswap! *i inc)

                                       (conj a
                                             {:el         el
                                              :i           @*i
                                              :l LEVEL
                                              ;:idx (+ counter @*i)
                                              })

                                       #_(if (not (skip-fn el $))
                                         (let [selector-data (conj $
                                                                   (merge
                                                                     {:t           (.-tagName el)
                                                                      :i           @*i
                                                                      :child-count child-count
                                                                      }
                                                                     (if (> child-count 1)
                                                                       {:nth-child @*i}
                                                                       {})
                                                                     )



                                                                   )]

                                           (if (and has-text? (str/includes? text (.-textContent el)))
                                             (vreset! use-text? false))

                                           (conj a
                                                 {:$          selector-data
                                                  :_$         (to-selector selector-data)
                                                  :tag        (:t (last selector-data))
                                                  :el         el
                                                  :idx (+ counter @*i)
                                                  :parent-idx idx
                                                  }))
                                         a
                                         ))
                                     []
                                     children)

                    new-node (merge node
                                    {
                                      :child-count child-count
                                    }
                                    )
                            #_(make-node
                               (merge
                                 {
                                  ;; if at least one of children has same text - ommit
                                  :child-count child-count
                                  :text        (if @use-text? text "")
                                  }
                                 node
                                 )
                               )

                    new-ret (conj ret new-node)

                    new-children (into (pop queue)
                                       children-nodes)

                    ]

                ; (.log js/console idx (:parent-idx (last new-ret)) (last new-ret))

                (recur new-ret new-children
                       (+ counter (count children-nodes))))
              ret
              )
            )]
      ; (.groupEnd js/console)
      {
       :nodes result
       :stats @*STATS
       :root root
       }

      )
    )

  )




(defn parent-group [plan]
  (group-by (fn [node] (dec (:parent-idx node))) plan)
  )

;; [{}]

(defn el-plan-as-tree
  "converts el-map plan back to a tree form"
  [plan]
  ;; (.warn js/console "PLAN=" plan)

  (let [gr (parent-group plan)
        roots (sort (keys gr))

        ;;_ (do (.warn js/console gr))

        new-plan (loop [plan plan
                        parent-keys (reverse roots)]

                   ;; (.warn js/console "parent keys=" parent-keys)
                   (if (and (seq parent-keys) (>= (first parent-keys) 0))
                     (let [parent-k (first parent-keys)

                           child-keys (reduce (fn [a node] (conj a (get node :idx))) #{}
                                              (get gr parent-k))

                           children (filter (fn [item]
                                              (child-keys (:idx item))) plan)

                           ;;_ (do (.warn js/console "UPD:" parent-k (get plan parent-k) ))

                           new-plan (update-in plan [parent-k]
                                               update :children concat children)
                           ]

                       ;(.groupCollapsed js/console (pr-str parent-k))
                       ;(.log js/console "updating" parent-k "with" child-keys children)
                       ;(.log js/console new-plan)
                       ;(.groupEnd js/console)

                       ;;(.warn js/console parent-k "new plan=" new-plan)

                       (recur new-plan (rest parent-keys))
                       )
                     plan
                     )
                   )
        ]

    ;; remove first level elements
    (filter (fn [item] (= 0 (:parent-idx item))) new-plan)
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


(defn save-blob [filename s]

  (let [a (.createElement js/document "a")]
    (.appendChild (.-body js/document) a)
    (set! (.-style a) "display: none")

    (let [
          blob (js/Blob. (clj->js [s])
                         (js-obj "type" "octet/stream"))
          url (.createObjectURL (.-URL js/window) blob)]

      (set! (.-href a) url)
      (set! (.-download a) filename)

      (.click a)
      (.revokeObjectURL (.-URL js/window) url)
      ;; todo: remove a element
      )
    )

  )


(defn save-edn [filename edn]
  (save-blob filename (d/pretty! edn))
  )


(defn save-json [filename edn]
  (save-blob filename (.stringify js/JSON (clj->js edn)))
  )


(defn dataset->clj [el]
  (js->clj (.parse js/JSON (.stringify js/JSON el.dataset))
           :keywordize-keys true
           )
  )


(defn enrich-node [root-el n]
  (let [$ (get n :$)
        curr-tag (:t (last $))
        selector (get n :_$)

        $el (:el n)]

    (merge
      {
       ; :selector selector
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




(defn copy-to-clipboard [v]
  (let [clipboard js/navigator.clipboard
        copy-value (if (string? v) v (d/pretty! v))
        ]
    (-> (.writeText clipboard copy-value)
        (.then (fn [response] (.log js/console "Copied to clipboard!"))
               (fn [err]      (.warn js/console "Failed to copy to clipboard" err))))
    ))



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

(defn clipboard-ctx [params]
  {
   :copy-to-clipboard   {:fn copy-to-clipboard-handler}
   }
  )


;; keep track of injected styles
(defonce *styles-added-map (atom {}))

(defn _add-style-once-steps-fn [css-url params]   ;; add css styles only once
  (if-not (get @*styles-added-map css-url)
    (do
      (swap! *styles-added-map assoc css-url true)
      { (base/rand-sid "CSS-") [:css-file css-url]})
    {}))

;;
;; keyboard

(defn chord
  ([code]
  {:code code
   :shift false :ctrl false :alt false :meta false })
  ([code & {:keys [shift ctrl meta alt]
            :or {shift false ctrl false alt false meta false}}]
   {:code code
    :shift shift
    :ctrl ctrl
    :alt alt
    :meta meta}))



(defn outer-html [els]
  (if (instance? js/Element els)
    (.-outerHTML els)
    (if (and (seqable? els)
             (seq els))
      (reduce (fn [s el] (str s (. el -outerHTML))) "" els)
      (str els))
    ))


(defn mark! [el parse-id]
  (when el
    (if (instance? js/Element el)
      (do
        (classes/set el "DDD")
        (dataset/set el "parseId" parse-id))
      (if (and (seqable? el)
               (seq el))
        (doseq [el el]
          (classes/set el "DDD")
          (dataset/set el "parseId" parse-id)))))
  el)


(defn has-class? [el class]
		(classes/has el class))