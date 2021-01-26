(ns woof.client.playground.scraper.cc
  (:require
    [cljs.core.async :as async]
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]

    [goog.dom :as dom]
    [goog.dom.classes :as classes]
    [goog.Uri :as uri]

    [rum.core :as rum]

    [woof.base :as base]
    [woof.data :as d]

    ;[clj-fuzzy.metrics :as metrics]

    [woof.utils :as utils]

    [woof.browser :as woof-browser]

    [woof.client.dom :as wdom :refer [q q* html! ]]
    [woof.client.playground.ui :as pg-ui]
    [woof.client.playground.ui.wf :as wf-ui]
    [woof.client.ws :as ws]
    [woof.client.playground.ui.html :as html-ui]

    [woof.wfs.evt-loop :as evt-loop]
    ; old dashboard
    ;[woof.client.playground.scraper.tw :as dashboard]
    [woof.utils :as u]
    [woof.client.common :as common]
    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))

;; provide selector to compare items in

;; SELECTOR "#test-html > .catalog-item"
;; yt
;; SELECTOR "#test-html > #contents > ytd-video-renderer"

;;
;; tg
; history: whole message
;; (def SELECTOR "#test-html .im_history_wrap .im_history_message_wrap")

;; history message text (kind of lensing)
(def SELECTOR "#test-html .im_history_wrap .im_history_message_wrap .im_message_text")


;;
;; UI


;;
(rum/defc <html-source> < rum/reactive
  [*dict]
  (let [ks (get-in @*dict [:kv-ks] [])

        f (fn [x]
            (let [t0 (u/now)]
              (ws/GET
                (str "http://localhost:8081/kv/get/" x)
                (fn [_data]
                  (let [t1 (u/now)
                        data (d/to-primitive _data)]

                    (html! (q "#test-html") data)

                    (let [now (u/now)]
                      (.log js/console "total: " (- now t0) "parse: " (- now t1) "request: " (- t1 t0)))
                    )))))

        main-actions [
                      ["👀" (fn [] (classes/toggle (wdom/q "#test-html-container") "visible"))]
                      []
                      ["KV: load" (fn [] (ws/GET "http://localhost:8081/kv/list"
                                                  (fn [ks]
                                                    (swap! *dict assoc-in [:kv-ks] (d/to-primitive ks)))))]
                      []]
        ]

    [:div.flex.woof-toolbar
     (pg-ui/menubar "HTML: " (vec (concat
                               main-actions
                               (map (fn [x] [(str x) (partial f x)]) ks)
                               )))
     ]
    )
  )


;; two-pass version of $-fn for el-map, that collects usage of tags & classes
(defn _$-fn [*aggr parent-node-info node-info]
  (let [full-selector (wdom/default-$-fn parent-node-info node-info)
        $ (last full-selector)

        {t           :t
         i           :i
         classes     :classes
         child-count :child-count
         p-idx       :parent-idx} $

        clazzes (reduce #(assoc %1 (str "." %2) 1) {} classes)

        ;; stats aggregation fn
        aggr-fn #(-> %
                     (update-in [t] (fnil inc 0))
                     ((partial merge-with + clazzes)))]

    (vswap! *aggr #(-> %
                       (aggr-fn)
                       (update p-idx (fnil aggr-fn {}))))
    full-selector))


;; returns all data for <dashboard>
(defn dashboard-data [el-cfg els compare-ids]
  (let [skip-fn (get el-cfg :skip-fn (fn [$el $] (#{"G" "PATH"} (str/upper-case (.-tagName $el)))))

        el-fn (fn [id]
                (let [*aggr (volatile! {})
                      el-map (wdom/el-map (nth els id)
                                          :skip-fn skip-fn
                                          :node-fn wdom/enrich-node
                                          :$-fn (partial _$-fn *aggr))

                      $-stats @*aggr

                      ;; overwrite el-map
                      el-map (vec (map (fn [x]
                                         (let [nu-$ (map (fn [_x]
                                                      (wdom/_$-enrich $-stats _x)) (:$ x))]


                                           (assoc x
                                             :$ nu-$
                                             :_$ (wdom/$-selector nu-$)
                                             )
                                           )
                                         )
                                       el-map))

                      r {
                         ;;
                         :id     id
                         :el-map el-map

                         :nodes  (reduce (fn [a node] (assoc a (:_$ node) node)) {} el-map)

                         :aggr   $-stats
                         }
                      ]
                  ; (.warn js/console @*aggr)
                  ;;
                  ;; document how these are being produced
                  ;;
                  r))]
    (vec (map el-fn compare-ids))
    )
  )


;;
;;
(rum/defcs <html-analyzer> < rum/reactive
                             (rum/local [0] ::compare-ids)
                             (rum/local 1 ::compare-max)

  [local *wf]

  (let [wf @*wf
        *dict (rum/cursor-in *wf [:state ::dict])]

    [:div.playground-html-analyzer

     (<html-source> *dict)

     ;; todo: ui for providing selector
     [:div.woof-toolbar
      (pg-ui/menubar (str "SELECTOR: " SELECTOR)
                     [
                      ["use default" (fn []
                                         (swap! *dict assoc-in [:curr-selector] SELECTOR))]
                      ["use own" (fn []
                                   (swap! *dict assoc-in [:curr-selector]
                                          (js/prompt "provide selector" SELECTOR)
                                          )
                                    )]
                      ])
      ]

     (if-let [$listings (:curr-selector @*dict)]
       (let [compare-ids @(::compare-ids local)
             compare-max @(::compare-max local)

             els (wdom/q* $listings)

             skip-fn (fn [$el $]
                       (#{; "SVG"
                          "G" "PATH"} (str/upper-case (.-tagName $el))))

             ]
         [:div
          [:div.woof-toolbar.flex

           (pg-ui/menubar (str "Compare " compare-max " elements")
                          [
                           ["+" (fn []
                                  (let [nu-max (swap! (::compare-max local) inc)]
                                    (swap! (::compare-ids local) assoc (dec nu-max) 0))

                                  )]
                           ["-" (fn []
                                  (swap! (::compare-max local) dec)
                                  (swap! (::compare-ids local) #(vec (drop-last %)))
                                  ) ]
                           []])

           (let [CHARS (seq "ABCDEF")]
             [:div
              (map-indexed
                (fn [i j]
                  [:div
                   (pg-ui/menubar (str (nth CHARS i) ":")
                                  (map (fn [n]
                                         [(if (= j n) (str "✅" n) (str n))
                                          #(swap! (::compare-ids local) assoc i n)])
                                       (range (count els))))])
                compare-ids)
              ]
             )
           ]

          ;;
          ;; aggregate all :$
          (if els
            (let [el-cfg {:skip-fn skip-fn}
                  all-data (dashboard-data el-cfg els compare-ids)]
              [:div
                ;; dashboard for analyzing selected
                [:hr]
               [:header "V1: analysing all data"]
               ; hide for now
               (html-ui/<dashboard> all-data)
               ]
              )
            )
          ]
         )
       )
     ]
    )
  )



(rum/defcs <scraping-root> < rum/reactive
  [local *wf]

  (let [wf @*wf]
    [:div
     (if (= :not-started (:status wf))
       [:div "wf is not running"]
       (<html-analyzer> *wf))]))



;;
;; WF definition
(defn wf! [*SWF]


  (let [CHAN-FACTORY (base/chan-factory (atom {}))
        *dict (rum/cursor-in *SWF [:state ::dict])
        *categorizator (rum/cursor-in *SWF [:state ::categorizator])
        ]
    {

     :title       "Scraping HTML Analyzer"
     :explanation [:div.explanation

                   [:p [:code "#test-html"] " contains html for analysis"]
                   [:p "it can be provided manually or via woof KV store"]

                   [:p "Scraper can send html for analysis"]
                   [:ul
                    [:li "extract potentially interesting data"]
                    [:li "compare differences between data"]
                    ]
                   ]

     ;; this state will be added to a wf?
     :state {

             ;; just data for the ui
             ::dict {
                     }
             }

     :init-fns    [
                   (fn [params]
                     {
                      ::categorizator *categorizator
                      ::*dict *dict
                      }
                     )

                   (base/build-init-chan-factory-fn CHAN-FACTORY)
                   (evt-loop/build-evt-loop-init-fn (base/make-chan CHAN-FACTORY (base/rand-sid "evt-")))
                   ]
     ;;
     :ctx-fns     [
                   evt-loop/evt-loop-ctx-fn

                   ;; re-use common browser step handlers
                   common/common-ctx
                   wdom/dom-ctx

                   ]

     ;;
     :steps-fns   [(fn [params]
                     {
                      ::#EVT-LOOP# [:evt-loop (evt-loop/&evt-loop params)]
                      ; :CSS/custom-css-file [:css-file "http://localhost:9500/css/t.css"]
                      })
                   ]

     :opt-fns     [
                   (base/build-opts-chan-factory-fn CHAN-FACTORY)
                   ;; uncomment to display wf results (esp. if there was an error)
                   (base/build-opt-on-done (fn [params result]
                                             (.warn js/console params result)))
                   ]

     :ui-fn       (partial wf-ui/<wf-UI> (partial <scraping-root>))

     ;; dev stuff
     :playground/keys-to-update-on-reload [
                                           :actions
                                           :title
                                           :explanation
                                           :wf-actions

                                           ;; overwrite ui
                                           :ui-fn

                                           ;; update/overwrite workflow
                                           :init-fns :steps-fns :opt-fns
                                           ;; overwrite the state
                                           ;:state
                                           ]
     })
  )
