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

    [clj-fuzzy.metrics :as metrics]

    [woof.utils :as utils]

    [woof.browser :as woof-browser]

    [woof.client.dom :as wdom]
    [woof.client.playground.ui :as pg-ui]
    [woof.client.playground.ui.wf :as wf-ui]
    [woof.client.ws :as ws]
    [woof.client.playground.ui.html :as html-ui]

    [woof.wfs.evt-loop :as evt-loop]
    [woof.client.playground.scraper.tw :as dashboard]
    [woof.utils :as u])

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))




;;
;; UI
(rum/defcs <scraping-root> < rum/reactive
                             (rum/local {} ::inline-results?)
  [local *wf]

  (let [wf @*wf]
    [:div
     (if (= :not-started (:status wf))
       [:div "wf is not running"]

       ;; for now hadcode actions for ::current

       (let [*dict (rum/cursor-in *wf [:state ::dict])]
         [:div {:style {:padding "1rem"}}

          (pg-ui/menubar "KV"
                         (into
                           [
                            ["toggle html" (fn []
                                             (classes/toggle (wdom/q "#test-html-container") "visible")

                                             )]

                            ["load keys" (fn []
                                              (ws/GET "http://localhost:8081/kv/list" (fn [ks]
                                                                                        (swap! *dict assoc-in [:kv-ks] (d/to-primitive ks) )
                                                                                        ;; (.log js/console ks)
                                                                                        )
                                                       )
                                              )]
                               []]
                           (map (fn [x]
                                  [(pr-str x) (fn []
                                                (let [t0 (u/now)]
                                                  (ws/GET (str "http://localhost:8081/kv/get/" x)
                                                          (fn [_data]
                                                            (let [t1 (u/now)
                                                                  data (d/to-primitive _data)]
                                                              ;; (.log js/console data)

                                                              (wdom/html!
                                                                (wdom/q "#test-html")
                                                                data)

                                                              (let [now (u/now)]
                                                                (.log js/console

                                                                      "total: " (- now t0)
                                                                      "parse: " (- now t1)
                                                                      "request: " (- t1 t0))

                                                                ;; TODO: for now, hardcode the selector
                                                                (swap! *dict assoc-in [:curr-selector] "#test-html > .catalog-item" )
                                                                )

                                                              ;;
                                                              )
                                                            )
                                                          )
                                                  )

                                                )]
                                  ) (get-in @*dict [:kv-ks] []))
                           ))
          ;; (pr-str @*dict)

          (let [$listings "#test-html > .catalog-item"
                skip-fn (fn [$el $]
                          (#{; "SVG"
                             "G" "PATH"} (str/upper-case (.-tagName $el))))

                els (wdom/q* $listings)]

            (if els
              (let [make-el-map (fn [el]
                                  (wdom/el-map el
                                               :skip-fn skip-fn
                                               :node-fn wdom/enrich-node
                                               :top-selector-fn (fn [base el] { :nth-child (:i base)})))

                    el-map->nodes (fn [el-map-1]
                                    (reduce (fn [a node] (assoc a (:_$ node) node)) {} el-map-1))

                    id-1     (rand-int (count els))
                    el-map-1 (make-el-map (nth els id-1))
                    el-1     (el-map->nodes el-map-1)


                    id-2     (rand-int (count els))
                    el-map-2 (make-el-map (nth els id-2))
                    el-2     (el-map->nodes el-map-2)

                    ]

                [:div
                 [:header "ELS:"]

                 (html-ui/<dashboard>
                   [
                    {:id id-1

                     :el-map el-map-1
                     :nodes el-1
                     }
                    {
                     :id id-2

                     :el-map el-map-2
                     :nodes el-2
                     }
                    ])
                 ]
                )
              )


            )

          ;;html-ui/<plan>

          ;; streets

          ; [:pre (pr-str @*dict)]

          #_(pg-ui/<edn-list> (get-in wf [:state ::dict ::renamings]  []) "")

          #_[:pre
           (d/pretty! (get-in wf [:state ::dict ::renamings]  []))
           ]





          #_(let [dom-nfos (get-in wf [:state ::dict ::els]  [])]
            (<extract-renaming>
              [(rand-nth dom-nfos) (rand-nth dom-nfos)]
              ;;dom-nfos
              )
            )


          ;;(<streets-cc> *dict)

          ;;(<cc> *dict *categorizator)

          ]
         )

       )
     ]
    )
  )





;;
;; WF definition
(defn wf! [*SWF]


  (let [CHAN-FACTORY (base/chan-factory (atom {}))
        *dict (rum/cursor-in *SWF [:state ::dict])
        *categorizator (rum/cursor-in *SWF [:state ::categorizator])
        ]
    {

     :title       "Scraping command center"
     :explanation [:div.explanation
                   [:p "Analyze scraped data here"]]

     ;; this state will be added to a wf?
     :state {

             ;; just data for the ui
             ::dict {
                     }

             }

     :init-fns    [

                   (fn [params]

                     #_(ws/GET "http://localhost:9500/s/yt/channel-tags-mapping.edn" (fn [data]
                                                                                     (reset! *categorizator data)))

                     ;; todo: maybe returning channel and proceeding with init when the data is ready?
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
                   woof-browser/common-ctx
                   wdom/dom-ctx

                   ]

     ;;
     :steps-fns   [(fn [params]
                     {
                      ::#EVT-LOOP# [:evt-loop (evt-loop/&evt-loop params)]

                      ;; ::hello [:log "Hello"]

                      :CSS/custom-css-file [:css-file "http://localhost:9500/css/t.css"]
                      })
                   ;; parse-renamings-steps
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
