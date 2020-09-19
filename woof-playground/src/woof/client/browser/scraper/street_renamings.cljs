(ns woof.client.browser.scraper.street-renamings
  (:require

    [woof.base :as base]
    [woof.client.dom :as woof-dom :refer [q q* html! btn!]]
    [woof.client.dbg :as dbg :refer [__log]]
    [woof.data :as d]
    [woof.utils :as u]

    [goog.dom :as dom]
    [goog.dom.classes :as classes]
    ;; common wf

    [woof.utils :as u]

    [cljs.core.async :refer [go go-loop] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]
    [clojure.string :as str]))




(defn <ui>
  ([]

   ;; init UI
   (let [$panel (dom/createDom "div" "panel woof-scrape-panel")
         $pre  (dom/createDom "pre" "woof-scrape-pre" "")]
     (dom/appendChild $panel (dom/createDom "header" "" "SCRAPER STATE:"))
     (dom/appendChild $panel $pre)

     (woof-dom/ui-add-el! $panel)
     )
   )
  ([*state STATE]
   (let [$panel (q ".woof-scrape-panel")
         $pre (q ".woof-scrape-pre")
         ]

     (html! $pre (d/pretty! STATE))

     ;; (html! root "")
     )
   )
  )





;; a way to wrap some steps into reactive/infinitely updated ones
(defn reactify [steps upd-k upd-v]
  (reduce
    (fn [a [k [shandler v]]]
      (let [_vk (base/rand-sid)
            vk (base/rand-sid)
            ]
        (assoc a
          _vk [:v v]
          vk [:wait-rest [_vk upd-k]]
          k [shandler vk]
          )
        )

      )
    (array-map
      upd-k upd-v)
    steps
    )
  )


;;
;; parsing

(defn extract-renamed-lists [raw-district el]

  (let [r1 (woof-dom/query-selector* el "tr > td:nth-child(2)")
        r2 (woof-dom/query-selector* el "tr > td:nth-child(3)")

        els (partition 2 (interleave r1 r2))

        district (str/replace (str/capitalize raw-district) #"район" "р-н")
        ]

    {district
     (reduce (fn [a [old nu]]
               (let [old-text (.-innerText old)
                     nu-text (.-innerText nu)
                     ]
                 (if (not= ["Стара назва" "Нова назва"] [old-text nu-text])
                   (assoc a (.-innerText old)
                            (.-innerText nu))
                   a
                   )
                 )
               ) {} els)
     }
    )
  )



(defn _parse-renaming-table [i el]
  (.log js/console "i=" i el)
  (.scrollIntoView el true)

  (classes/add el (str "marker-" i))

  (if-let [first-row-el (woof-dom/query-selector el "tr:nth-child(1) > td strong ")]
    (if-let [text (.-innerText first-row-el)]
      (if (re-find #"РАЙОН" text)
        (do
          (extract-renamed-lists text el)

          ; (str i "\t" text)
          )
        )
      )
    )
  )


;;
;;

(defn _ordered-process-init [worker-chan-id params]
  (let [chan-factory (base/&chan-factory params)
        in-chan (base/make-chan chan-factory (base/rand-sid))]

    ;; process handler one by one

    (go-loop []
             (when-let [[handler out-chan] (async/<! in-chan)]
               (.log js/console "got handler" (meta handler))
               (let [ready-chan (handler)
                     val (async/<! ready-chan)]


                 (.log js/console "got val. " (meta handler) val)
                 ;; (async/<! (u/timeout 5000))

                 (async/put! out-chan val)
                 )
               (recur)))

    {worker-chan-id in-chan}))



(defn _linerized-shandler [worker-chan-id shandler-fn params v]
  (let [
        make-chan (fn [] (base/make-chan (base/&chan-factory params) (base/rand-sid)))
        ;; we should give this one a name like for wathcer
        worker-chan (get params worker-chan-id)

        handler-fn (with-meta (fn []
                     (let [c (make-chan)]
                       (go
                         (if-let [r (shandler-fn v)]
                           (async/put! c r)))
                       c))
                              {:v v}

                              )
        ]

    (let [outbound-chan (make-chan)]
      (async/put! worker-chan [handler-fn outbound-chan])

      outbound-chan
      )
    )

  )

(defn clean-up-css[]
  ;; remove previosly added styles
  (woof-dom/remove-added-css)

  ;; remove classes for previously added elements
  (doseq [s* ["woof-el"
              "woof-start"
              "woof-tmp"
              "woof-err"
              "woof-processed"

              "marker-1"
              "marker-2"
              "marker-3"
              "marker-4"
              "marker-5"
              "marker-6"
              "marker-7"
              "marker-8"
              "marker-9"
              "marker-10"
              "marker-11"
              "marker-12"
              "marker-13"

              ]
          ]
    (doseq [el (q* (str "." s*))]
      (classes/remove el s*)
      )
    )

  )

(defn wf! [*wf-state meta-info]

  {
   :init    [
             (fn [params]
               (.clear js/console)
               (clean-up-css)
               ;; add ui for scraping
               (<ui>)
               {})

             (partial _ordered-process-init ::linearizer)
             ]

   :ctx     [(fn [params]
               {

                :ui   {:fn       (fn [state]
                                   (<ui> (atom {}) state))
                       :collect? true
                       }

                :blob {:fn (fn [edn]

                             (let [a (.createElement js/document "a")]
                               (.appendChild (.-body js/document) a)
                               (set! (.-style a) "display: none")

                               (let [s (pr-str edn)
                                     blob (js/Blob. (clj->js [s])
                                                    (js-obj "type" "octet/stream"))
                                     url (.createObjectURL (.-URL js/window) blob)]

                                 (set! (.-href a) url)
                                 (set! (.-download a) (str "zzz-" (u/now) ".edn"))

                                 (.click a)
                                 (.revokeObjectURL (.-URL js/window) url)
                                 ;; todo: remove a element
                                 )
                               )

                             )}
                }
               )

             (fn [params]
               (let [*i (atom 0)]
                 {

                  ;; normal process fn
                  :process-renamed-table       {
                                                :fn (fn [el]
                                                      (let [i (swap! *i inc)]
                                                        (.log js/console "i=" i el)
                                                        (_parse-renaming-table i el)
                                                        )
                                                      )

                                                }
                  ;; run each :process-renamed-table in random order
                  :process-renamed*            (base/expand-into :process-renamed-table)


                  ;; ordered process

                  :process-renamed-table-async {
                                                ;; pass the proper order, but execute in different
                                                :fn-1 (fn [[i el]]
                                                        (_parse-renaming-table i el)
                                                        )
                                                ;; start all processes in the correct order, but now waiting for a result
                                                :fn   (partial _linerized-shandler
                                                               ::linearizer
                                                               (fn [[i el]]
                                                                 (.log js/console "linear " el)

                                                                 (let [make-chan (fn [] (base/make-chan (base/&chan-factory params) (base/rand-sid)))

                                                                       c (make-chan)
                                                                       ]
                                                                   (go
                                                                     ;;(async/<! (u/timeout 5000))

                                                                     (if-let [result (_parse-renaming-table i el)]
                                                                       (do
                                                                         (async/put! c result)
                                                                         (.log js/console "done" el)
                                                                         )
                                                                       (do
                                                                         (.log js/console "returned none")
                                                                         )
                                                                       )

                                                                     )
                                                                   c

                                                                   )
                                                                 )

                                                               params)
                                                }

                  ;; keep the order, by passing an order directly to the step
                  :process-renamed-async*      {
                                                ;; here should be used ::linearizer worker

                                                :fn       (fn [els]
                                                            (reduce (fn [a e]
                                                                      (assoc a (base/rand-sid)
                                                                               [:process-renamed-table-async [(swap! *i inc) e]]))
                                                                    (array-map) els))
                                                :expands? true
                                                }

                  :process-renamed-async-1*    {
                                                ;; here should be used ::linearizer worker

                                                :fn       (fn [els]
                                                            (let [make-chan (fn [] (base/make-chan (base/&chan-factory params) (base/rand-sid)))
                                                                  ;; for now use hardcoded worker channel
                                                                  worker-chan (get params ::linearizer)]

                                                              (reduce (fn [a e]
                                                                        (let [outbound-chan (make-chan)
                                                                              p [(swap! *i inc) e]

                                                                              [i _] p

                                                                              handler-fn (with-meta (fn []
                                                                                                      (let [c (make-chan)]
                                                                                                        (go
                                                                                                          ;; todo: add a confirm channel
                                                                                                          (async/<! (u/timeout (* i 1000)))
                                                                                                          (if-let [r (apply _parse-renaming-table p)]
                                                                                                            (async/put! c r)
                                                                                                            (async/put! c :nil)
                                                                                                            ))
                                                                                                        c))
                                                                                                    {:v p}

                                                                                                    )
                                                                              ]
                                                                          (async/put! worker-chan [handler-fn outbound-chan])

                                                                          (assoc a (base/rand-sid)
                                                                                   [:v outbound-chan])
                                                                        ))
                                                                      (array-map) els)
                                                              )
                                                            )
                                                :expands? true
                                                }




                  :filter-nil?                 {
                                                :fn       (fn [vs]
                                                            (filter #(not= :nil %) vs))

                                                :collect? true
                                                }

                  :merge                       {
                                                :fn       (fn [vs]
                                                            (apply merge vs)
                                                            )
                                                :collect? true
                                                }
                  })
               )

             ]

   :steps   [

             (fn [params]
               {
                ::hello              [:log "Hello from street rename scraper"]

                ::tables             [:query-selector-all "table"]

                ;; tables are in order
                ::log-tables         [:log ::tables]

                ;; this process renamed will parse tables in random order
                ;;::processed-elements [:process-renamed* ::tables]


                ;;::processed-elements [:process-renamed-async* ::tables]
                ::processed-elements [:process-renamed-async-1* ::tables]


                ::_renamed-streets   [:filter-nil? ::processed-elements]
                ::renamed-streets    [:merge ::_renamed-streets]

                ::log                [:log ::renamed-streets]

                ;;::save-results [:blob ::renamed-streets]

                ::css-2              [:css-file "http://localhost:9500/css/t.css"]

                }
               )

             ]
   :opts    []


   :api     {
             "street renaming scraper" (fn []

                                         (.log js/console "street renaming scraper")
                                         )

             }

   :on-stop (fn [state]
              (__log "GENERIC: ON STOP")
              ;; (.log js/console state)


              ;; todo: does copying prev state makes sense here
              #_(when-let [*old-state (get-in state [:WF/params ::state])]
                  (.log js/console "copying prev state")
                  (reset! *state @*old-state)
                  )

              ;; can return channel
              )
   }

  )