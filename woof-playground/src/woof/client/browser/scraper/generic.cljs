(ns woof.client.browser.scraper.generic
  (:require

    [woof.base :as base]
    [woof.client.dom :as woof-dom :refer [q q* html! btn!]]
    [woof.client.dbg :as dbg :refer [__log]]
    [woof.data :as d]
    [woof.utils :as u]

    [goog.dom :as dom]
    [goog.dom.classes :as classes]

    [goog.dom.dataset :as dataset]
    ;; common wf

    [woof.wfs.evt-loop :as evt-loop]
    [woof.wfs.watcher :as watcher]

    [woof.client.ws :as ws]
    [woof.utils :as u]
    [woof.client.browser.scraper.session :as ss]
    [woof.client.browser.scraper.scraping-ui :as sui]

    [cljs.core.async :refer [go] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]
    [clojure.string :as str]))

;; GENERIC SCRAPING WORKFLOW
;;
;; WF that is run if no other scraping workflow is configured to run
;; Idea is to have a dev-tools like tooling for
;; * extracting html for furher analyisis
;; * building el-plan for element/s for certain selector
;; * etc

(defn selector-el [el]
  (let [parent (.-parentElement el)]
    {
     :t  (.-tagName el)
     :i (inc (.indexOf (array-seq (.-children parent)) el))
     :child-count  (-> el .-children .-length)
     }
    )

  )

(defn- traverse-up [el pred additional-map]

  (let [
        ;; (> (.-childElementCount parent) 10)

        _ (do
            (.log js/console "FS: starting traversal from" el))

        parent-nfo (loop [el el
                          selector (conj '() (selector-el el))]

                     (let [parent (.-parentElement el)]
                       (.log js/console "FS: candiadate" parent)

                       (if-not (pred el)
                         (do
                           (prn "recur")
                           ;;
                           (recur parent
                                  (conj selector (selector-el parent))
                                  ))
                         (do
                           (prn "end")
                           {
                            :el          el
                            :parent      parent
                            :$           selector
                            }
                           )
                         )
                       )
                     )
        ]

    (merge additional-map
           parent-nfo
           )


    )

  )


(defn <selector> [finder]
  (let [$ (:$ finder)
        root (dom/createDom "span" "woof-selector-ui" "")
        ]

    ;; parent selector

    (let [part (dom/createDom "span" "woof-selector-part" "")
          body (dom/createDom "span" "woof-selector-v" ".woof-range-start")
          ]
      (dom/appendChild part body)

      (btn! "marker 1" (fn []
                         (set! (.-innerText body) ".marker-1")


                         ) body)
      (dom/appendChild root part)
      )


    (doseq [z $]
      (let [part (dom/createDom "span" "woof-selector-part" "")
            use-nth? (>= (:i z) 1)
            tag (:t z)
            *v (volatile! false)
            selector-v  (str
                          tag
                          (if use-nth?
                            (str ":nth-child(" (:i z) ")")
                            "")
                          )
            selector (dom/createDom "span" "woof-selector-v" selector-v)
            ]

        ;; (:t %)
        (dom/appendChild part selector)
        (if use-nth?
          (btn! "n" (fn []
                      (vswap! *v not)
                      (set! (.-innerText selector)
                            (str tag ":nth-child(" (if @*v "n"  (:i z)) ")")
                            )
                      ) part)
          )


        (dom/appendChild root part)
        )

      )

    root
    )

  )


(defn finder! [*state finder-fn]

  (woof-dom/remove-class* (.-body js/document) "woof-range-start")
  (woof-dom/remove-class* (.-body js/document) "woof-range-end")


  (let [finder (finder-fn *state)]
    (swap! *state assoc :finder finder)


    (when-let [el* (:el* finder)]
      (classes/add (first el*) "woof-range-start")
      (classes/add (last el*) "woof-range-end")
      )

    ;; todo: return these
    ;;  (classes/add (:parent finder) "woof-range-start")
    ;; (classes/add (:el finder) "woof-range-end")
    )
  )


(defn <ui>
  ([]
   (let [$panel (dom/createDom "div" "panel woof-scrape-panel")
         $pre  (dom/createDom "pre" "woof-scrape-pre" "")]
     (dom/appendChild $panel (dom/createDom "header" "" "SCRAPER"))
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



(defn <ui-1>
  ([] ;; first initailization
   (let [panel (dom/createDom "div" "panel woof-scrape-panel")

         pre (dom/createDom "pre" "woof-scrape-pre" "")
         selection-actions (dom/createDom "div" "panel woof-scrape-selected" "")
         ]
     (dom/appendChild panel (dom/createDom "header" "" "SCRAPER"))

     (dom/appendChild panel pre)
     (dom/appendChild panel selection-actions)

     (woof-dom/ui-add-el! panel)
     )
   )
  ;; pass state atom for updates, but use state instead
  ([*state state]

  (.log js/console "upd.." (u/now))
   (let [panel (q ".woof-scrape-panel")
        el (q ".woof-scrape-pre")
        root (q ".woof-scrape-selected")]

    (html! root "")

    #_(when-let [selected (:selected state)]
        (btn! "mark" (fn [] (classes/add selected "woof-el")) root)
        (btn! "mark custom"
              (fn []
                (let [class (js/prompt "set class" "woof-el")]
                  (classes/add selected class))
                ) root)
      )

    (when-let [finder (:finder state)]
      (let [panel (dom/createDom "div" "panel" "")

            pre (dom/createDom "pre" ""
                               (str "/n" (d/pretty! finder)))

            span (<selector> finder)
            ]


        (dom/appendChild panel span)

        (btn! "start: UP" (fn []
                             #_(finder! *state (fn [*state]

                                                    (let [st @*state
                                                          initial-level (get-in st [:level])
                                                          initial-selector (get-in st [:finder :$])

                                                          el (get-in st [:finder :parent])

                                                          *inner-i (volatile! 1)
                                                          pred (fn [el]
                                                                 (vswap! *inner-i inc)

                                                                 (>= @*inner-i initial-level)
                                                                 )

                                                          parent (.-parentElement el)
                                                          ]

                                                      {
                                                       :el          (get-in st [:finder :el])
                                                       :parent      parent
                                                       :$           (conj initial-selector (selector-el parent))
                                                       :level (inc initial-level)
                                                       }

                                                      #_(loop [el el
                                                             selector (conj '() (selector-el el))]

                                                        (let [parent (.-parentElement el)]
                                                          (if-not (pred el)
                                                            (do
                                                              (recur parent
                                                                     (conj selector (selector-el parent))
                                                                     ))
                                                            (do
                                                              {
                                                               :el          el
                                                               :parent      parent
                                                               :$           selector
                                                               :zzz @*inner-i
                                                               :level (+ initial-level @*inner-i)
                                                               }
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )


                                                    ))

                             ) panel)

        (btn! "end: UP" (fn []
                           #_(finder! *state (fn [*state]
                                                  (let [st (swap! *state update-in [:level] inc)
                                                        level (:level st)
                                                        top-i level
                                                        *inner-i (volatile! 1)
                                                        ;; browse until predicate returns true
                                                        pred (fn [el]
                                                               (vswap! *inner-i inc)

                                                               (>= @*inner-i top-i)
                                                               )

                                                        el (get-in st [:finder :el])
                                                        parent (.-parentElement el)
                                                        ]

                                                    (traverse-up parent
                                                                 pred
                                                                 {:level (:level st)})
                                                    )))

                           ) panel)


        (btn! "mark!" (fn []

                        (doseq [el (q* ".woof-tmp")]
                          (classes/remove el "woof-tmp"))

                        (let [selector (str/join " > " (map
                                                         (fn [el]
                                                           (.-innerText el))
                                                         (q* span ".woof-selector-v")))]

                          (.log js/console selector)

                          (doseq [el (q* selector)]
                            (classes/add el "woof-tmp")
                            )

                          )
                        ;(classes/add (:parent finder) "woof-range-start")

                        ) panel)

        ;(btn! "mark start" (fn [] (classes/add (:parent finder) "woof-range-start")) panel)
        ;(btn! "mark end" (fn [] (classes/add (:el finder) "woof-range-end")) panel)

        (dom/appendChild panel pre)

        (btn! "finder" (fn []
                           (.log js/console finder)
                           ) panel)


        (dom/appendChild root panel)
        )

      )

    (btn! "state" (fn []
                    (.log js/console state)
                    (.log js/console (d/pretty! state))
                    ) root)


    )
  ; woof-scrape-pre
  (u/now)
  ))


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

;; example

#_(fn [params]
    ;; normal steps

    ::css-1 [:css-rule ".woof-scrape-panel { height: 150px; width: 100%; }"]
    ::css-2 [:css-file "http://localhost:9500/css/t.css"]

    ;; extract the shandler ids to a separate step [:v ..]
    ;; ::css-1 [:css-rule ::v1]
    ;;  ::_v1 [:v ".woof-scrape-panel { height: 150px; width: 100%; }"]
    ;; wrap v to a wait-rest pair
    ;; ::v1 [:wait-rest [::_v1 ::UPD-SOURCE]]
    )



(defn from-selection! [*state ]
  (let [st @*state
        el (woof-dom/sel-text-el)]
    {
     :el* [el]
     }
    )
  )


(defn move-up! [*state ]
  (let [st @*state
          el* (get-in st [:finder :el*])
          start-el (first el*)]

    {
     :el* (vec (concat [(.-parentElement start-el)]
                       (drop-last el*)))
     }
    )
  )

(defn start-up! [*state ]
  (let [st @*state
        el* (get-in st [:finder :el*])
        start-el (first el*)
        ]
    {
     :el* (vec (concat [(.-parentElement start-el)] el*))
     }
    )
  )

(defn end-up! [*state ]
  (let [st @*state
        el* (get-in st [:finder :el*])
        ;;last-el (last el*)
        ]
    {
     :el* (vec (drop-last el*))
     }
    )
  )



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


(defn wf! [*wf-state meta-info]

  (let [WATCHER-ID :state
        *state (atom {
                      ;; :level 1

                      })

        ;; start with 1 level
        ;; *i (volatile! 1)

        style-upd-chan (async/chan)

        clean-up-css (fn []
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

                       ;; send signal to re-load all css
                       (async/put! style-upd-chan :update)
                       )

        ;; finding common parent
        ]
    {
     :init    [
               (fn [params]
                 (clean-up-css)
                 {
                  ::state *state
                  })

               (fn [params]
                 ;; add ui for scraping
                 (<ui>)
                 {}
                 )

               ;; state watcher for react like UI updates
               (partial watcher/_watcher-cf-init WATCHER-ID *state)
               ]

     :ctx     [watcher/watcher-ctx

               (fn [params]
                 {

                  :rnd-scroll {:fn (fn [_]
                                     (rand-nth [1 2 3]))}

                  :ui         {:fn       (fn [state]
                                           (<ui> *state state))

                               :collect? true
                               }

                  :blob       {:fn (fn [edn]

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
                    :process-renamed*      (base/expand-into :process-renamed-table)

                    :process-renamed-table {
                                            :fn (fn [el]
                                                  (let [i (swap! *i inc)]
                                                    ;; (.log js/console "i=" i)
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
                                                  )
                                            }
                    :filter-nil? {
                                  :fn (fn [vs]
                                        (filter #(not= :nil %) vs))

                                  :collect? true
                                  }

                    :merge {
                            :fn (fn [vs]
                                  (apply merge vs)
                                  )
                            :collect? true
                            }
                    })
                 )

               ]

     :steps   [

               #_(fn [params]
                   {

                    ::css-1              [:css-rule ".woof-processed { display: none; }"]

                    ::selector           [:v "P"]
                    ;; get the elements to be parsed
                    ::els                [:query-selector-all ::selector]

                    ;; process the elements similar to pmap
                    ::processed-elements [:process* ::els]

                    ::collect            [:collect ::processed-elements]

                    ;; ::save-results [:blob ::collect]

                    ::log                [:log ::collect]

                    }
                   )


               ;;
               (fn [params]
                 (let [STEPS {

                              ;; ::css-1 [:css-rule ".woof-scrape-panel { height: 150px; width: 100%; }"]

                              ::css-2 [:css-file "http://localhost:9500/css/t.css"]
                              }

                       upd-styles? true]
                   (if upd-styles?
                     (reactify STEPS :CSS/upd [:v-8 style-upd-chan])
                     STEPS
                     )
                   )
                 )

               (fn [params]
                 {

                  :UI/state  [:watch WATCHER-ID]
                  :UI/render [:ui :UI/state]

                  ::hello    [:prn (u/now)]

                  ;; recurring parse
                  }
                 )]
     :opts    [
               watcher/watcher-opts
               ]

     :api     {}
     #_(array-map
         "refresh state" (fn []
                           (swap! *state assoc :t (u/now))
                           )

         "refresh CSS" (fn []
                         (clean-up-css)

                         (swap! *state assoc :level 1)

                         (woof-dom/remove-class* (.-body js/document) "woof-range-start")
                         (woof-dom/remove-class* (.-body js/document) "woof-range-end")

                         (woof-dom/remove-class* (.-body js/document) "woof-tmp")
                         )


         ;; todo: handle selection through several elements
         "select!" (fn [] (finder! *state from-selection!))
         "start: up" (fn [] (finder! *state start-up!))
         "move: up" (fn [] (finder! *state move-up!))
         "end: up" (fn [] (finder! *state end-up!))



         "marker 1" (fn []
                      (woof-dom/remove-class* (.-body js/document) "marker-1")
                      (let [el (woof-dom/sel-text-el)]
                        (classes/add el "marker-1")))

         "marker 2" (fn [] (woof-dom/remove-class* (.-body js/document) "marker-2")
                      (let [el (woof-dom/sel-text-el)]
                        (classes/add el "marker-2")))

         "\uD83D\uDCCB between markers" (fn []

                                          (let [range (q* ".marker-1 ~ *:not(.marker-2)")
                                                clipboard js/navigator.clipboard

                                                html (reduce (fn [s el]

                                                               (str s (.-outerHTML el) "\n")
                                                               ) "" range)

                                                ]

                                            (-> (.writeText clipboard html)
                                                (.then (fn [response] (.log js/console "Copied to clipboard - " response))
                                                       (fn [err] (.warn js/console "Failed to copy to clipboard" err))))
                                            )
                                          ;;
                                          )


         ;; manually marking start and end of the scraping range

         "scroll" (fn []
                    (let [params (get @*wf-state :WF/params {})
                          evt-loop (evt-loop/&evt-loop params)]
                      (async/put! evt-loop {
                                            (base/rand-sid) [:scroll 1]
                                            })
                      )
                    )
         )




     :on-stop (fn [state]
                (__log "GENERIC: ON STOP")
                ;; (.log js/console state)
                ;; can return channel
                )
     }
    )
  )