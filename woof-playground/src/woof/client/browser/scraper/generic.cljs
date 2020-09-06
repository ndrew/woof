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



(defn selector-el [el]
  (let [parent (.-parentElement el)]
    {
     :t  (.-tagName el)
     :i (inc (.indexOf (array-seq (.-children parent)) el))
     :child-count  (-> el .-children .-length)
     }
    )

  )

(defn- traverse [pred additional-map]

  (let [el (woof-dom/sel-text-el)

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

    (let [part (dom/createDom "span" "woof-selector-part" "")]
      (dom/appendChild part (dom/createDom "span" "woof-selector-v"
                                           ".woof-range-start"))
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


(defn <ui>
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
  ([state]

  (.log js/console "upd.." (u/now))

  (let [panel (q ".woof-scrape-panel")
        el (q ".woof-scrape-pre")
        root (q ".woof-scrape-selected")
        ]

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

        (btn! "mark by selector" (fn []

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




(defn wf! [*wf-state meta-info]

  (let [WATCHER-ID :state
        *state (atom {
                      :level 1

                      })

        ;; start with 1 level
        ;; *i (volatile! 1)

        style-upd-chan (async/chan)

        clean-up-css (fn []
                       ;; remove previosly added styles
                       (woof-dom/remove-added-css)

                       ;; remove classes for previously added elements
                       (doseq [el (concat
                                    ;; todo: get these from state
                                    (q* ".woof-el")
                                    (q* ".woof-start")
                                    )
                               ]
                         (classes/remove el "woof-el"))

                       ;; send signal to re-load all css
                       (async/put! style-upd-chan :update)
                       )

        ;; finding common parent
        ]
    {
     :init    [
               (fn [params]
                 (clean-up-css)
                 {})

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

                  :ui         {:fn <ui>}
                  }
                 )
               ]

     :steps   [
               (fn [params]
                 (let [STEPS {
                              ::css-1 [:css-rule ".woof-scrape-panel { height: 150px; width: 100%; }"]
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

                  :UI/state          [:watch WATCHER-ID]
                  :UI/render         [:ui :UI/state]

                  ::hello            [:prn (u/now)]

                  ;; recurring parse
                  }
                 )]
     :otps    [
               watcher/watcher-opts
               ]

     :api     (array-map

                "refresh CSS" (fn []
                                (clean-up-css)

                                (swap! *state assoc :level 1)

                                (woof-dom/remove-class* (.-body js/document) "woof-range-start")
                                (woof-dom/remove-class* (.-body js/document) "woof-range-end")
                                )

                "select!" (fn []

                            (let [st (swap! *state update-in [:level] #(identity 1))
                                  finder (traverse identity {:level (:level st)})]
                              (swap! *state assoc :finder finder)

                              (classes/add (:parent finder) "woof-range-start")
                              (classes/add (:el finder) "woof-range-end")
                              )

                            )

                "-1 level" (fn []
                             ; (.clear js/console)

                             (woof-dom/remove-class* (.-body js/document) "woof-range-start")
                             (woof-dom/remove-class* (.-body js/document) "woof-range-end")

                             (let [st (swap! *state update-in [:level] inc)
                                   level (:level st)
                                   top-i level
                                   *inner-i (volatile! 0)
                                   ;; browse until predicate returns true
                                   pred (fn [el]
                                          (vswap! *inner-i inc)

                                          (>= @*inner-i top-i)
                                          )
                                   ]

                               (.log js/console "I- " top-i)

                               (let [finder (traverse pred {:level (:level st)})]
                                 (swap! *state assoc :finder finder)

                                 (classes/add (:parent finder) "woof-range-start")
                                 (classes/add (:el finder) "woof-range-end")
                                 )
                               )
                             )

                "+1 level" (fn []
                             (woof-dom/remove-class* (.-body js/document) "woof-tmp")
                             (let [
                                   ;; browse until predicate returns true
                                   pred (fn [el]
                                          (swap! *state update-in :level dec)
                                          true)
                                   ]
                               ;; todo:
                               (swap! *state assoc
                                      :finder (traverse pred {})
                                      )

                               )
                             )



                ;; manually marking start and end of the scraping range

                ;; "mark start" (fn [] (swap! *state assoc :start (woof-dom/sel-text-el)))
                ;; "mark end" (fn [] (swap! *state assoc :end (woof-dom/sel-text-el)))

                "common ancestor" (fn []
                                    (let [{start :start
                                           end   :end} @*state]

                                      ;; todo: finding common ancestor
                                      ;; - container that holds both elements

                                      (let [range (.createRange js/document)]

                                        (.selectNode range start)
                                        (.selectNode range end)

                                        ;; (js-debugger)

                                        (.log js/console range)
                                        )

                                      ;var range = document.createRange();
                                      ;var nodes = [document.head, document.body];  // or any other set of nodes
                                      ;nodes.forEach(range.selectNode, range);
                                      ;range.commonAncestorContainer;

                                      )
                                    )




                "selected el" (fn []
                                (let [el (woof-dom/sel-text-el)]

                                  (swap! *state assoc :selected el)
                                  )

                                )

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
                (__log "ON STOP")
                (.log js/console state)

                ;; can return channel
                )
     }
    )
  )