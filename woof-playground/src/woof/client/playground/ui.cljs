(ns ^:figwheel-hooks woof.client.playground.ui
  (:require
    [rum.core :as rum]

    [woof.playground.v1.utils :refer [dstr kstr vstr]]
    [woof.base :as base]
    [woof.data :as d]
    [woof.utils :as u])
  )



;;
;; button
(rum/defc btn < rum/static [label f]
  [:button {:on-click (fn[e] (f))} label])


;;
;; menu

(rum/defc
  menu-item          <   { :key-fn (fn [label _] (str label))}
                         "menu item component"
  [label action-fn]

  [:a.menu-item
   {:href "#" :on-click (fn [e]
                          (action-fn)
                          (.preventDefault e)
                          false)}
   label])


(rum/defc menubar     <   rum/static     { :key-fn (fn [header items] (str header (count items)))}
                          "generic menubar component. has header and buttons"
  [menu-header menu-items]

  (into [:span.menubar
         (if-not (= "" menu-header)
           [:.header menu-header])
         ]
        (map (fn [[label action]]
               (if (and (nil? label) (nil? action))
                 [:.separator]
                 (menu-item label action)))
             menu-items)))


(defn shorten-bool [b]
  (if b "✓" "✕"))

(defn kv-menu-item-toggler [kv-ref kv header k ]
  [(str header (shorten-bool (get kv k)))
   (fn [] (swap! kv-ref update k not))]
  )



;;
;; select

;; immutable ds
(defn select-data-source
  ([items]
   (select-data-source items 0))
  ([items selected-idx]
   {
    :data items
    :ui   {
           :selected-idx selected-idx
           }
    :api  {
           ;; 'public' api
           :select-i (fn [*select-ds new-idx]
                       (swap! *select-ds assoc-in [:ui :selected-idx] new-idx))
           ;; todo: get value?
           }
    }
   )
  )


(rum/defc <select> < rum/reactive [*select-ds]
  (let [ds (rum/react *select-ds)
        data (get ds :data [])

        select-i! (partial (get-in ds [:api :select-i]) *select-ds)
        selected-idx (get-in ds [:ui :selected-idx])
        ]
    (into [:select
           {:on-change (fn [e]
                         (let [new-idx (.. e -target -options -selectedIndex )]
                           (select-i! new-idx)))
            ;; checked control
            :value (:v (get data selected-idx))
            }
           ]
          (map-indexed (fn [i a]
                         ;;
                         [:option {:value (:v a)} (:text a)]) data)
          )
    )
  )

;; generic wf ui



(defonce status-classes-map {
                             :not-started ""
                             :done        "done"
                             :running     "pending"
                             :stopped     "error"
                             :error       "error"
                             })

(defonce status-caption-map {
                             :not-started "…"
                             :done        "done!"
                             :running     "running"
                             :stopped     "stopped!"
                             :error       "error!"
                             })


(rum/defc <tag>  < rum/static
  [class text]

  [:span.tag
   {:class class}  text])


(rum/defc <wf-status-ui>  < rum/static
  [status]

  (<tag> (get status-classes-map status "")
         (get status-caption-map status "")))


(rum/defc <wf-menu-ui> < rum/reactive
  [header status all-actions]

   (let [actions (get all-actions status [])]
     [:div.main-menu
      (menubar header actions)
      (<wf-status-ui> status)
      ]
     )
   )


;;

(rum/defcs <debug> < rum/reactive (rum/local true ::show?)

  [{show? ::show?} data]
  (let [h (fn [] (swap! show? not))]
    (if @show?
      [:pre.debug
       (dstr (into (sorted-map) data))
       (btn "..." h)
       ]
      (btn "..." h)
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn shorten-sid [sid & {:keys [regex] :or {regex #"woof.client.playground.wf."}}]
  (clojure.string/replace (pr-str sid) regex "")
  )

(defn epsilon-sid [short-sid [shorten-threshold shorten-before shorten-after]]
  (if (< (count short-sid) shorten-threshold)
    ;; return short sid as is
    short-sid
    ;;
    (str
      (subs short-sid 0 shorten-before)
      "..."
      (subs short-sid (- (count short-sid) shorten-after))

      )

    )
  )



(rum/defcs <sid> < rum/static
                   (rum/local true ::short-keys?)
  [local ui-cfg k]

  (let [short? (and @(::short-keys? local)
                    (:short-keys? ui-cfg))
        short-sid (shorten-sid k)
        epsilon-cfg (get ui-cfg :epsilon [30 12 5])
        ]
    [:.sid {:on-click (fn [e] (swap! (::short-keys? local) not))}

     (str
       ;@(::short-keys? local) " " (:short-keys? ui-cfg)

       (if short?
         ;; very short key
         (epsilon-sid short-sid epsilon-cfg)
         ;;
         short-sid
         )
       )
     ]
    )
  )


(defn sval [v]
  (try
    (if (u/channel? v)
      "<channel>"
      (d/pretty v)
      )
    (catch js/Error e
      "ERROR"
      )
    )
  )


(rum/defcs <value> < rum/static
                     (rum/local true ::short-keys?)
  [local ui-cfg v]

  (if (u/channel? v)
    [:.val.channel
     "<channel>"
     ]
    [:.val
     (sval v)
     ]
    )



  )

(rum/defcs <sid-value> < rum/static
                         (rum/local true ::expanded-kv?)
  [local ui-cfg k v results]

  (let [expand? (and @(::expanded-kv? local)
                     (:expanded-kv? ui-cfg))
        value (get results k)]
    (into
      [:.wf-body]

      (if (:expands? v)
        [(menubar "" [[ (if expand? "as sid-list" "as values")
                          (fn [] (swap! (::expanded-kv? local) not))]])
         (if expand?

            (if (seq? (:res v))
              [(into [:.val-list] (map #(<value> ui-cfg %) (:res v)))]
              [(pr-str (:res v))]
              )



           (let [sid-list-ui-cfg (assoc ui-cfg :epsilon [20 5 5])]
             (into [:.sid-list] (map #(<sid> sid-list-ui-cfg %) value)))
           )

         ]
        [(<value> ui-cfg value)]
        )

      )

    ))




(rum/defc <result-row> < rum/static
  [ui-cfg metadata results KV]

  (let [k (:k KV)
        v KV
        OUT? (get-in metadata [:OUT k])]
    [:div.wf-results-row
     {:class (if (:updated? v) "wf-updated" "")}

     ;; modifier

     [:div.wf-modifier
      (str

        (if-not (nil? OUT?) "OUT" "___")
        "|" (:modifier v))

      ]

     (<sid> ui-cfg k)

     ;; todo: sort by date

     (<sid-value> ui-cfg k v results )

     #_[:.wf-body
        (if (:expands? v)
          (if (:expanded-kv? ui-cfg)
            (d/pretty (map (fn[a] (get results a)) (:res v)))

            (let [sid-list-ui-cfg (assoc ui-cfg :epsilon [20 5 5])]
              (into [:.sid-list
                     (ui/menubar "" [["yo" (fn[]
                                             )]])
                     ]
                    (map #(<sid> sid-list-ui-cfg %) value))
              )


            ;(d/pretty (d/pretty value))
            )
          ;; non-expand step
          (d/pretty value)
          )
        #_(if (:expands? v)
            (into
              [:.v]
              (map (fn[a] [:div.kv {:style {:margin-left "1rem"}}
                           [:.k (short-key a)]
                           [:.v (d/pretty (get results a))]
                           ] ) (:res v))
              )
            [:.v
             (str
               (short-value (:res v))
               )
             ]
            )
        ]
     ]
    )
  )


(defn make-tree [ui-cfg initial-data results]
  ;; make tree a lists
  (let [{
         ctx-map :context-map
         initial-steps :steps
         } initial-data

        ;; todo: is it possible to have items sorted by time?
        tree (reduce (fn [a [k [step-id v]]]
                       (let [ctx (get ctx-map step-id)
                             expands? (get ctx :expands?)
                             modifier (clojure.string/join " "
                                                           [(if (get ctx :infinite) "i" "")
                                                            (if (get ctx :collect) "c" "")
                                                            (if (get ctx :expands?) "e" "")])
                             ]
                            (if (:expanded-kv? ui-cfg)
                              ;; todo: expand
                              (let [sid-list (get results k)
                                    root-node {
                                               :k k
                                               :step [step-id v]
                                               :res sid-list
                                               :ctx ctx
                                               :modifier (clojure.string/trim modifier)
                                               :expands? expands?

                                               :updated? ((get ui-cfg :updated-keys #{}) k)
                                               }
                                    ]
                                (concat a [root-node]
                                        #_(map (fn [sid]
                                               {
                                                :k sid
                                                :res (get results sid)

                                                :step [step-id v]
                                                :ctx ctx
                                                :modifier (clojure.string/trim modifier)
                                                :expands? false
                                                }
                                               )
                                             sid-list )
                                        )
                                )

                              (conj a  {
                                        :k k
                                        :step [step-id v]
                                        :res (get results k)
                                        :ctx ctx
                                        :modifier (clojure.string/trim modifier)
                                        :expands? expands?

                                        :updated? ((get ui-cfg :updated-keys #{}) k)
                                        })
                              )
                            )
                       ) [] initial-steps)]
    tree

    )
  )




(rum/defcs <results-ui> < rum/static
                          (rum/local true ::show?)
                          (rum/local {
                                      :short-keys? true
                                      :expanded-kv? true
                                      } ::ui-cfg)
                          (rum/local (atom {}) ::prev-results)

  [local

   heading
   initial-data
   results
   ]

  (let [show?  @(::show? local)
        *ui-cfg (::ui-cfg local)
        ui-cfg @(::ui-cfg local)

        mi-toggler (partial kv-menu-item-toggler *ui-cfg ui-cfg)

        results-metadata (if-let [m (meta results)] m (base/default-meta-map))

        *prev @(::prev-results local)

        prev-results @*prev
        _ (reset! *prev results)

        [upd] (clojure.data/diff results prev-results)
        upd-keys (if (nil? upd) #{} (into #{} (keys upd)))
        ]
    (into [:div.wf-results

           (menubar heading [
                                [(if show? "↑" "↓") (fn [] (swap! (::show? local) not))]
                                (mi-toggler "short keys: " :short-keys?)
                                (mi-toggler "expanded keys: " :expanded-kv?)
                                ])
           ]
          (if show?
            (let [tree (make-tree (assoc ui-cfg :updated-keys upd-keys) initial-data results)]
              (concat
                [
                 ; [:pre (d/pretty tree) ]

                 #_[:pre
                  "expanded steps:\n"
                  (d/pretty
                    (clojure.set/difference
                      (into #{} (keys results))
                      (into #{} (keys tree))
                      )
                    )

                  ]
                 ]

                (map (partial <result-row>
                              ui-cfg
                              results-metadata
                              results) tree)
                )
              )
            []
            )
          )
    )

  )