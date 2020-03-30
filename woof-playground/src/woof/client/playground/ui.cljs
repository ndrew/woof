(ns ^:figwheel-hooks woof.client.playground.ui
  (:require
    [rum.core :as rum]

    [woof.playground.v1.utils :refer [dstr kstr vstr]]
    [woof.base :as base]
    [woof.data :as d]
    [woof.utils :as u]
    [woof.wf :as wf])
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


(defn sid-length [k-width]
  (if (not= -1 k-width)
    (str (.floor js/Math (- k-width
            (/ k-width 3)
            ))
            "em" )
    "auto"))

(defn calculate-k-width [ks]
  (+ 1 (reduce (fn [a k]
                 (let [len (count k)]
                      (if (> len a)
                        len
                        a))
                 ) 0 ks))
  )

;;


(rum/defcs <sid> < rum/static
                   (rum/local true ::short-keys?)
  [local ui-cfg k]

  (let [short? (and @(::short-keys? local)
                    (:short-keys? ui-cfg))
        short-sid (shorten-sid k)
        epsilon-cfg (get ui-cfg :epsilon [30 12 5])
        k-width (get ui-cfg :k-width -1)
        ]
    [:.sid
     {
      :on-click (fn [e]
                  ;; todo: on-click should select a top level sid in a tree
                  (swap! (::short-keys? local) not))
      :style    { :width (sid-length k-width) }
      }

     (str
       (if short?
         (epsilon-sid short-sid epsilon-cfg) ;; very short key
         short-sid ;;
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


;;
(rum/defcs <single-v> < rum/static
                        (rum/local true ::short-map?)
  [local ui-cfg v]

  (cond
    (u/channel? v)
    [:.val.channel "<channel>"]

    (map? v)
    (let [k-width (calculate-k-width (map str (keys v)))
          nu-ui-cfg (assoc ui-cfg :k-width k-width)]
      [:.val {:on-click (fn [e]
                          (swap! (::short-map? local) not)
                          )}


       (if @(::short-map? local)
         ;(pr-str v)
         (map (fn [[k v]]
                [:.kv-row
                 (<sid> nu-ui-cfg k)
                 [:.val (sval v)]
                 ]
                ) v)
         (d/pretty v))
       ]
      )

    (and (seq? v) (wf/sid-list? v))
    (let [epsilon-cfg (get ui-cfg :epsilon [20 5 5])
          k-width (calculate-k-width
                    (map (fn [sid]
                           (epsilon-sid (shorten-sid sid) epsilon-cfg)) v))
          ]
      [:.val {:style { :width (sid-length (inc k-width))}}

       [:.sid-list
        (map (fn [sid]
               (<sid> ui-cfg sid)
               ;[:span (epsilon-sid (shorten-sid sid) epsilon-cfg)]
               ) v)
        ]
       ]
      )

    (seq? v)
    (let [foo 1]
      [:.val
       [:.val-list
        (map (fn [v] (<single-v> ui-cfg v)) v)
        ]
       ]
      )

    :else
    [:.val
     (sval v)
     ]
    )

  )

(defn rotate [n s]
  (lazy-cat (drop n s)
            (take n s)))

(defn shift-1[s]
  (rotate 1 s))


;; ui for [:step ::other step]
(rum/defcs <k-v> < rum/static
                        (rum/local true ::as-diff?)
                        (rum/local [::sid-only ::value-only ::sid-and-value ]  ::modes)
  [local ui-cfg k v]

  (let [value (:value v)
        mode (first @(::modes local))
        ]

    [:.k-val
     {:on-click (fn [e]
                  (swap! (::modes local) shift-1))}
     (cond
       (= mode ::sid-only)
       (<sid> ui-cfg k)

       (= mode ::value-only)
       (<single-v> (assoc ui-cfg :k-width -1) (:parent-value v))

       (= mode ::sid-and-value)
       [:.k-val
        (<sid> ui-cfg k)
        (<single-v> (assoc ui-cfg :k-width -1) (:parent-value v))
        ]
       )

      [:span.separator "→"]

      (<single-v> ui-cfg value)

     #_(if @(::as-diff? local)
         (let [[only-in-v _ _] (clojure.data/diff value
                                                  parent-value)]
           [[:span.separator "+"]
            (<single-v> ui-cfg only-in-v)
            ]

           )
         [
          [:span.separator "→"]
          (<single-v> ui-cfg value)
          ]

         )

     ]
    )
  )



(rum/defc <parent-step> < rum/static
  [ui-cfg v]

  [:.sid-body (<k-v> ui-cfg (:parent-step v) v)]
  )


(rum/defcs <sid-value> < rum/static
                         (rum/local true ::expanded-kv?)
  [local ui-cfg k v results]

  (let [expand? (and @(::expanded-kv? local)
                     (:expanded-kv? ui-cfg))
        value (get results k)]

    (if (:parent-step v)
      (<parent-step> ui-cfg (assoc v
                                :value value
                                :parent-value (get results (:parent-step v))))

      ;; else
      [:.sid-body
       (<single-v> ui-cfg value)
       ])))


(rum/defc <result-row> < rum/static
  [ui-cfg metadata results KV]

  (let [k (:k KV)]
    [:div.wf-results-row
     {:class (if (:updated? KV) "wf-updated" "")}

     ;; todo: is modifier needed? maybe we need a menu?
     ;; [:div.wf-modifier (str (if-not (nil? OUT?) "OUT" "___") "|" (:modifier v))]
     (<sid> ui-cfg k)
     (<sid-value> ui-cfg k KV results )
     ]
    )
  )


(defn make-tree [ui-cfg initial-data results]
  ;; list items
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

                            ;; todo:
                            #_(if (:expanded-kv? ui-cfg)
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
                              )



                            (conj a {
                                     :k        k
                                     :step     [step-id v]

                                     :parent-step   (if (wf/sid? v)
                                                 v
                                                 nil)


                                     :res      (get results k)
                                     :ctx      ctx
                                     :modifier (clojure.string/trim modifier)
                                     :expands? expands?

                                     :updated? ((get ui-cfg :updated-keys #{}) k)
                                     })
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
        ui-cfg @(::ui-cfg local)

        results-metadata (if-let [m (meta results)] m (base/default-meta-map))

        *prev @(::prev-results local)

        prev-results @*prev
        _ (reset! *prev results)

        [upd] (clojure.data/diff results prev-results)
        upd-keys (if (nil? upd) #{} (into #{} (keys upd)))

        ;; mi-toggler (partial kv-menu-item-toggler (::ui-cfg local) ui-cfg)
        ]
    (into [:div.wf-results
           (menubar heading [
                                [(if show? "↑" "↓") (fn [] (swap! (::show? local) not))]
                                ; todo: what togglers do we need?
                                ;(mi-toggler "short keys: " :short-keys?)
                                ;(mi-toggler "expanded keys: " :expanded-kv?)
                                ])
           ]
          (if show?
            (let [tree (make-tree (assoc ui-cfg :updated-keys upd-keys) initial-data results)

                  k-shorten-fn shorten-sid
                  k-width (calculate-k-width (map #(k-shorten-fn (get % :k)) tree))
                  nu-ui-cfg (assoc ui-cfg
                              :k-width k-width
                              :epsilon [20 5 5]
                              )
                  ]
              (map (partial <result-row>
                            nu-ui-cfg
                            results-metadata
                            results) tree)
              )
            []
            )
          )
    )

  )