(ns ^:figwheel-hooks woof.client.playground.ui.wf
  (:require
    [rum.core :as rum]

    [clojure.data :as cd]
    [clojure.string :as str]

    [woof.base :as base]
    [woof.client.playground.ui :as ui]
    [woof.data :as d]
    [woof.wf :as wf]
    [woof.utils :as u])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



(defn shorten-sid [sid & {:keys [regex] :or {regex #"woof.client.playground.wf."}}]
  (str/replace (pr-str sid) regex ""))

(defn epsilon-sid [short-sid [shorten-threshold shorten-before shorten-after]]
  (if (< (count short-sid) shorten-threshold)
    short-sid ;; return short sid as is
    (str      ;; shorten it
      (subs short-sid 0 shorten-before)
      "..."
      (subs short-sid (- (count short-sid) shorten-after)))))

(defn calculate-k-width [ks]
  (try
    (if (seq? ks)
      (+ 1 (reduce (fn [a k]
                     (let [len (count k)]
                          (if (> len a)
                            len
                            a))
                     ) 0 ks))
      -1)

    (catch js/Error e
      (do
        (.warn js/console e)
        -1
        )
      )
    )


  )

(defn rotate [n s]
  (lazy-cat (drop n s)
            (take n s)))

(defn shift-1[s]
  (rotate 1 s))


;;;;

(defn result-row-from-step [ui-cfg ctx-map results step]
  (let [[k [step-id v]] step
        updated? ((get ui-cfg :updated-keys #{}) k)

        ctx (get ctx-map step-id)
        expands? (get ctx :expands?)
        modifier (clojure.string/join " "
                                      [(if (get ctx :infinite) "i" "")
                                       (if (get ctx :collect) "c" "")
                                       (if (get ctx :expands?) "e" "")])
        ]
    ;; should we provide all ctx, or just parts needed?
    {
     :k        k

     :step     [step-id v]
     :parent-step   (if (wf/sid? v) v nil)

     :res      (get results k)

     :ctx      ctx
     :expands? expands?
     :expanded? false

     :modifier "" ;(str/trim modifier)

     :updated? updated?


     }
  ))

(defn result-row-expanded [ui-cfg root sid]
  (let [sids (:res root)
        parent-step (:k root)]
    {
     :k           sid
     :res         sids

     ;:step [step-id v]
     ;:expands? false
     ;;:ctx (:ctx root)

     ;:modifier "" ;;;
     :parent-step parent-step

     ;; ::debug? true

     :updated?    ((get ui-cfg :updated-keys #{}) sid)
     :expanded?   true
     }
    )
  )

(defn make-tree [ui-cfg initial-data results]
  ;; list items
  (let [{
         ctx-map :context-map
         initial-steps :steps
         } initial-data
        ;; todo: is it possible to have items sorted by time?
        ]

    (if (:expanded-kv? ui-cfg)
      (reduce (fn [a step]
                (let [root (result-row-from-step ui-cfg ctx-map results step)
                      res (:res root)]

                     (if (and (get-in root [:ctx :expands?])
                              (seq? res)
                              (wf/sid-list? res))
                       (concat a [root]
                               (map (partial result-row-expanded ui-cfg root) res))
                       (conj a root)
                       )

                     )
                ) [] initial-steps)

      (reduce (fn [a step]
                (conj a (result-row-from-step ui-cfg ctx-map results step))) [] initial-steps)
      )

    )
  )


;;
(defn sid-length [k-width]
  (if (not= -1 k-width)
    (str (.floor js/Math (- k-width
                            (/ k-width 3)
                            ))
         "em" )
    "auto"))


(rum/defc <sid> < rum/static
  [ui-cfg k]

  (let [short? (:short-keys? ui-cfg)
        short-sid (shorten-sid k)
        epsilon-cfg (get ui-cfg :epsilon [30 12 5])
        k-width (get ui-cfg :k-width -1)
        ]
    [:.sid
     {
      :on-click (fn [e]
                  ;; todo: on-click should select a top level sid in a tree
                  ;; (swap! (::short-keys? local) not)
                  )
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
    (cond
      (u/channel? v) "<channel>"
      (nil? v) "<nil>"
      :else (d/pretty v)
      )
    (catch js/Error e
      (do
        (.error js/console e)
        "<ERROR>")
      )
    )
  )


(rum/defcs <v-sid-list> < rum/static
                          (rum/local [::inline-short ::inline-full ::vertical] ::modes)
  [local ui-cfg v]
  (let [mode (first @(::modes local))
        swap-mode! (fn [e] (swap! (::modes local) shift-1))]
    [:.val
     (cond
       (= ::inline-short mode)
       [:.sid-list {:on-click swap-mode!
                    :class "inline"}

        [:button "..."]
        (map (partial <sid> (assoc
                              ui-cfg
                              :short-keys? false
                              :k-width -1
                              )) (take-last 3 v))

        ]

       (= ::inline-full mode)
       [:.sid-list {:on-click swap-mode!
                    :class "inline"}
        (map (partial <sid> (assoc ui-cfg :short-keys? false :k-width -1)) v)]

       (= ::vertical mode)
       (let [k-width (calculate-k-width (map shorten-sid v))]
         [:.sid-list {:on-click swap-mode!
                      :style {:width (sid-length k-width)}}
          (map (partial <sid> (assoc ui-cfg :short-keys? false :k-width -1) ) v)]
         )
       :else [:div "unknown mode " (pr-str mode)])
     ]
    )
  )

(declare <single-v>)

(rum/defcs <v-val-list> < rum/static
                          (rum/local [::inline-short ::inline-full ::vertical] ::modes)
  [local ui-cfg v]
  (let [mode (first @(::modes local))
        swap-mode! (fn [e] (swap! (::modes local) shift-1))]
    [:.val
     ; (pr-str mode)
     (cond
       (= ::inline-short mode)
       [:.val-list {:on-click swap-mode!
                    :class "inline"}

        [:button "..."]
        (map (fn [v] (<single-v> ui-cfg v)) (take-last 3 v))
        ]

       (= ::inline-full mode)
       [:.val-list {:on-click swap-mode!
                    :class "inline"}
        (map (fn [v] (<single-v> ui-cfg v)) v)]

       (= ::vertical mode)
       [:.val-list {:on-click swap-mode!
                    :style {:width "200px"}
                    }
        (map (fn [v] (<single-v> ui-cfg v)) v)]

       :else [:div "unknown mode " (pr-str mode)])
     ]
    )
  )

;;
(rum/defcs <single-v> < rum/static
                        (rum/local true ::short-map?)
  [local ui-cfg v]

  (cond
    (u/channel? v)
    [:.val.channel "<channel>"]

    (nil? v)
    [:.val "<nil>"]

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
    (<v-sid-list> ui-cfg v)


    (seq? v)
    (<v-val-list> ui-cfg v)

    :else
    [:.val (sval v)
     ]
    )

  )


;; ui for [:step ::other step]
(rum/defcs <k-v> < rum/static
                   (rum/local true ::as-diff?)
                   (rum/local [::sid-only ::value-only ::sid-and-value ]  ::modes)
  [local ui-cfg k v]

  (let [value (:value v)
        mode (first @(::modes local))
        ]

    [:.k-val

     ;; todo: click here conflicts with click on <single-v>, so for now using button
     [:button
      {:on-click (fn [e] (swap! (::modes local) shift-1))}
      "*"]



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



(rum/defc <sid-value> < rum/static
  [ui-cfg v results]

  (let [value (get results (:k v))]


    (cond
      ;;(::debug? v)
      ;;[:.sid-body (d/pretty v)]

      (:parent-step v)
      (let [nu-v (assoc v
                   :value value
                   :parent-value (get results (:parent-step v)))]

        [:.sid-body (<k-v> ui-cfg (:parent-step nu-v) nu-v)])

      :else
      [:.sid-body
       (<single-v> ui-cfg value)
       ]
      )


    ))



(rum/defc <result-row> < rum/static
  [ui-cfg metadata results row]

  (let [k (:k row)]
    [:div.wf-results-row
     {:class
      (str
        (if (:updated? row) " wf-updated" "")
        (if (:expanded? row) " wf-expanded" "")
        )
      }
     ;; todo: is modifier needed? maybe we need a menu?
     ;; [:div.wf-modifier (str (if-not (nil? OUT?) "OUT" "___") "|" (:modifier v))]


     (<sid> ui-cfg k)

     (<sid-value> ui-cfg row results )
     ]
    )
  )


(defn shorten-bool [b]
  (if b "✓" "✕"))

(defn kv-menu-item-toggler [kv-ref kv header k ]
  [(str header (shorten-bool (get kv k)))
   (fn [] (swap! kv-ref update k not))]
  )


;;
(rum/defcs <results-ui> < rum/static
                          (rum/local true ::show?)
                          (rum/local {
                                      :short-keys? true
                                      :expanded-kv? false
                                      } ::ui-cfg)
                          (rum/local (atom {}) ::prev-results)

  [local   heading initial-data results]

  (let [show?  @(::show? local)
        *prev @(::prev-results local)
        prev-results @*prev
        _ (reset! *prev results)
        [upd] (cd/diff results prev-results)
        upd-keys (if (nil? upd) #{} (into #{} (keys upd)))

        ui-cfg @(::ui-cfg local)
        mi-toggler (partial kv-menu-item-toggler (::ui-cfg local) ui-cfg)]

    (if-not show?
      [:div.wf-results]

      (try
        (let [tree (make-tree (assoc ui-cfg :updated-keys upd-keys) initial-data results)

              epsilon-cfg [20 5 5]
              k-shorten-fn (if (:short-keys? ui-cfg)
                             (fn [sid] (epsilon-sid (shorten-sid sid) epsilon-cfg))
                             shorten-sid)

              k-width (calculate-k-width (map #(k-shorten-fn (get % :k)) tree))
              nu-ui-cfg (assoc ui-cfg
                          :k-width k-width
                          :epsilon epsilon-cfg
                          )

              results-metadata (if-let [m (meta results)] m (base/default-meta-map))]

          [:div.wf-results
           (ui/menubar heading [
                                [(if show? "↑" "↓") (fn [] (swap! (::show? local) not))]

                                (mi-toggler "short keys: " :short-keys?)
                                (mi-toggler "show expanded keys: " :expanded-kv?)

                                ;; todo: filter steps by sid?
                                ])


           (map (partial <result-row>
                         nu-ui-cfg
                         results-metadata
                         results) tree)

           ]
          )
        (catch js/Error e
          (.warn js/console e)
          )
        )


  )))





;;;;;;

;;
(rum/defcs <default-wf-details-ui> < rum/reactive
                                     (rum/local true ::inline-results?)
                                     (rum/local true ::sort-results?)
  [local wf]

  [:div.wf-details

   (if-let [results (:result wf)]
     (if (not= :not-started (:status wf))
       (<results-ui> "RESULTS"
                        (get-in wf [:runtime :initial])
                        results)))

   (if-let [initial-steps (get-in wf [:runtime :initial :steps])]
     (<results-ui> "INITIAL STEPS" ;; TODO: better UI for steps, for now use same ui as for results
                   (get-in wf [:runtime :initial])
                   initial-steps))

   (if-let [initial-params (get-in wf [:runtime :initial :params])]
     [:div.wf-results
      ;; menubar
      (ui/menubar "INITIAL PARAMS" [])
      (map (fn [[k v]]
        [:div.wf-results-row
         [:.sid
          (pr-str k)
          ]
         [:.val
          (pr-str v)
          ]
         ])
           initial-params
           )

      ])

   ]

  )


(defn- safe-pretty [v]
  (try
    [:pre (d/pretty v)]
    (catch js/Error e
      (do
        (.error js/console e)
        [:pre (pr-str (js->clj e))]))
    )
  )

(rum/defc <default-body> < rum/static
  [wf]
  (condp = (:status wf)
    :not-started [:div
                  "Hit run! to start a workflow"
                  ]
    :running (<default-wf-details-ui> wf)                      ; [:pre "..."]
    :done (<default-wf-details-ui> wf)                         ; (safe-pretty (:result wf))
    :error [:pre.wf-error "Error:\n" (safe-pretty (:result wf))]
    )
  )


;; default ui for wf runner
(rum/defc <default-wf-ui> < rum/reactive
  [<body-fn> *wf]
  (let [wf @*wf
        status (:status wf)
        title (:title wf)
        explanation (:explanation wf)
        ]

    [:div.default-wf-ui
     (ui/<wf-menu-ui> title status (:actions wf))

     (cond
         (string? explanation) [:pre explanation]
         (vector? explanation) explanation
         (fn? explanation) (explanation)
         )

     (try
       (<body-fn> wf)
       (catch js/Error e
         [:pre (pr-str e)] ;; todo: nicer error catching
         )
       )
     ]
    )
  )

