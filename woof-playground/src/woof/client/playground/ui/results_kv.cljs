(ns ^:figwheel-hooks woof.client.playground.ui.results-kv
  (:require
    [rum.core :as rum]

    [clojure.data :as cd]
    [clojure.string :as str]

    [woof.wf :as wf]
    [woof.utils :as u])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))

;;;

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
  (if (seq? ks)
    (+ 1 (reduce #(max %1 (count %2)) 0 ks))
    -1))


(defn sid-length [k-width]
  (if (not= -1 k-width)
    (str (.floor js/Math (- k-width
                            (/ k-width 3)
                            ))
         "em" )
    "auto"))


(defn display-sid [ui-cfg k]
  (if (get ui-cfg :short-sids? true)
    (shorten-sid k)
    (str k)
    )
  )


(defn results->kvs [ui-cfg wf]
  (let [results (:result wf)
        {initial-params :params
         ctx-map :context-map
         initial-steps :steps
         } (get-in wf [:runtime :initial])
        ]


    (let [
          ;; calculate width for top level workflows
          k-width (calculate-k-width
                    (map (fn [[sid]]
                           (display-sid ui-cfg sid)) initial-steps))

          ;; find kvs to be displayed
          kvs (map (fn [[root-sid [step-handler v]]]
           (let [ctx (get ctx-map step-handler)
                 res (get results root-sid)

                 kv {
                     :k root-sid
                     :short-sid (display-sid ui-cfg root-sid)

                     :parent-k (if (wf/sid? v) v nil)

                     :k-width k-width

                     :step [step-handler v]
                     :v (get results root-sid)



                     }]


                ; (.log js/console root-sid (get ctx :expands?) (seq? res) (wf/sid-list? res))

                (if (and (get ctx :expands?) (seq? res) (wf/sid-list? res))
                  (assoc kv
                    :children (map (fn [expanded-sid]
                                     {
                                      :k expanded-sid
                                      :parent-k root-sid

                                      ;; for now
                                      :short-sid (epsilon-sid (shorten-sid expanded-sid) [20 5 5])

                                      :k-width -1

                                      :v (get results expanded-sid)
                                      }
                                     ) res)
                    )
                  kv
                  )
                )
           ) initial-steps)]



      ;; now try to do 2-step process
      (if (:nest-expanded? ui-cfg)
        (reduce (fn [a r]
                  (if (:children r)
                      (concat a [r] (:children r))
                      (conj a r))) [] kvs)
        kvs
        )
      )
    )
  )


;;;;
;;

(defn row-attrs-fn [cfg row]
  {:class (str
            ;(if (:updated? row) " wf-updated" "")
            ;(if (:expanded? row) " wf-expanded" "")
            ;; for now use initial
            (if (:step row)
              ""
              " wf-expanded"
              )
            )
   }
  )

(defn sid-attrs-fn [cfg row]
  (let []
    {
     :on-click (fn [e]

                 (.log js/console row)
                 ;; todo: on-click should select a top level sid in a tree
                 ;; (swap! (::short-keys? local) not)
                 )
     :style    {
                :width (sid-length (get row :k-width -1))
                }
     }
    )
  )

(defn sid-display-fn [cfg row]
  (if (:short-sids? cfg)
    (str (:short-sid row))
    (str (:k row))
    )
  )


(defn kv-ui-default-map []
  {
   :short-sids? true
   :nest-expanded? true

   :row-attrs-fn row-attrs-fn
   :k-attrs-fn   sid-attrs-fn
   :k-display-fn sid-display-fn

   }
  )



;; sid key control
(rum/defc <sid-k> < rum/static
                    {:key-fn (fn [_ row]
                               (if-let [parent-k (:parent-k row)]
                                       (str "k-" parent-k "." (:k row))
                                       (str "k-" (:k row)))
                                   )
                     }

  [cfg row]

  (let [_attrs-fn (get cfg :k-attrs-fn (identity {}))
        _k-fn (get cfg :k-display-fn (identity (pr-str (:k row))))
        ]
    [:.sid (_attrs-fn cfg row)

     (_k-fn cfg row)]
    )
  )



(rum/defcs <sid-list> < rum/static
                        {:key-fn (fn [_ row]
                                   (str "sid-list-" (:k row))
                                   )
                         }
                        (rum/local [::inline-short ::inline-full
                                    ;; ::vertical
                                    ] ::modes)
  [local ui-cfg row]
  (let [mode (first @(::modes local))
        swap-mode! (fn [e] (swap! (::modes local) u/shift-1))

        sid-list (:v row)

        ]

    [:.val
     (cond
       (= ::inline-short mode)
       [:.sid-list {:on-click swap-mode!
                    :class "inline"}

        ;; should we overwrite short-sids?



        (if (> (count sid-list) 3)
          [:button "..."]
          )

        (map (fn [k]
               [:.sid
                (epsilon-sid (shorten-sid k) [20 5 5])]
               ) (take-last 3 sid-list))

        ]

       (= ::inline-full mode)
       [:.sid-list {:on-click swap-mode!
                    :class "inline"}
        (map (fn[k]
               [:.sid
                (shorten-sid k)]) sid-list)]

       ;; todo: vertial mode
       #_(= ::vertical mode)
       #_(let [k-width (calculate-k-width (map shorten-sid sid-list))]
           [:.sid-list {:on-click swap-mode!
                        :style {:width (sid-length k-width)}}
            (map (partial <sid> (assoc ui-cfg :short-keys? false :k-width -1) ) sid-list)]
           )
       :else [:div "unknown mode " (pr-str mode)])
     ]
    )
  )






(rum/defc <sid-v> < rum/static
                    {:key-fn (fn [_ row] (str "v-" (:k row)))}
  [cfg row]

   (let [v (:v row)]
     (cond
       (u/channel? v) [:.val.channel "<channel>"]
       (nil? v) [:.val.channel " "]

       (and (seq? v) (wf/sid-list? v))
       (<sid-list> cfg row)

       :else [:.val (pr-str v)]
       )
     )

  )



;;; steps ui


(rum/defc <step-k> < rum/static
                     {:key-fn (fn [_ row] (str "step-k-" (:k row)))}
  [cfg row]

  [:.sid
   {:style {:width (sid-length 30)
            :color (if (get-in row [:ctx :expands?]) "red" "auto")
            }}



   ; (pr-str (keys row))
   (shorten-sid (:k row))]

  )


(rum/defc <step-v> < rum/static
                    {:key-fn (fn [_ row] (str "step-body-" (:k row)))}
  [cfg row]


  (let [[shandler v] (:v row)]
    [:.kv-row
     [:.sid
      {:style {:width (sid-length 15)}}
      (str shandler)
      ]


     (cond
       (u/channel? v) [:.val.channel "<channel>"]
       (nil? v) [:.val.channel " "]
       :else [:.val (str v)])
     ]

    )

  )





