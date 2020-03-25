(ns ^:figwheel-hooks woof.playground.prototype6
  (:require
    [cljs.core.async :as async]
    [rum.core :as rum]


    ;; client core
    [woof.base :as base]
    [woof.wf :as wf]
    [woof.client.playground.ui :as ui]

    [woof.data :as d]
    [woof.utils :as utils]

    [woof.playground.common :as cmn]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;;

;; -------

;; ui workflow example
;; like UI component that can be extended with wf


;; the ui component will be a list box with a details view on the right
;;
;; list box data source
;;   ? infinite?
;;   ? expand?
;;

(defonce *UI-STATE (atom
                     {
                      ;; wf state atom per each key
                      :ui-wf     {

                                  }

                      :static-wf {

                                  }

                      }))


(declare <ui>)                                              ;; (defc <ui> []) - exported ui component


(def init! (partial cmn/default-init! *UI-STATE))
(def reload! (partial cmn/default-reload! *UI-STATE))


;; sample blog posts, used as a source for crud

(defonce *TEST-DATA
         (atom {
                :posts [
                        {:header  "post1"
                         :created "2019.01.01"
                         :details "POST 1"}

                        {:header  "post2"
                         :created "2019.06.01"
                         :details "POST 2"
                         }]}))

;; generic wf stuff
(defn keep-xtor-ref [*wf params]
  {
   :before-process  (fn [wf-chan xtor]
                      (swap! *wf assoc ::xtor xtor)
                      :ok)
   :op-handlers-map {
                     :done    (fn [result]
                                (swap! *wf assoc ::result result)
                                (swap! *wf assoc ::status :done)
                                (.log js/console "done" result))

                     :process (fn [result]
                                ;; (.log js/console result)
                                (swap! *wf assoc ::result result))

                     :error   (fn [result]
                                (swap! *wf assoc ::status :error)
                                (.error js/console result))
                     }
   })

;; 1. create running ui wf with ui loop

(defn ui-wf [*wf
             init-fns
             ctx-fns
             steps-fns
             opt-fns]

  (let [wf-params-fn identity                               ;; transforms initial map to a wf params
        ; latest fn that returns params
        opt-params-fn (fn [params]
                        ;; maybe store params here
                        ;(.log js/console "params" params)
                        ;(swap! *wf assoc ::params params)
                        params
                        )                                   ;; transforms wf params to opt params


        wf (base/parametrized-wf!
             (base/combine-init-fns init-fns)
             wf-params-fn
             opt-params-fn

             (base/combine-fns opt-fns :merge-results base/merge-opts-maps)
             (base/combine-fns ctx-fns)
             (base/combine-fns steps-fns))]

    ; store wf
    (swap! *wf merge wf)
    (swap! *wf assoc ::status :running)
    (base/run-wf! wf identity)
    )
  )

;; todo: map-based run function



; nice way of displaying wf results

(defn short-key [k]
  (clojure.string/replace-all (pr-str k) #":woof.playground.prototype6/" "::"))


(defn short-value [v]
  (if (utils/channel? v)
    "<channel>"
    (d/pretty v)
    )
  )


(rum/defc <wf-results> < rum/static [results]
  (into [:div.proto6-results
         [:header "RESULTS"]
         ]
        (map (fn [[k v]]
               [:div.kv
                [:.k {:style {:min-width "38%"}} (short-key k)]

                ; maybe pass steps and ctx here
                ; [:.step "aaa"]

                ; how to know how to show value
                [:.v (short-value v)]]
               ) results))
  )


;;
;;
(rum/defc <generic-wf-ui> < rum/reactive [*wf cmp]
  (let [wf @*wf
        status (get wf ::status :not-started)
        ;; ui-state (get-in wf [::ui] {})
        ]
    [:div {:style {:outline "1px solid crimson"
                   :padding "1rem"}}

     (ui/menubar (pr-str status) (if (= :running status)
                                   [["stop" (fn []
                                              (if-let [xtor (::xtor @*wf)]
                                                (base/end! xtor)
                                                (println "no :xtor found")))]]
                                   []))


     (if (= :running status)
       [:div
        (cmp wf)
        ;; display generic results
        (<wf-results> (::result wf))
        ])
     ]
    )
  )



(rum/defc <timer-ui> < rum/static [wf]
  (let [ui-state (get-in wf [::ui] {})]
    [:h2 (str "now is " (str (js/Date. (::now ui-state))))]))


(rum/defc <test-ui> < rum/static [wf]
  (let [ui-state (get-in wf [::ui] {})]
    [:div
     [:pre
      (d/pretty ui-state)
      ]

     [:div {:style {:display "flex"
                    :flex    "1 0 auto"}}
      (into [:ul.listbox {:style {:width "33%"}}]
            (map-indexed (fn [i id]
                           ; note that vs and selected-id can be out of sync
                           (let [selected-id (::list-selected ui-state)
                                 vs (get ui-state ::list-v [])]
                             [:li (merge {:on-click
                                          (fn [e]
                                            (let [loop-chan (get-in wf [::params ::loop])
                                                  selected-step (wf/rand-sid "sel")
                                                  ]
                                              ;(.log js/console loop-chan)
                                              (go
                                                (async/>! loop-chan
                                                          {
                                                           selected-step           [:ui-select [id]]
                                                           (wf/rand-sid "details") [:ui-details id]
                                                           })
                                                ))
                                            )}
                                         (if (= selected-id id) {:class "selected"} {})
                                         )


                              (str (nth vs i "not-found!!!") " (" (pr-str id) ")")
                              ]
                             ))
                         (get ui-state ::list-ids [])
                         ))

      (if-let [post (::details ui-state)]
        [:div
         [:header (:header post) [:span {:style {:margin-left "3rem"}} (:created post)]]
         (d/pretty (:details post))
         ]
        )
      ]

     ]))

; we need a step that will return id as a value

(defn evt-loop-init-fn [params] {::loop (async/chan)})
(defn evt-loop-ctx-fn [params]
  {
   :loop {
          :fn       (fn [loop-chan]
                      ;; (u/wiretap-chan in-chan (partial println "UI LOOP:"))
                      loop-chan)

          :infinite true
          :expands? true
          }
   })
(defn evt-loop-steps-fn [params]
  {
   ::loop [:loop (::loop params)]
   })


(defn common-ctx-fn [params]
  {
   ; or use notion with * and &
   :identity   {:fn identity}
   :collect    {
                :fn       identity
                :collect? true
                }
   :kv-collect {:fn       (fn [kvs]
                            (reduce merge {} kvs))
                :collect? true
                }

   :log        {:fn (fn [v]
                      (.warn js/console v)
                      v
                      )}

   :log*       {:fn       (fn [v]
                            (.warn js/console v)
                            v)
                :collect? true
                }
   }
  )

;; which naming convention use to distinguish between higher order functions (or fns that need to
;; be 'specified') and ready to use

(defn _init-ui-state-fn [*wf params]
  (swap! *wf assoc ::params params)                         ;; save params
  (swap! *wf assoc ::ui {})                                 ;; make a map for ui stuff

  params)


(defn chain-expanded-ctx-cfg [wrapper-step]
  {
   :fn       (fn [sid-list]
               (reduce (fn [a p]
                         (assoc a (wf/rand-sid) [wrapper-step p]))
                       {} sid-list))
   :expands? true
   :collect? false
   }
  )

(rum/defc <ui> < rum/reactive [*STATE]
  (let [*wf (rum/cursor-in *STATE [:static-wf])             ;; wf with ui state
        init-ui-state-fn (partial _init-ui-state-fn *wf)
        POSTS* (rum/cursor-in *TEST-DATA [:posts])]
    [:div.proto

     [:header "DB like system UI "]

     [:p "idea: have a way extend existing workflow to work with ui. old wf should work"]
     [:p "CRUD like stuff on atom, via infinite steps"]

     ;[:p "working with collections returned by workflow"]
     ;[:p "* collection can be returned as value (usual step handler)"]
     ;[:p "* collection can be split into items (via expand step handler) and then collected"]
     ;[:p "each single item can be modified/enriched by wrapping these with wrapper steps"]


     [:div {:style {:margin-bottom ".5rem" :margin-top "1.5rem"}}

      (ui/menubar "WF-specific actions: "
                  [
                   ["init!" (fn []
                              (let [blog-ctx-fn (fn [params]
                                                  {

                                                   :posts       {
                                                                 :fn (fn [_]
                                                                       (:posts @*TEST-DATA)
                                                                       ; (swap! *ui assoc ::now v)
                                                                       )
                                                                 }

                                                   :p           {:fn identity}

                                                   :enrich-post {:fn (fn [post]
                                                                       (assoc post :now (utils/now)))}

                                                   :posts*      {
                                                                 :fn       (fn [posts]
                                                                             (reduce (fn [a p]
                                                                                       (assoc a (wf/rand-sid "POST-") [:identity p])
                                                                                       ) {} posts))
                                                                 :expands? true
                                                                 }

                                                   ;; how to call these
                                                   :wrap*       (chain-expanded-ctx-cfg :enrich-post)
                                                   })
                                    blog-steps-fn (fn [params]
                                                    {
                                                     ::infinite-posts  [:watch :posts-watcher]
                                                     ::posts*          [:posts* ::infinite-posts]

                                                     ::enriched-posts* [:wrap* ::posts*]
                                                     ::POSTS           [:collect ::enriched-posts*]
                                                     })
                                    ui-blog-ctx-fn (fn [params]
                                                     (let [*ui (rum/cursor-in *wf [::ui])]
                                                       {
                                                        ;; how to represent a list box state?

                                                        ;; first, try with id
                                                        :ui-posts-ids          {
                                                                                :fn (fn [posts]
                                                                                      (swap! *ui assoc ::list-ids posts)
                                                                                      posts)
                                                                                }

                                                        :post-title            {
                                                                                :fn (fn [post]
                                                                                      (:header post)
                                                                                      )
                                                                                }

                                                        :ui-post-titles        (chain-expanded-ctx-cfg :post-title)

                                                        :ui-collected-titles   {
                                                                                :fn       (fn [posts]
                                                                                            (swap! *ui assoc ::list-v posts)
                                                                                            posts)
                                                                                :collect? true
                                                                                }

                                                        :ui-select             {
                                                                                ;; note that we pass a sid-list instead of single id
                                                                                :fn (fn [[id]]
                                                                                      (swap! *ui assoc ::list-selected id)
                                                                                      id
                                                                                      )
                                                                                }

                                                        ;; return values as kv
                                                        :ui-posts-ids-1        {
                                                                                :fn (fn [sids]
                                                                                      (.warn js/console sids)
                                                                                      {::list-ids sids}
                                                                                      )
                                                                                }
                                                        :ui-collected-titles-1 {
                                                                                :fn       (fn [posts]
                                                                                            {::list-v posts}
                                                                                            )
                                                                                :collect? true
                                                                                }


                                                        :ui-details            {
                                                                                :fn       (fn [post]
                                                                                            (swap! *ui assoc ::details post)
                                                                                            post
                                                                                            )
                                                                                :collect? true
                                                                                }

                                                        :ui-merge-state        {
                                                                                :fn (fn [new-state]
                                                                                      (.log js/console "MERGE" new-state)

                                                                                      (swap! *ui merge new-state)
                                                                                      new-state
                                                                                      )
                                                                                }
                                                        }
                                                       ))
                                    ui-blog-steps-fn (fn [params]
                                                       {
                                                        ; build list-box items headers
                                                        ::list-titles   [:ui-post-titles ::posts*]

                                                        ;; store separately to [::ui ::list-ids] and [::ui ::list-v]
                                                        ;; ::list-posts [:ui-posts-ids ::posts*]
                                                        ;; ::collected-titles [:ui-collected-titles ::list-titles]

                                                        ;; how to collect these in a transactional way

                                                        ::list-box-ids  [:ui-posts-ids-1 ::posts*]
                                                        ::list-box-vals [:ui-collected-titles-1 ::list-titles]



                                                        ;; indirect propagation is not working :(
                                                        ::lll           [:log* [::list-box-ids ::list-box-vals]]


                                                        ;; !!!! - this one is not updated
                                                        ::merge         [:kv-collect [::list-box-ids ::list-box-vals]]
                                                        ::ui-set-state  [:ui-merge-state ::merge]

                                                        }
                                                       )

                                    watcher-init-fn! (fn [watcher-id DATA* params]
                                                       ; for now put ht
                                                       (let [ch (async/chan)]
                                                         (async/put! ch @DATA*)
                                                         (add-watch DATA*
                                                                    watcher-id
                                                                    (fn [key atom old-state new-state]
                                                                      (async/put! ch new-state)))
                                                         {
                                                          ::watchers (merge {
                                                                             watcher-id [ch DATA*]
                                                                             } (get params ::watchers {}))
                                                          }
                                                         )
                                                       )
                                    watcher-ctx-fn! (fn [params]
                                                      {
                                                       :watch {:fn       (fn [watcher-id]
                                                                           ;; store channel and data*
                                                                           (get-in params [::watchers watcher-id 0]))
                                                               :infinite true
                                                               }
                                                       }
                                                      )

                                    ]
                                (ui-wf *wf
                                       ; init
                                       [init-ui-state-fn evt-loop-init-fn
                                        ; init-watcher
                                        (partial watcher-init-fn!
                                                 :posts-watcher
                                                 POSTS*)
                                        ]
                                       ; ctx
                                       [common-ctx-fn
                                        evt-loop-ctx-fn

                                        watcher-ctx-fn!
                                        blog-ctx-fn         ; main logic (w/o ui stuff)
                                        ui-blog-ctx-fn]
                                       ; steps
                                       [evt-loop-steps-fn
                                        blog-steps-fn
                                        ui-blog-steps-fn

                                        ]
                                       ;; provides opts map via opt params
                                       [(partial keep-xtor-ref *wf)
                                        ; tear down watcher
                                        (fn [params]
                                          {:op-handlers-map
                                           {
                                            :error (fn [err]
                                                     (utils/close-channels! err)
                                                     err)
                                            :done  (fn [result]
                                                     (utils/close-channels! result)
                                                     result)
                                            }})
                                        (fn [params]
                                          (let [stop-watch! (fn [v]
                                                              (doseq [[watcher-id [_ *DATA]] (get params ::watchers)]

                                                                (remove-watch *DATA watcher-id))
                                                              (prn "END!")
                                                              v)]
                                            {:op-handlers-map
                                             {
                                              :error stop-watch!
                                              :done  stop-watch!
                                              }}
                                            )

                                          )
                                        ]
                                       )
                                )
                              )]
                   []
                   ["new post" (fn []
                                 (let [c (inc (count @POSTS*))]
                                   (swap! POSTS* conj {
                                                       :header  (str "post" c)
                                                       :created "2019.08.01"
                                                       :details (str "NEW POST" c)
                                                       })
                                   )
                                 )]])
      ]


     ;; maybe extend <test-ui> to actions inside?
     (<generic-wf-ui> *wf <test-ui>)
     ]
    )

  )


;; how to stop running wf on reload??

;; root ui component for prototype
(def <app> #(<ui> *UI-STATE))
