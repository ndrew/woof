(ns woof.playground.old.results
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]

    [woof.data :as d]
    [woof.wf :as wf]

    [woof.playground.old.ui :as old-ui]
    [woof.client.playground.ui :as ui]
    [woof.playground.old.wf-ui :as wf-ui]
    [woof.utils :as u]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]
    [woof.utils-macros :refer [put!?]]))


;; step editor

(rum/defcs <wf-step-editor>     <      rum/reactive {:key-fn (fn [k _ _ _] k)}
                                       (rum/local nil ::updated-v)
                                       (rum/local false ::manually-entered)
  [local header params editor-chan]

  ;; todo: store both params and manually entered value if any

  (if-let [local-params @(::updated-v local)]
    (if-not (or (= local-params params) @(::manually-entered local))
      (reset! (::updated-v local) params))
    (reset! (::updated-v local) params))


  (if-let [v @(::updated-v local)]
    [:.editor
     [:header header]
     (old-ui/data-editor (fn[new-v]
                       (reset! (::updated-v local) new-v)
                       (reset! (::manually-entered local) true)
                       (go
                         (async/put! editor-chan new-v)))
                         v)])
  )



;; row

(rum/defc <wf-step-ui> < rum/reactive {:key-fn (fn [k _ _ _] k)}

  [k v result editor-fn preview-fn]

  (let [[action params] v]
    [:.step-ui {:class (str
                         (if editor-fn "edit" "")
                         (if preview-fn "prev" ""))}

     [:span.dbg-k (d/pretty k)]

     (if editor-fn
       (editor-fn))

     (if preview-fn
       (preview-fn))

     (wf-ui/<step-status> k v (u/nil-get result k))]
    ))




;;
;;


(rum/defc <wf-full-step-ui>
  < rum/reactive
    { :key-fn (fn [_ _ [header _]] header)}
  [result *editors [k v]]

  (let [[action params] v

        {param-editors :pre
         previews :post} @*editors

        editor-chan (get param-editors k)

        ;; todo: take sid-list into account
        show-param-editor? (and editor-chan
                                (if (wf/sid? params) (not (u/channel? (get result params))) true))

        editor-fn (if show-param-editor?
                    (fn []
                      (if (wf/sid? params)
                        (if-let [nu-params (get result params)]
                          (<wf-step-editor> (str (name k) action ": ") nu-params editor-chan))
                        (<wf-step-editor> (str (name k) action ": ") params editor-chan))
                      )
                    nil)

        preview-chan (get previews k)

        preview-fn (if preview-chan
                     (fn []
                       [:.preview
                        (ui/menubar "Preview"
                                        [["ok" (fn[]
                                             (go
                                               (async/>! preview-chan :ok))
                                             ;; TODO:
                                             (swap! *editors update-in [:post] dissoc k)
                                             )]])
                        [:div.preview-content

                         (let [get! (partial u/nil-get result)
                               preview-data (get! (second v))]
                           (if (wf/sid-list? preview-data)
                             (map get! preview-data)
                             preview-data))

                         ]])
                     nil)

        ]

    (<wf-step-ui> k v result editor-fn preview-fn)
    )
  )




#_(let [gsteps (group-by
                 (fn[[step-id sbody]]
                   (let [v (get-value result step-id)]
                     (cond
                       (u/channel? v) :channel
                       (wf/sid? v) :sid
                       (wf/sid-list? v) :sid-list
                       :else :v
                       )
                     )
                   )
                 actual-steps)]
    [:pre (pr-str
            (keys gsteps) )]
    )


(defn- get-value [result k]
  (let [get! (partial u/nil-get result)
        raw-data (get! k)]
    (if (wf/sid-list? raw-data)
      (map get! raw-data)
      raw-data)))


(rum/defcs <wf-results-ui>
  < rum/reactive
    (rum/local false ::pending-only?)
    { :key-fn (fn [header _ _] header)}

  [local header result actual-steps *editors]
  ;; todo: store changed params
  [:.results

   ;; todo: toggle button
   #_(ui/menubar "" [["pending?" (fn []
                                   (swap! (::pending-only? local) not)
                                   )]])


   #_(if @(::pending-only? local)
       (do
         (map (partial <wf-full-step-ui> result *editors)
              (filter (fn[[step-id sbody]]
                        (let [v (get-value result step-id)]
                          (or
                            (u/channel? v)
                            (wf/sid? v)
                            (wf/sid-list? v)
                            ))) actual-steps))))


   (into [:div.steps] (map (partial <wf-full-step-ui> result *editors) actual-steps))



   ]

  )
;;
;;


(rum/defc <test>
  < rum/reactive
  []

  [:div
   (<wf-results-ui> "header"
                    {::hello "www"}  ;; result
                    {
                     ::hello [:action {}]
                     } ; (sort-steps actual-steps)

                    (atom {:pre   {}
                           :post {}}) ;; *editors
                    )
   ]
  )


