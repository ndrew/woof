(ns woof.wf.popup.example1
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.wfc :as wfc]

    [woof.utils :as u]
    [woof.ui :as ui]

    [woof.wf-ui :as wf-ui]

    [woof.ui.results :as r]

    [markdown.core :refer [md->html]])


  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))

;; example of workflow with custom ui


(defn- DBG [a]
  (.warn js/console (d/pretty a)))


;;
;; initializers - context, steps


(defn prepare-params
  ""
  []
  {
       :ui-chan (async/chan)
       :editor-chan (async/chan)
       :confirm-chan (async/chan)
     }
  )


(defn- get-test-context-map
  ""
  []

  {
    :initial {:fn (fn [x]
                {:text "Hello Editor!"})}

    :text {:fn (fn [x] (:text x))}

    :md {:fn (fn [x] (md->html x))}

  }
  )



(defn- pipe-fn [editor-chan s]

  (let [c (async/chan)]
    ;; send initial value if any
    (if-not (or (nil? s) (u/channel? s))
      (go (async/put! c s)))

    ;; wait for new values
    (go-loop []
             (when-let [v (async/<! editor-chan)]
               (async/put! c v)
               (recur))

             )
    c))


(defn gen-passthrough-fn [confirm-chan]
  (let [acc (volatile! nil)
        c (async/chan)]

    (go-loop []
               (when-let [v (async/<! confirm-chan)] ; read from preview chan
                 (async/put! c @acc)
                 (recur)))

    (fn[v]
      (vreset! acc v)
      c
      )
    )

  )






(defn context-map-fn [& {:keys [ui-chan editor-chan confirm-chan]}] ;;
  (merge
    {
    :id (wf/step-handler (fn [a] a))

    :log (wf/step-handler (fn [a] (println a)))


    ;; infinite expander - for adding new steps to workflow, usually from UI
    :ui-loop {:fn (fn [in-chan]
                    (u/wiretap-chan in-chan (partial println "UI:")))
              :infinite true
              :expands? true
              }


    ;; infinite editor from channel
    :editor {:fn (partial pipe-fn editor-chan)
                                   :infinite true}


    ;; ininite collector that take last value from chann
    :result {:fn (gen-passthrough-fn confirm-chan)
                                                 :infinite true
                                                 :collect? true
                                                 }


    :save {:fn (fn [v]
                 (println "SAVING: " (d/pretty v))
                 v
                 )}
    }
    (get-test-context-map)
    )
  )


(defn steps-fn
  ""
  [& {:keys [ui-chan]}]

    {
      ::ui    [:ui-loop ui-chan]

      ::log [:log "Hello!"]


      ::initial [:initial nil]
      ::initial-text [:text ::initial]

      ::text [:editor ::initial-text]

      ::md [:md ::text]

      ;; wait for result
      ::preview [:result ::md]

      ; ::save [:save ::preview]

    }
  )




(defn actions-fn [& {:keys [ui-chan editor-chan confirm-chan]}]
  (let [send-ui-action (fn [steps]
                          (go
                            (async/>! ui-chan steps)))]
    {;; :start! (fn[])
     :stop!  (fn[]
               ; (println "close")
               (async/close! ui-chan)
               (async/close! editor-chan)
               (async/close! confirm-chan)
               )
     :reset! (fn[]
               ; (println "reset!")
               )

     :actions [

      ["save!" (fn []
                 ;; ; (send-ui-action { (wf/rand-sid) [:save ::md] })

                 (go (async/>! confirm-chan :ok))

                 (send-ui-action { (wf/rand-sid) [:save ::preview] })
                 )
       ]


      ]
     }))






(defn wf!
  "*local - state atom for ui updates"
  [*local initial-params]

  (let [a 123] ;; todo: remove this
    {
      :wf (fn [params]
            (wfc/params-wf params context-map-fn steps-fn)
            )

      :params (merge initial-params
                     {
                       :api {
                               ;;
                              }

                       :actions [
                                  ["yo" (fn[]
                                          (println (dissoc initial-params :*state)))]

                                  ;; todo: add some actions
                                  ]
                       })
      }
    )

  )



(rum/defc text-editor < rum/reactive
  [value change-fn]
  [:textarea { :type "text"
                :value value
                ;:style { :width 170 }
                :on-change (fn [e]
                             (change-fn (.. e -currentTarget -value))
                             ;(reset! *ref (long (.. e -currentTarget -value)))
                             ) }])



(rum/defcs <wf-editor>     <      rum/reactive {:key-fn (fn [k _ _] k)}
                                       (rum/local nil ::updated-v)
                                       (rum/local false ::manually-entered)
  [local header params editor-chan]

  ;; todo: store both params and manually entered value if any

  (if-let [local-params @(::updated-v local)]
    (if-not (or (= local-params params) @(::manually-entered local))
      (reset! (::updated-v local) params))
    (reset! (::updated-v local) params))


  (if-let [v @(::updated-v local)]
    [:.text-editor
     [:header header]
     #_(ui/data-editor (fn[new-v]
                       (reset! (::updated-v local) new-v)
                       (reset! (::manually-entered local) true)
                       (go
                         (async/put! editor-chan new-v)))
                     v)

     (text-editor v (fn[new-v]
                       (reset! (::updated-v local) new-v)
                       (reset! (::manually-entered local) true)
                       (go
                         (async/put! editor-chan new-v))))
     ])
  )



(rum/defcs <popup-ui> < rum/reactive
  [local *STATE editor-chan]

  (let [cursor (partial rum/cursor-in *STATE)
        {status :status
         ;wf :wf
         ;full-history :history

         result :result

         steps :steps
         } @*STATE



        ;; todo: move this to upper level - ui-fn


        ;; todo: take sid-list into account
        ;show-param-editor? (and editor-chan
        ;                        (if (wf/sid? params) (not (u/channel? (get result params))) true))

        editor-fn (if editor-chan;; show-param-editor?
                    (fn []
                      (let [[action params] (get steps ::text)]
                        (if (wf/sid? params)

                          (<wf-editor> "TEXT"
                                       (if-let [nu-params (get result params)] nu-params params)
                                       editor-chan))
                        )

                      )
                    nil)

        ]

    [:div.popup

     (wf-ui/<wf-menu-ui> "POPUP:" status @(cursor [:status-actions]))

     [:hr]


   ;  (r/<wf-step-ui> ::editor [:editor "HELLO"]
   ;                  @(cursor [:result]) editor-fn nil) ;; editor-fn preview-fn

     [:div.flex {:style {:width "60%"}}
       [:div
        ;; [:div @(cursor [:result ::text])]
        (editor-fn)
        ]
       ;[:div @(cursor [:result ::md])]
       [:div
        [:header "Preview"]

        [:div {:dangerouslySetInnerHTML {:__html @(cursor [:result ::md])}}]]
      ]

     [:pre
      (d/pretty (keys @*STATE))
      ]





     ]
    )
  )





;; todo:
(defn ui-fn [& {:keys [ui-chan editor-chan confirm-chan]}]
  (fn [*STATE]
    (<popup-ui> *STATE editor-chan confirm-chan)
    )

  )



