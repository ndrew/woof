(ns woof.client.browser.example.seq-wf
  (:require

    [cljs.core.async :refer [go go-loop] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]
    [clojure.string :as str]

    [goog.dom :as dom]
    [goog.dom.classes :as classes]

    [woof.base :as base]
    [woof.client.dom :as woof-dom :refer [q q* html! btn!]]
    [woof.client.dbg :as dbg :refer [__log]]
    [woof.data :as d]
    [woof.utils :as u]

    ;; common wf
    [woof.wfs.alpha :as alpha]
    [woof.wfs.watcher :as watcher]
    [woof.wfs.evt-loop :as evt-loop]

    ))


;; http://localhost:9500/example/seq.html


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
   ;; update UI on state change
   (let [$panel (q ".woof-scrape-panel")
         $pre (q ".woof-scrape-pre")
         ]

     (html! $pre (d/pretty! STATE))
     ;; (html! root "")
     )
   )
  )



(defn _parse-article-in-order [i el]
  (.log js/console "parse: i=" i el)

  (classes/add el (str "marker-" i))

  {
   :i i
   }
  )



(defn wf! [*wf-state meta-info]

  (let [WATCHER-ID :state

        ;; watch for changes in internal state
        *state (atom {
                      :internal ::state
                      })]
    {
     :init    [
               (fn [params]

                 ;; clean up-previously added css classes
                 (woof-dom/remove-added-css ["marker-1"
                                             "marker-2"
                                             "marker-3"
                                             "marker-4"
                                             "marker-5"
                                             "marker-6"
                                             "marker-7"
                                             "marker-8"
                                             "marker-9"
                                             "marker-10"
                                             ])

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

               ;; add linearization worker
               (partial alpha/_seq-worker-init ::linearizer)
               ]

     :ctx     [watcher/watcher-ctx

               (fn [params]
                 {
                  :ui {:fn       (fn [state]
                                   (<ui> *state state))
                       :collect? true
                       }
                  }
                 )

               (fn [params]
                 (let [*i (atom 0)]
                   {
                    :process-normal  {
                                      :fn (fn [el]
                                            (let [i (swap! *i inc)]
                                              (.log js/console ":process-normal\ti=" i el)
                                              (_parse-article-in-order i el)
                                              )
                                            )

                                      }
                    ;; run each :process-renamed-table in random order
                    :process-normal* (base/expand-into :process-normal)

                    :process-seq*    {
                                      ;; here should be used ::linearizer worker

                                      :fn       (partial alpha/_seq-worker-expander
                                                         ::linearizer
                                                         (fn [el]
                                                           (let [i (swap! *i inc)]
                                                             (.log js/console ":process-seq*\ti=" i el)
                                                             (_parse-article-in-order i el)
                                                             )
                                                           )
                                                         params
                                                         )
                                      :expands? true
                                      }

                    }
                   )
                 )

               ]

     :steps   [

               (fn [params]
                 {

                  ::hello              [:log "seq ordering stuff"]

                  ;; add custom css to display parse order
                  ::css-file           [:css-file "http://localhost:9500/css/example/seq.css"]

                  ::$articles          [:query-selector-all "article"]


                  ;; expand in non-deterministic order
                  ;;::processed-articles [:process-normal* ::$articles]

                  ;; expand in proper order
                  ::processed-articles [:process-seq* ::$articles]



                  ;; add infinite step in order to keep wf running

                  }
                 )

               ;;
               #_(fn [params]
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

               ;; render UI on state change
               (fn [params] {
                             :UI/state  [:watch WATCHER-ID]
                             :UI/render [:ui :UI/state]})
               ]
     :opts    [
               watcher/watcher-opts
               ]

     :api     {
               "change state" (fn []
                                (swap! *state assoc :t (u/now))
                                )

               }

     :on-stop (fn [state]
                (__log "GENERIC: ON STOP")



                #_(when-let [*old-state (get-in state [:WF/params ::state])]
                    (.log js/console "OLD STATE:" @*old-state)
                    ; (reset! *state @*old-state)
                    )

                ;; can return channel
                )
     }
    )
  )