(ns woof.client.browser.example.seq-wf
  (:require

    [cljs.core.async :refer [go go-loop] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]
    [clojure.string :as str]

    [goog.dom :as dom]
    [goog.dom.classes :as classes]

    [woof.base :as base]
    [woof.client.dom :as wdom :refer [q q* html! btn!]]
    [woof.client.dbg :as dbg :refer [__log]]
    [woof.data :as d]
    [woof.utils :as u]

    ;; common wf
    [woof.wfs.alpha :as alpha]
    [woof.wfs.watcher :as watcher]
    [woof.wfs.evt-loop :as evt-loop]

    ))

;; example: processing dom elements in order
;;
;; http://localhost:9500/example/seq.html


;; non-react ui
;;
(defn <ui>
  ([]
   ;; init UI
   (let [$panel (dom/createDom "div" "panel woof-scrape-panel")
         $pre  (dom/createDom "pre" "woof-scrape-pre" "")
         $conf  (dom/createDom "div" "woof-scrape-conf" "")
         ]

     (dom/appendChild $panel (dom/createDom "header" "" "SCRAPER STATE:"))
     (dom/appendChild $panel $pre)
     (dom/appendChild $panel $conf)

     (wdom/ui-add-el! $panel)
     )
   )
  ([*state STATE]
   ;; update UI on state change
   (let [$pre (q ".woof-scrape-pre")
         $conf (q ".woof-scrape-conf")]

     ;; (html! $pre (d/pretty! STATE))
     (html! $pre "")

     (let [confirms (get STATE :confirms)
           panel (dom/createDom "div" "" "")]

       (html! $conf "")

       (dom/appendChild $conf panel)
       (doseq [[k v] confirms]
         (let [btn! (wdom/btn! (str k) (fn []
                                             (swap! *state update-in [:confirms] dissoc k)
                                             (async/put! v (u/now))
                                             ) panel)
               ]
           ;;
           )
         )
       )
     ;;(html! $conf (pr-str (get STATE :confirms)))
     )
   )
  )



(defn _parse-article-in-order [i el]
  (.log js/console "parse: i=" i el)

  (classes/add el (str "marker-" (rem i 10)))

  {
   :i i
   }
  )



(defn wf!
  "sequential expands scraping wf"
  [*wf-state meta-info]

  (let [WATCHER-ID :state

        ;; watch for changes in internal state
        *state (atom {
                      :internal ::state
                      :confirms {}
                      })]
    {
     :init    [
               (fn [params]

                 ;; clean up-previously added css classes
                 (wdom/remove-added-css ["marker-0"
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
                                             ])

                 {
                  ::state *state
                  })

               (fn [params]
                 ;; add ui for scraping
                 (<ui>)
                 {})

               ;; state watcher for react like UI updates
               (partial watcher/_watcher-cf-init-cb WATCHER-ID *state
                        (fn [*state state]
                          (.log js/console "UI: upd" state (= state @*state))
                          (<ui> *state state)
                          ))


               ;; add linearization worker
               (partial alpha/_seq-worker-init ::linearizer)
               ]

     :ctx     [watcher/watcher-ctx

               (fn [params]
                 {
                  :ui {:fn       (fn [state]
                                   (.log js/console "UI update from WF")

                                   ;; (<ui> *state state)
                                   )
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


                    :process-seq-async*    {
                                      ;; here should be used ::linearizer worker

                                      :fn       (partial alpha/_seq-worker-expander
                                                         ::linearizer
                                                         (fn [el]
                                                           (let [i (swap! *i inc)
                                                                 make-chan (fn []
                                                                             (base/make-chan (base/&chan-factory params) (base/rand-sid)))
                                                                 ]
                                                             (.log js/console ":process-seq-async*\ti=" i el)
                                                             (let [c (make-chan)
                                                                   res (_parse-article-in-order i el)]
                                                               (.scrollIntoView el true)
                                                               (go
                                                                 (async/<! (u/timeout 1000))
                                                                 (async/put! c res)
                                                                 )
                                                               c
                                                               )

                                                             )
                                                           )
                                                         params
                                                         )
                                      :expands? true
                                      }


                    :process-confirm  {
                                      :fn (fn [el]
                                            (let [i (swap! *i inc)
                                                  make-chan (fn [] (base/make-chan (base/&chan-factory params) (base/rand-sid)))]
                                              (.log js/console ":process-confirm\ti=" i el)

                                              (let [c (make-chan)
                                                    conf-chan (make-chan)]

                                                (.scrollIntoView el true)

                                                (swap! *state update-in [:confirms] assoc (keyword (str "confirm-" i)) conf-chan )

                                                (go
                                                  ;; wait from confirm channel
                                                  ; (async/<! (u/timeout 1000))

                                                  (async/<! conf-chan)
                                                  (async/put! c (_parse-article-in-order i el))
                                                  )
                                                c
                                                )
                                              )
                                            )
                                       }
                    :process-confirm* (base/expand-into :process-confirm)

                    :process-seq+confirm*    {
                                            ;; here should be used ::linearizer worker

                                            :fn       (partial alpha/_seq-worker-expander
                                                               ::linearizer
                                                               (fn [el]
                                                                 (let [i (swap! *i inc)
                                                                       make-chan (fn [] (base/make-chan (base/&chan-factory params) (base/rand-sid)))
                                                                       ]
                                                                   (.log js/console ":process-seq+confirm**\ti=" i el)
                                                                   (let [c (make-chan)
                                                                         conf-chan (make-chan)]


                                                                     (swap! *state update-in [:confirms] assoc (keyword (str "confirm-" i)) conf-chan )

                                                                     (go
                                                                       ;; wait from confirm channel
                                                                       ; (async/<! (u/timeout 1000))

                                                                       (async/<! conf-chan)

                                                                       (.scrollIntoView el true)

                                                                       (async/put! c (_parse-article-in-order i el))
                                                                       )
                                                                     c
                                                                     )

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


               ;; render UI on state change
               (fn [params] {
                             :UI/state  [:watch WATCHER-ID]

                             :UI/render [:ui :UI/state]
                             })

               (fn [params]
                 {

                  ::hello              [:log "seq ordering stuff"]

                  ;; add custom css to display parse order
                  ::css-file           [:css-file "http://localhost:9500/css/example/seq.css"]

                  ::$articles          [:query-selector-all "article"]


                  ;; expand in non-deterministic order
                  ;;::processed-articles [:process-normal* ::$articles]

                  ;; expand in proper order
                  ;;::processed-articles [:process-seq* ::$articles]

                  ;; expand async
                  ;;::processed-articles [:process-seq-async* ::$articles]


                  ;; confirm seq
                  ::processed-articles [:process-seq+confirm* ::$articles]

                  ;; confirm non-deterministic order
                  ;; ::processed-articles [:process-confirm* ::$articles]



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

               ]
     :opts    [
               watcher/watcher-opts
               ]

     :api     {
               "refresh" (fn []
                           (<ui> *state @*state)
                           )

               "change state" (fn []
                                ;; should be updated
                                (swap! *state assoc :t (u/now))
                                )
               }

     :on-stop (fn [state] ;; can return channel
                (__log "GENERIC: ON STOP"))
     }
    )
  )