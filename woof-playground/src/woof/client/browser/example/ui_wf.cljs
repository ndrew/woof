(ns woof.client.browser.example.ui-wf
  (:require
    [cljs.core.async :refer [go] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]
    [clojure.string :as str]

    [goog.dom :as dom]
    [goog.dom.classes :as classes]
    [goog.dom.dataset :as dataset]

    [woof.base :as base]
    [woof.client.dom :as woof-dom :refer [q q* html! btn!]]
    [woof.client.dbg :as dbg :refer [__log]]
    [woof.data :as d]
    [woof.utils :as u]

    ;; common wf
    [woof.wfs.evt-loop :as evt-loop]
    [woof.wfs.watcher :as watcher]
    )
  )


;; http://localhost:9500/example/ui.html


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




(defn wf! [*wf-state meta-info]

  (let [WATCHER-ID :state

        ;; watch for changes in internal state
        *state (atom {
                      :internal ::state
                      })]
    {
     :init    [
               (fn [params]
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
               ]

     :ctx     [watcher/watcher-ctx

               (fn [params]
                 {
                  :ui         {:fn       (fn [state]
                                           (<ui> *state state))
                               :collect? true
                               }
                  }
                 )


               ]

     :steps   [

               (fn [params]
                 {
                  ::hello                [:log "React-like rendering for browser workflows"]

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
     :otps    [
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