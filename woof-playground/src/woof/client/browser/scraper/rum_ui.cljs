(ns woof.client.browser.scraper.rum-ui
  (:require
    [goog.dom :as dom]
    [goog.functions :as fs]

    [cljs.core.async :refer [go go-loop] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]


    [rum.core :as rum]

    [woof.base :as base]
    [woof.data :as d]
    [woof.utils :as u]

    [woof.client.dom :as woof-dom]
    [woof.wfs.watcher :as watcher]


    [woof.client.playground.ui :as ui]
    [woof.client.playground.ui :as pg-ui]

    [woof.client.browser.scraper.scraping-ui :as s-ui]

    ))

;;
;; wrapper to add rum ui to a scraping workflow
;;

(defn gen-rum-ui [<scraping-ui>]
  ;; ndrw: should separate arity be used for UI mount?
  (fn
    ([]
     ;; init UI
     (let [$panel (dom/createDom "div" "panel woof-scrape-panel ui-container")]
       (woof-dom/ui-add-el! $panel))
     )
    ([*state STATE]
     (if-let [container-el (woof-dom/q ".ui-container")]
       ;; react UI
       (rum/mount (<scraping-ui> *state STATE) container-el)
       (let [$panel (dom/createDom "div" "panel woof-scrape-panel ui-container")]
         (woof-dom/ui-add-el! $panel)
         (rum/mount (<scraping-ui> *state STATE) $panel))
       )))
  )




(defn ui-impl! [*WF-UI <rum-ui>]
  (let [WATCHER-ID (base/rand-sid "watcher-")
        WATCHER-CFG {
                     :watcher-id WATCHER-ID
                     ;;
                     :send-channel-updates? false
                     ;;
                     }
        debounce-time 33;333
        ]

    ;; A) SIDE-EFFECTS
    {
     :init [
            (fn [params]
              ;; B) SIDE-EFFECTS

              (merge
                {
                 :wf/*UI *WF-UI
                 }
                (watcher/_watcher-cf-init-cb
                  WATCHER-ID *WF-UI
                  (fs/debounce (fn [*state state]
                                 ;(.log js/console "RENDER")
                                 (<rum-ui> *state state))
                               debounce-time)
                  #_(fn [*state state]
                    (<rum-ui> *state state)
                    ;; (.log js/console "UI: upd" state (= state @*state))
                    )
                  params)))]

     :ctx  [watcher/watcher-ctx]
     :opts [
            ;; C) SIDE-EFFECTS
            ;;
            #_{:before-process (fn [_ _]
                                 ; (<rum-ui>)

                                 :ok
                                 )}
            watcher/watcher-opts]
     }
    )
  )



(defn _on-run! [*WF-UI meta-info scraper-impl-map prev-state]
  ;; update the API with stop function
  (let [{api :api} scraper-impl-map]

    (when (seq api)
      (swap! *WF-UI assoc :api
             (into [["WF:\uD83D\uDEAB" (fn []

                                         (js* "woof.browser.stop_workflow();")

                                         (swap! *WF-UI update-in [:api] concat
                                                [["WF: run!"
                                                  (fn []
                                                    (js* "woof.browser.run_workflow();")
                                                    )]])
                                         )]
                    []]
                   api)
             ))))



;;
;;


;; ui for scraping workflow

(rum/defc <scraping-ui> < rum/static
  [*state STATE]

  [:div.woof-scraper-control

   (when (seq (:api STATE))
     (ui/menubar "API" (:api STATE) :class "woof_api"))


   (let [ids (get STATE :IDs/ids #{})
         sent-ids (get STATE :IDs/sent #{})
         ]
     [:div
      [:header (str "IDS (" (count ids)) ")"]

      (pr-str (clojure.set/difference ids sent-ids))
      ]

     )

   ;(str "ids: (" (count (get STATE :ids #{})) ")")


   #_[:button {:on-click (fn [e]
                           #_(.log js/console
                                   (filter #(= -1 (get % :rooms)) (:scraped STATE)))
                           (doseq [z (:scraped STATE)]
                             (.log js/console (select-keys z
                                                           #{:area
                                                             :area_total
                                                             :area_living
                                                             :area_kitchen}
                                                           ))
                             )
                           )} "YYYYYY"]

   ;;
   (when-let [data (:scraped STATE)]
     (pg-ui/<transform-list>
       s-ui/<listing>
       data
       ;; filters
       {} ;(group-by :ID @*asserts)
       :id-fn :id
       ;:copy-fn #(dissoc % :test)
       ;:sort-fn (fn [a b] (compare (:code a) (:code b)))
       :filter-map {
                    ;"test-street" (partial pg-ui/_marker-class-filter "test-street")
                    ;"drv-street" (partial pg-ui/_marker-class-filter "drv-street")
                    ;"non drv-street" (partial pg-ui/_marker-except-class-filter "drv-street")
                    ;"no-house" (partial pg-ui/_marker-class-filter "no-house")
                    }
       )
     )

   ;[:hr]
   #_[:pre
      (d/pretty! (-> STATE
                     (dissoc :api)
                     (dissoc :scraped)
                     ))
      ]

   ])



(def <rum-ui> (gen-rum-ui <scraping-ui>))


(defn ui-sub-wf [*WF-UI API]
  (assoc
    ;; cfg: debounce interval
    (ui-impl! *WF-UI <rum-ui>)
    :steps
    [(fn [params]
       {
        ; :CSS/test-page-styles [:css-file "http://localhost:9500/css/t.css"]
        :CSS/scraper-styles   [:css-file "http://localhost:9500/css/r.css"]
        })]
    )
  )