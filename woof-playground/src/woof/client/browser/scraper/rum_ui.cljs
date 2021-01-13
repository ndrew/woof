(ns woof.client.browser.scraper.rum-ui
  (:require
    [goog.dom :as dom]

    [rum.core :as rum]

    [woof.base :as base]
    [woof.client.dom :as woof-dom]
    [woof.wfs.watcher :as watcher]

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
  (let [WATCHER-ID (base/rand-sid "watcher-")]

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
                  (fn [*state state]
                    ;; (.log js/console "UI: upd" state (= state @*state))
                    (<rum-ui> *state state))
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