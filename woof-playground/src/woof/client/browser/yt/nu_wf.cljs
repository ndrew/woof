(ns woof.client.browser.yt.nu-wf
  (:require

    [woof.base :as base :refer [rand-sid]]

    [rum.core :as rum]

    [woof.client.dom :as woof-dom]

    [woof.client.playground.ui :as ui]

    [woof.client.dbg :as dbg :refer [__log]]

    [woof.client.browser.scraper.rum-ui :as rum-wf]
    [woof.client.browser.scraper.actions :as api-wf]

    ))

;; scraping wf example
;; - get watch history


;; WORKFLOW UI
;;

(rum/defc <scraping-ui> < rum/static
  [*state STATE]

  [:div.woof-scraper-control
   (when (seq (:api STATE))
     (ui/menubar "API" (:api STATE)))

     "..."
   ])

(def <rum-ui> (rum-wf/gen-rum-ui <scraping-ui>))


;; WORKFLOW ACTIONS (API)
;;
(defn wf-api [*wf-state]
  [
   (api-wf/chord-action (woof-dom/chord 49 :shift true )
     "HELLO" (fn []
               (.log js/console "HELLO")))

   ]
  )



(defn wf! [*wf-state meta-info]

  (let [
        *WF-UI (rum/cursor-in *wf-state [:wf/UI])

        _UI_ (rum-wf/ui-impl!
               *WF-UI
               <rum-ui>)

        API (wf-api *wf-state)

        _API_ (api-wf/actions-impl! API api-wf/default-on-chord)

        ;; *WF-scrape-data (rum/cursor-in *WF-UI [:scraped])

        ]
    {
     :init  (concat
              (get _UI_ :init [])
              (get _API_ :init [])
              )


     :ctx   (concat
              (get _UI_ :ctx [])
              ;[(make-ctx-fn *WF-scrape-data)]
              )

     :steps [
       ;; UI
       {
        :CSS/playground-styles [:css-file "http://localhost:9500/css/r.css"]

        }
       ;;
       {
        ::hello [:prn "scraping started!!!"]
        }
       ]

     :opts  (concat
              (get _UI_ :opts [])
              (get _API_ :opts []))

     :api   API

     :on-stop (fn [state]
                (__log "ON STOP")
                ;; for now do not return channel
                nil)

     ;; for now provide custom on-run handler - as older wf APIs are a map
     :scraper/on-run! (partial rum-wf/_on-run! *WF-UI)
     }
    )
  )
