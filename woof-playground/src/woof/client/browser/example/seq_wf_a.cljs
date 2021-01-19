(ns woof.client.browser.example.seq-wf-a
  (:require
    ;; core
    [clojure.string :as str]
    [cljs.core.async :as async :refer [go go-loop]]


    ;; woof core
    [woof.base :as base :refer [rand-sid sid
                                &chan-factory make-chan own-chan]]
    [woof.data :as d]
    [woof.utils :as u]

    ;; client utils
    [woof.client.dom :as woof-dom :refer [q q*]]
    [woof.client.dbg :as dbg :refer [__log]]

    ;; wf helpers -
    [woof.client.ws :as ws]

    ; helpers from base
    [woof.wfs.alpha :as alpha]
    [woof.wfs.evt-loop :as evt-loop]

    ;; ui
    [goog.dom.classes :as classes]
    [goog.dom.dataset :as dataset]

    [rum.core :as rum]

    [woof.client.playground.ui :as ui]
    [woof.client.browser.scraper.rum-ui :as rum-wf]
    [woof.client.browser.scraper.actions :as api-wf :refer [chord-action]]
    [woof.client.browser.scraper.scrape :as scrape]


    )
  )



(defn clean-up-dom-side-effects []
  ;; clean up added css
  (woof-dom/remove-added-css [
                              ;;
                              ])
  )

;; _UI_

(rum/defc <scraping-ui> < rum/static
  [*state STATE]

  [:div
   ;; api
   (when (seq (:api STATE))
     (ui/menubar "API" (:api STATE)
                 :class "woof_api") )

   ]
  )

(def <rum-ui> (rum-wf/gen-rum-ui <scraping-ui>))


(defn ui-sub-wf [*WF-UI API]
  (assoc
    (rum-wf/ui-impl! *WF-UI <rum-ui>)
    :steps [(fn [params]
              {
               :CSS/test-styles    [:css-file "http://localhost:9500/css/t.css"]
               :CSS/-styles    [:css-file "http://localhost:9500/css/r.css"]
               })]
    )
  )


;;
;; _RUN_

;; copy-pasted
#_(defn runner-sub-wf [& {:keys [execute-mode
                               t ; imeout
                               n ; chunk size
                               ] :as params
                        :or {execute-mode :idle-w-timeout
                             t 100
                             n 30}}]
  {
   :opts [(fn [params]
            ;; todo: find out the most efficient way of parsing
            (condp = execute-mode
              ;; process wf msgs only when page is idle
              :on-idle         {:execute alpha/execute-on-idle}
              ;; process wf msgs only when page is idle,
              ;; if page is busy then process messages after timeout t
              :idle-w-timeout  {:execute (partial alpha/_execute-on-idle-w-timeout t)}
              ;; process wf msgs on certain time interval t
              :timed-execute   {:execute (partial base/_timed-execute-fn t)}
              ;; process wf msg :process updates in chunks of size n
              :chunked-execute {:execute (partial base/_chunked-execute-fn n)}
              ;; use default execute - may be greedy
              {}
              )
            )]
   })

;;
;; SCRAPE
;;

(defn scraping-sub-wf [*wf-state *WF-UI]
  (let []
    {

     :init []

     :steps [
             {::hello [:log "hello!"]}
             ]

     :api []
     }
    )
  )



;;;;;
;;
;; WF
;;
(defn wf! [*WF-STATE meta-info]
  (let [
        ;; state
        *WF-UI (rum/cursor-in *WF-STATE [:wf/UI])

        ;_RUN_ (runner-sub-wf :execute-mode :idle-w-timeout :t 10)

        _SCRAPE_ (scraping-sub-wf *WF-STATE *WF-UI)
        API (get _SCRAPE_ :api [])

        _UI_ (ui-sub-wf *WF-UI API)
        _API_ (api-wf/actions-impl! API api-wf/default-on-chord)

        ]

    {
     :init            (concat
                        (get _UI_  :init [])
                        (get _API_ :init [])
                        (get _SCRAPE_ :init [])
                        )

     :ctx             (concat
                        (get _UI_     :ctx [])
                        (get _API_    :ctx [])
                        (get _SCRAPE_ :ctx [])
                        )

     :steps           (concat
                        (get _UI_  :steps [])
                        (get _API_ :steps [])
                        (get _SCRAPE_ :steps [])
                        )

     :opts            (concat
                        (get _UI_  :opts [])
                        (get _API_ :opts [])
                        (get _SCRAPE_ :opts [])
                        ;(get _RUN_ :opts [])
                        )

     ;; expose some wf API
     :api             [["hello" (fn [])]] ;; API

     :on-stop         (fn [state]
                        ; (__log "ON STOP")
                        (clean-up-dom-side-effects)

                        ;; for now do not return channel
                        nil)

     ;; for now provide custom on-run handler - as older wf APIs are a map
     :scraper/on-run! (partial rum-wf/_on-run! *WF-UI)
     }
    )
  )