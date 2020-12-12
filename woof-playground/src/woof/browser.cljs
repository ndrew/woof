(ns ^:figwheel-hooks woof.browser
  (:require
    [cljs.core.async :refer [go] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]

    [goog.dom :as dom]

    [rum.core :as rum]

    [woof.base :as base]
    [woof.client.dom :as woof-dom]
    [woof.client.dbg :as dbg :refer [__log]]
    [woof.data :as d]
    [woof.utils :as u]

    ;; scraper impl
    [woof.client.browser.impl :as impl]
    [woof.client.browser.scraper.generic :as default-scraper]

    ;; common wf
    [woof.wfs.evt-loop :as evt-loop]
    [woof.client.ws :as ws]

    ;; ui
    [woof.client.browser.scraper.session :as ss]
    [woof.client.browser.scraper.scraping-ui :as sui]
    [woof.client.common :as common]

    ))

;;
;; ns for doing in-browser scraping

;; for now, build as :browser-min and inject in the host page as
;   (function() { var $script = document.createElement('script'); $script.setAttribute("type","text/javascript"); $script.setAttribute("src", "http://localhost:9500/cljs-out/browser-main.js"); document.body.appendChild($script); })()
;; or copy page contents into browser.html

(enable-console-print!)

;; whether to run wf automatically, or run via separate button
(defonce AUTO-START-WF?
         (and
           ;; do not start the browser workflow if we
            ;; are in playground
            (not (goog.object/get js/window "PLAYGROUND"))
            ;; todo: can we know here the params that were passed from clojure deps cmd args, like --auto-run-browser-wf
            ;(seq (has-cli-arg? "--auto-run-browser-wf"))
            true
           ))


;; state

(defonce chan-factory (base/chan-factory (atom {})))

;; shared step handlers for all scraping workflows



(defn browser-ctx [params]
  {
   :copy-to-clipboard   {:fn woof-dom/copy-to-clipboard-handler}
   }
  )

;; ws

(defn init-ws-fn [params]
    ;; injects: :ws/summary-chan - for returning summary via ws
    ;;          :ws/chan-fn
    ;;          :ws/gen-msg-handler
    (let [chan-factory (base/&chan-factory params)
          summary-chan (base/make-chan chan-factory (base/rand-sid))

          msg-fn     (partial ss/_get-summary-msg-fn summary-chan)
          ws-init-fn (partial ws/_ws-init-fn msg-fn)]

      (merge
        {:ws/summary-chan summary-chan}
        (ws-init-fn params))
      )

    )


;; opts


;; wf storage
(defonce *wf-instance
         (atom {
                :wf/instance nil

                }))

(defonce *initialized (volatile! false))

(defn &display-results-fn [params] (get params :wf/display-results-fn identity))

(defn common-opts[params]
  {

   ;; do we need to limit workflow output
   ;;:execute  (partial base/_timed-execute-fn 100)

   :op-handlers-map {
                     :done  (fn [result]
                              ;; todo: nicer display of results
                              ;; (.log js/console "WF DONE: " result)

                              ;; handle wf results if needed
                              (let [wf-done (&display-results-fn params)]
                                   (wf-done result))

                              (if (:ui? params)
                                  (sui/indicate-wf-done))

                              result
                              )

                     :error (fn [result]
                              (if (:ui? params)
                                (sui/indicate-wf-error))

                              (.error js/console result)

                              result)
                     }

   })



;;;
;; "http://localhost:9500/css/apt.css"

;; keep track of injected styles
(defonce *styles-added-map (atom {}))

(defn _add-style-once-steps-fn [css-url params]   ;; add css styles only once
  (if-not (get @*styles-added-map css-url)
    (do
      (swap! *styles-added-map assoc css-url true)
      { (base/rand-sid "CSS-") [:css-file css-url]})
    {}))




(defn scraper-wf!
  "boilerplate for defining scraping workflow:
  * wf - state atom for wf
  * meta-info - configuration map for wf
  * scraper-fn - sub workflow impl"
  [
   *wf
   meta-info
   scraper-fn]

  ;; pros:
  ;; - can reduce amount of boiler-plate - no need to include init/ctx/steps in each subsequent wf

  ;; cons:
  ;; - not explicit config - like whether there is a scraping ui

  (.groupCollapsed js/console "__WF")


  (let [{UI? :ui?
         EVT-LOOP? :evt-loop?
         WS? :ws?
         DEBUG? :debug?} meta-info

        ;; inner WF
        scraper-impl-map (scraper-fn *wf meta-info)

        *wf-instance (rum/cursor-in *wf [:wf/instance])

        ;; INIT
        ;;
        init* (concat
                [;; mandatory inits
                 (fn [params]                                   ;; pass configuration
                   (when UI? (sui/indicate-wf-started)) ;; UI: indicate WF has started
                   meta-info)

                 (base/build-init-chan-factory-fn chan-factory) ;; chan factory

                 ;; todo: what happens to *wf-instance
                 ;;(base/build-init-state-fn *wf-storage)          ;; state
                 (base/build-init-state-fn *wf)           ;; state
                 ]

                ;; evt loop
                (if EVT-LOOP? [(evt-loop/build-evt-loop-init-fn (base/make-chan chan-factory (base/rand-sid "evt-")))]
                              [])

                ;; ws
                (if WS? [init-ws-fn]
                        [])

                ;;
                (get scraper-impl-map :init [])

                ;; to store init params in state
                [(fn [last-params]
                   ;;
                   (swap! *wf assoc :WF/params last-params)
                   {}
                  )])

        ;; CTX
        ;;
        ctx* (concat
               [
                common/common-ctx
                browser-ctx
                woof-dom/dom-ctx
                ]

               (if WS? [ws/ws-ctx-fn]
                       [])

               (if EVT-LOOP? [evt-loop/evt-loop-ctx-fn]
                             [])

               (get scraper-impl-map :ctx [])
               )

        opts* (concat [
                       common-opts

                       (base/build-opts-chan-factory-fn chan-factory)

                       (base/build-opt-save-wf-fn *wf-instance)
                       ]
                      (get scraper-impl-map :opts [])
                      )

        steps* (concat (if EVT-LOOP?
                         [(fn [params]
                            {:evt/loop [:evt-loop (evt-loop/&evt-loop params)]})]
                         [])

                       (get scraper-impl-map :steps []))


        wf-impl (base/wf!
                  :init init*
                  :ctx ctx*
                  :opts opts*

                  :steps steps*
                  :wf-impl (if DEBUG?
                             (dbg/dbg-wf) ;; use capturing wf to keep the initial stuff
                             base/_default-wf-impl)

                  )

        api     (get scraper-impl-map :api {})
        on-stop (get scraper-impl-map :on-stop (fn [_] ))

        run-fn! (fn []
                  ;; update sub-atom
                  (let [wf-fn (fn [prev-state]
                                (when UI?
                                  (sui/wf-indicator-ui!))

                                (when (seq api)
                                  (sui/wf-api-ui!
                                    (if EVT-LOOP?
                                      ;; TODO: do not show this if the WF had been already ended
                                      (assoc api
                                        "WF: stop!" (fn []
                                                      ;; todo: maybe do this by getting :stop-fn from *wf-instance ?
                                                      (js* "woof.browser.stop_workflow();")
                                                      ))
                                      api))
                                  )

                                ;; show scraping ui by default?
                                (if (and UI?
                                         ; (get meta-info :ws? false)
                                         )
                                  (sui/scraping-ui-impl! meta-info))

                                (__log "🚀" (if prev-state "re-starting" "starting") " scraping wf!")

                                #_(if prev-state
                                    (__log prev-state))

                                (base/stateful-wf *wf-instance wf-impl
                                                  ;; do we need to pass api here?
                                                  :api api)
                                )]
                    (base/auto-run-wf! *wf-instance wf-fn
                                       :on-stop (fn [state]
                                                  ;; todo: handle channel
                                                  (on-stop state)
                                                  (dbg/__log-end)
                                                  ))
                    )
                  )
        ]

    (if AUTO-START-WF?
      (run-fn!)
      (let [btn-el (dom/createDom "button" "" "run!")]
        ;; todo: expose api actions and show them in ui
        (goog.events.listen btn-el goog.events.EventType.CLICK run-fn!)
        (woof-dom/ui-add-el! btn-el)
        ))

    (.groupEnd js/console)

    (dbg/__log-start "SCRAPING WF")
    )
  )




;;
;; export - to be able to run workflow from dev tools
(defn ^:export run_workflow []

  (.log js/console "RUNNING WORKFLOW" (u/now))
  (let [url (.. js/document -location -href)

        ;; todo: should meta info config be here or inside wf should decide
        META-INFO {
                   :ws? false
                   :evt-loop? true
                   :ui? true
                   :use-generic-wf? true
                   :debug? false
                   }
        ]

    (if (:ui? META-INFO)
      (woof-dom/<scraping-ui>) )

    ;; map localhost to a specific wf implementation
    (if-let [w (impl/choose-workflow url)]
      ;; if there is a wf assigned for the URL - run it
      (scraper-wf! *wf-instance
                   META-INFO w)

      (if (:use-generic-wf? META-INFO)
        ;; run generic scraping wf if needed
        (scraper-wf! *wf-instance
                     META-INFO default-scraper/wf!)

        ;; no scraper for this URL - show error
        (if (:ui? META-INFO)
          ;; or displaying an error text
          (let [el (dom/createDom "h3" "" (str "can't find scraping wf for URL: " url))]
            (woof-dom/ui-add-el! el))
          (.warn js/console (str "can't find scraping wf for URL: " url)))
        )
      )
    )
  )



;; export the function to stop the workflow
(defn ^:export stop_workflow []
  (let [wf-state @*wf-instance]
    (when-let [wf-instance (:wf/instance wf-state)]
      (let [stop (:stop-wf! wf-instance)]
        (stop))
      )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; side-effects, start wf automatically if there is certain var on the page
;;


;; CASE 1: auto-start of browser workflow in BROWSER_PLAYGROUND

(when AUTO-START-WF?

  ;; start wf automatically - if we are in browser playground
  (when (and (goog.object/get js/window "BROWSER_PLAYGROUND")
             (not @*initialized))
    (vswap! *initialized not)
    (dbg/__log "browser WF: starting (BROWSER_PLAYGROUND=true)")
    (run_workflow))

  ;; CASE 2: auto-start from extension or after manual script load
  ;;
  ;; run wf - if we are auto-scraping

  (when (and (not (goog.object/get js/window "BROWSER_PLAYGROUND"))
             AUTO-START-WF?)
    (.requestIdleCallback js/window
                          (fn []
                            (when-not @*initialized
                              (dbg/__log "browser WF: starting (BROWSER_PLAYGROUND=false)")
                              (vswap! *initialized not)
                              (run_workflow)
                              ))
                          )
    )
  )


;;
;; CASE 3: figwheel reload
(defn ^:after-load on-js-reload []

  ;; todo: handle re-load from other ns
  (when AUTO-START-WF?
    (dbg/__log "browser wf: re-starting due to JS reload")
    (run_workflow)
    )
  )
