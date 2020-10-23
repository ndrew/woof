(ns ^:figwheel-hooks woof.browser
  (:require

    [woof.base :as base]
    [woof.client.dom :as woof-dom]
    [woof.client.dbg :as dbg :refer [__log]]
    [woof.data :as d]
    [woof.utils :as u]

    [goog.dom :as dom]

    [woof.client.browser.impl :as impl]


    ;; auto.ria.com
    [woof.client.browser.autoria.scraper :as autoria-scraper]
    ;; domik
    [woof.client.browser.domik.scraper :as domik-scraper]
    ;; lun
    [woof.client.browser.lun.scraper :as lun-scraper]

    [woof.client.browser.scraper.generic :as default-scraper]



    ;;
    [woof.client.browser.scraper.streets :as streets]
    [woof.client.browser.scraper.street-renamings :as street-rename]

    ;; common wf

    [woof.wfs.evt-loop :as evt-loop]

    [woof.client.ws :as ws]
    [woof.client.browser.scraper.session :as ss]
    [woof.client.browser.scraper.scraping-ui :as sui]

    [cljs.core.async :refer [go] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]
    ))


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
           ;;true
           ))


;; configure scraping workflow
(def META-INFO {
                :ws? false
                :evt-loop? true
                :ui? true
                })


;; state

(defonce chan-factory (base/chan-factory (atom {})))


;; shared step handlers for all scraping workflows

;; common step handlers
(defn common-ctx [params]
  {
   :log     {:fn (fn[v] (.log js/console v) true)}
   :&log     {:fn (fn[v] (.log js/console v) true)
              :collect? true
              }

   :prn     {:fn (fn[v] (.log js/console (d/pretty! v)) "")}
   :*prn     {:fn (fn[v] (.log js/console (d/pretty! v)) "")
              :collect? true
              }
   :prn-seq{:fn (fn[vs]
                   (doseq [v vs]
                     (.log js/console (d/pretty! v))
                     )
                    "")
              :collect? true
              }

   :wait-rest      {
                    :fn       (fn [[v & rest]] v)
                    :collect? true
                    }

   :wait-steps {
                :fn (fn [[steps & rest]]
                      steps
                      )
                :collect? true
                :expands? true
                }

   ;;
   ;; mem
   :mem-k*             {
                        :fn       (fn [o]
                                    {(base/rand-sid "mem-k-") [:identity {:k o}]})
                        :expands? true
                        }

   :mem-zip {:fn       (fn [xs]
                         (partition-all (count xs)
                                        (apply interleave
                                               (reduce (fn [col [a]]
                                                         (conj col (:k a)))
                                                       [] xs))))
             :collect? true
             }

   :mem-zip* {
              :fn       (fn [xs]

                          ;; todo: what if sub-sid-list are of different length
                          (let [grouped-sids (partition-all (count xs)
                                                            (apply interleave
                                                                   (reduce (fn [col [a]]
                                                                             (conj col (:k a)))
                                                                           [] xs)))]
                            (reduce
                              (fn [a x]
                                (assoc a (base/rand-sid "mem-zip-") [:collect x])
                                )
                              {} grouped-sids)
                            )

                          )
              :expands? true
              :collect? true
              }

   ;; kv zipping - joins keys with values
   :*kv-zip            {
                        :fn       (fn [[[k] vs]]
                                    (let [ks (:k k)]
                                         (apply assoc {} (interleave ks vs))
                                         ))
                        :collect? true
                        }

   :identity {:fn identity }
   :identity*    (base/expand-into :identity)
   :v {:fn identity }
   :v-8 {:fn identity
         :infinite true
         }
   :v* (base/expand-into :v)



   :collect  {
              :fn       (fn [xs]
                          ; (.warn js/console xs)
                          xs)
              :collect? true
              }


   :save-to-state {
                   :fn (fn [[k v]]
                         (let [*state (base/&state params)]
                           (swap! *state assoc k v)
                           ))
                   :collect? true
                   }

   :tick {:fn       (fn [[t max-num]]
                      (let [chan-factory (base/&chan-factory params)
                            chan (base/make-chan chan-factory (base/rand-sid))]

                        (async/go-loop [i 0]
                                       (async/>! chan (u/now))
                                       (async/<! (u/timeout t))

                                       (if (< i max-num)
                                         (recur (inc i))))

                        chan))
          :infinite true}

   }
  )


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


;; todo: migrate to use *wf-instance
(defonce *running-wf-xtor (atom nil))

(defonce *wf-instance (atom nil))
(defonce *initialized (volatile! false))


(defn &display-results-fn [params] (get params :wf/display-results-fn identity))

(defn common-opts[params]
  {
   :before-process  (fn [wf-chan xtor]   ;; swf-bp-store-xtor
                      ;; is this actually needed? as use state workflows?
                      (reset! *running-wf-xtor xtor)
                      ;; (swap! *state assoc-in [:runtime :xtor] xtor)
                      :ok)

   ;; do we need to limit workflow output
   ;;:execute  (partial base/_timed-execute-fn 100)


   :op-handlers-map {
                     :done  (fn [result]
                              ;; todo: nicer display of results
                              ;; (.log js/console "WF DONE: " result)

                              ;; handle wf results if needed
                              (let [wf-done (&display-results-fn params)]
                                   (wf-done result))

                              (sui/indicate-wf-done)
                              ;; (__log "WF DONE")

                              result
                              )

                     :error (fn [result]
                              (sui/indicate-wf-error)

                              (.error js/console result)
                              result)

                     }

   })


;; fixme: deprecate this
(defn run-wf! [wf-impl & {:keys [api on-stop meta-info] :or {
                                                             api     {}
                                                             on-stop (fn [state] (__log "default on-stop"))
                                                             meta-info {
                                                                        :ws? false
                                                                        }
                                                             }}]
  ;; always add scraping UI

  (let [run-fn! (fn []
                  (base/auto-run-wf! *wf-instance
                                     (fn [prev-state]
                                       ;;
                                       ;; init the woof custom scraping panel
                                       (woof-dom/<scraping-ui>)

                                       (sui/wf-indicator-ui!)

                                       (when (seq api)


                                         (sui/wf-api-ui!
                                           (if (get meta-info :evt-loop? false)
                                             ;; TODO: do not show this if the WF had been already ended
                                             (assoc api
                                               "WF: stop!" (fn []
                                                             ;; todo: maybe do this by getting :stop-fn from *wf-instance ?
                                                            (js* "woof.browser.stop_workflow();")
                                                            ))
                                             api))
                                         )

                                       (if (get meta-info :ws? false)
                                         ;; show scraping ui by default?
                                         (sui/scraping-ui-impl! meta-info))

                                       (__log "🚀" (if prev-state "re-starting" "starting") " scraping wf!")

                                       #_(if prev-state
                                         (__log prev-state))

                                       (base/stateful-wf *wf-instance wf-impl :api api)
                                       )
                                     :on-stop on-stop
                                     )
                  )

        ]


    (.groupCollapsed js/console "WF")

    ;; todo: handle reload via extension
    (if AUTO-START-WF?
      (run-fn!)
      (let [btn-el (dom/createDom "button" "" "run!")]
        ;; todo: expose api actions and show them in um
        (goog.events.listen btn-el goog.events.EventType.CLICK run-fn!)
        (woof-dom/ui-add-el! btn-el)
        ))

    (.groupEnd js/console)


    )
  )


;;;;;;;;;;;;;;;;;;;
;;
;; scrapers

(defn lun-scraping! []
  ;; todo: use meta workflow
  (run-wf!
    (base/wf!
      :init []
      :ctx [common-ctx
            browser-ctx
            woof-dom/dom-ctx
            lun-scraper/ctx-fn]
      :steps [lun-scraper/steps-fn]
      :opts [lun-scraper/opt-fn]
      )
    )
  )

(defn domik-scraping! [url]

  ;; todo: rework domik scraping similar to scraper/ws workflow
  ;; pass configuration to the workflow
  (let [*internal-state (atom {})

        wf-impl (base/wf!
                  :init [(base/build-init-chan-factory-fn chan-factory)
                         (evt-loop/build-evt-loop-init-fn (base/make-chan chan-factory (base/rand-sid "evt-")))

                         (base/build-init-state-fn *internal-state)

                         domik-scraper/meta-init-fn
                         domik-scraper/scraper-init

                         ]

                  :ctx [
                        common-ctx
                        browser-ctx
                        woof-dom/dom-ctx

                        ws/ws-ctx-fn
                        evt-loop/evt-loop-ctx-fn

                        domik-scraper/scraper-ctx

                        ]
                  :opts [
                         common-opts
                         (base/build-opts-chan-factory-fn chan-factory)
                         ]

                  :steps [

                          (cond
                            (= url "http://localhost:9500/domik.html") domik-scraper/parse-listings-steps
                            (= url "http://localhost:9500/domik_house.html") domik-scraper/parse-house-steps

                            (clojure.string/starts-with? url "http://domik.ua/poleznoe/photoalbum/")  domik-scraper/parse-house-steps
                            (clojure.string/starts-with? url "http://domik.ua/nedvizhimost/") domik-scraper/parse-listings-steps



                            :else {
                                   ::err [:log (str "unknown URL: " url)]
                                   }
                            )


                          ]

                  ;; think better name
                  :wf-impl (dbg/dbg-wf)
                  )
        ]

    ;; todo: maybe choose whether to run wf from workflow itself?

    (run-wf! wf-impl)
    )
  )


(defn autoria-sraping! []
  ;; pass configuration to the workflow
  (let [WS? true
        meta-init-fn (fn [params]
                       {

                        ;; meta params
                        :ws? WS?

                        :ws/skip-processed? false

                        ;; on-done
                        :wf/display-results-fn (fn [wf-results]
                                                 (.log js/console wf-results)
                                                 )
                        })

        *internal-state (atom {})

        wf-impl (base/wf!
                  :init [(base/build-init-chan-factory-fn chan-factory)
                         (base/build-init-state-fn *internal-state)
                         meta-init-fn
                         autoria-scraper/evt-loop-init
                         autoria-scraper/scraper-init
                         ]
                  :ctx [common-ctx
                        browser-ctx
                        woof-dom/dom-ctx
                        ws/ws-ctx-fn

                        autoria-scraper/scraper-ctx
                        ]
                  :opts [common-opts]

                  :steps [autoria-scraper/scraper-steps]

                  ;; think better name
                  :wf-impl (dbg/dbg-wf)
                  )
        ]

    (run-wf! wf-impl)
    ))


;; todo: meta-init
  ;; should be a generic one for all scraping workflows? or per wf?




(defn scraper-wf!
  "boilerplate for defining scraping workflow"
  [*wf-state meta-info scraper-fn]
  (let [meta-init-fn (fn [params]
                       (if (:ui? meta-info)
                         ;; use this as starting point
                         (sui/indicate-wf-started))

                       meta-info)

        evt-loop? (get meta-info :evt-loop?)
        ws? (get meta-info :ws?)

        scraper-impl-map (scraper-fn *wf-state meta-info)

        init* (concat
                [;; mandatory inits
                 ;; configuration
                 meta-init-fn
                 ;; chan factory
                 (base/build-init-chan-factory-fn chan-factory)
                 ;; state
                 (base/build-init-state-fn *wf-state)]

                (if evt-loop?
                  [(evt-loop/build-evt-loop-init-fn (base/make-chan chan-factory (base/rand-sid "evt-")))]
                  [])

                (if ws?
                  [init-ws-fn]
                  [])

                (get scraper-impl-map :init [])

                [(fn [last-params]
                  (swap! *wf-state assoc :WF/params last-params)
                  {}
                  )]
                )

        ctx* (concat
               [
                common-ctx
                browser-ctx
                woof-dom/dom-ctx
                ]
               (if ws?
                 [ws/ws-ctx-fn] [])

               (if evt-loop?
                 [evt-loop/evt-loop-ctx-fn] [])

               (get scraper-impl-map :ctx [])
               )

        opts* (concat [
                       common-opts
                       (base/build-opts-chan-factory-fn chan-factory)
                       (base/build-opt-state-fn *wf-state)
                       ]
                      (get scraper-impl-map :opts []))

        steps* (concat (if evt-loop?
                         [(fn [params]
                            {:evt/loop [:evt-loop (evt-loop/&evt-loop params)]})]
                         []
                         )

                       (get scraper-impl-map :steps [])
                       )

        wf-impl (base/wf!
                  :init init*
                  :ctx ctx*
                  :opts opts*

                  :steps steps*

                  ;; think better name
                  :wf-impl (dbg/dbg-wf)
                  )
        ]

    (run-wf! wf-impl
             :api (get scraper-impl-map :api {})
             :on-stop (get scraper-impl-map :on-stop (fn [_]))
             :meta-info meta-info)
    )
  )




;; export runner workflow to be accessible from console dev tools
(defn ^:export run_workflow []
  (.warn js/console "RUNNING WORKFLOW" (u/now))

  (let [url (.. js/document -location -href)
        wf! (partial scraper-wf! *wf-instance META-INFO)
        ]

    (if (:ui? META-INFO)
      (woof-dom/<scraping-ui>))

    ;; map localhost to a specific wf implementation
    (if-let [w (impl/choose-workflow url)]
      (wf! w)
      (do
        (.log js/console "trying generic scraper workflow" url)
        (wf! default-scraper/wf!)

        ;; or displaying an error text
        #_(let [el (dom/createDom "h3" ""
                                  (str "can't find scraping wf for URL: " url))]
            (woof-dom/ui-add-el! el)
            )
        )
      )



    )

  )

;; export the function to stop the workflow
(defn ^:export stop_workflow []
  (when-let [xtor @*running-wf-xtor]

    (let [end-chan (base/end! xtor)]
      (.log js/console "browser wf: Stopping WF" end-chan)))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; side-effects, start wf automatically if there is certain var on the page
;;


;; CASE 1: auto-start of browser workflow in BROWSER_PLAYGROUND

;; start wf automatically - if we are in browser playground
(when (and (goog.object/get js/window "BROWSER_PLAYGROUND")
           AUTO-START-WF?
           (not @*initialized))
  (vswap! *initialized not)
  (dbg/__log-start)
  ;(dbg/__log-once "auto-starting browser workflow")
  (run_workflow))


;; CASE 2: auto-start from extension or after manual script load

;; run wf - if we are auto-scraping

(when (and (not (goog.object/get js/window "BROWSER_PLAYGROUND"))
           AUTO-START-WF?)
  (.requestIdleCallback js/window
                        (fn []
                          (when-not @*initialized
                                    ;(dbg/__log-once "auto-starting browser workflow")
                                    (vswap! *initialized not)
                                    (run_workflow)
                                    ))
                        )
  )


;; CASE 3: figwheel reload
(defn ^:after-load on-js-reload []

  ;; handle re-load from other ns
  (when AUTO-START-WF?
    (dbg/__log "browser wf: JS RELOAD")
    (run_workflow)
    )
  )



;;;

(defonce *styles-added-map (atom {}))


;; "http://localhost:9500/css/apt.css"

(defn _add-style-once-steps-fn [css-url params]   ;; add css styles only once
  (if-not (get @*styles-added-map css-url)
    (do
      (swap! *styles-added-map assoc css-url true)
      { (base/rand-sid "CSS-") [:css-file css-url]})
    {}))