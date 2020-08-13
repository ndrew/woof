(ns ^:figwheel-hooks woof.browser
  (:require

    [woof.base :as base]
    [woof.client.dom :as woof-dom]
    [woof.client.dbg :as dbg :refer [__log]]
    [woof.data :as d]
    [woof.utils :as u]

    [goog.dom :as dom]


    [woof.client.browser.scraper.ws :as scraping-test-ws]

    ;; auto.ria.com
    [woof.client.browser.autoria.scraper :as autoria-scraper]
    ;; domik
    [woof.client.browser.domik.scraper :as domik-scraper]
    ;; lun
    [woof.client.browser.lun.scraper :as lun-scraper]
    ;; blagovist.ua
    [woof.client.browser.blago.scraper :as blago-scraper]


    ;; common wf

    [woof.wfs.evt-loop :as evt-loop]


    [woof.client.ws :as ws]

    [cljs.core.async :refer [go] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]
    ))


;; ns for doing in-browser scraping

;; for now, build as :browser-min and inject in the host page as
;   (function() { var $script = document.createElement('script'); $script.setAttribute("type","text/javascript"); $script.setAttribute("src", "http://localhost:9500/cljs-out/browser-main.js"); document.body.appendChild($script); })()
;; or copy page contents into browser.html

(enable-console-print!)


;; whether to run wf automatically, or display run button
(defonce AUTO-START-WF? true)




;; state

(defonce chan-factory (base/chan-factory (atom {})))


;; common step handlers


(defn common-ctx [params]
  {
   :log     {:fn (fn[v] (.log js/console v) true)}
   :&log     {:fn (fn[v] (.log js/console v) true)
              :collect? true
              }

   :prn     {:fn (fn[v] (prn v) "")}

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


   :collect  {
              :fn       (fn [xs]
                          ; (.warn js/console xs)
                          xs)
              :collect? true
              }

   }
  )

(defn copy-to-clipboard [v]
  (when js/navigator.clipboard.writeText
    (let [clipboard js/navigator.clipboard

          copy-handler (fn []
                         (-> (.writeText clipboard (d/pretty! v))
                             (.then (fn [response] (.log js/console "Copied to clipboard - " response))
                                    (fn [err]      (.warn js/console "Failed to copy to clipboard" err))))
                         )
          ]

      (let [btn-el (dom/createDom "button" ""
                                  "copy results to clipboard")]

        (goog.events.listen btn-el goog.events.EventType.CLICK copy-handler)
        (woof-dom/ui-add-el! btn-el)

        (.focus btn-el)
        )
      )
    )

  )

(defn browser-ctx [params]
  {
   :copy-to-clipboard   {:fn copy-to-clipboard}

   :ui-progress {
                 :fn (fn [v]
                       ;; todo: use value
                       (let [el (dom/createDom "div" ""
                                               "READY!")]


                            (woof-dom/ui-add-el! el)
                            )
                       )
                 }
   }
  )


(defn &display-results-fn [params] (get params :wf/display-results-fn identity))

(defonce *running-wf (atom nil))

(defn common-opts[params]
  {
   :before-process  (fn [wf-chan xtor]   ;; swf-bp-store-xtor
                      (reset! *running-wf xtor)
                      ;; (swap! *state assoc-in [:runtime :xtor] xtor)
                      :ok)

   :op-handlers-map {
                     :done  (fn [result]
                              (.log js/console "WF DONE: " result)

                              ;; handle wf results if needed
                              (let [wf-done (&display-results-fn params)]
                                   (wf-done result))

                              )

                     :error (fn [result]
                              (.error js/console result))

                     }

   })



;; adds a workflow ui panel
(defn run-wf! [wf-impl]
  (let [run-fn! (fn []
                  (__log "🚀 starting scraping wf!")
                  (base/run-wf! wf-impl))]

    (if AUTO-START-WF?
      (run-fn!)
      (let [btn-el (dom/createDom "button" "" "run!")]
        (goog.events.listen btn-el goog.events.EventType.CLICK run-fn!)
        (woof-dom/ui-add-el! btn-el)
        ))
    )
  )



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


(defn domik-scraping! []

  ;; pass configuration to the workflow
  (let [meta-init-fn (fn [params]
                       {

                        :ws? false
                        :ws/skip-processed? false

                        ;; on-done
                        :wf/display-results-fn (fn [wf-results]

                                                 (let [listings (get wf-results :domik/LISTINGS)]

                                                      ; todo: handle listings here if needed
                                                      ;; (.clear js/console)
                                                      ;; (.log js/console listings)

                                                      )

                                                 )
                        })]

    (run-wf!
      (base/wf!
        :init [meta-init-fn
               domik-scraper/scraper-init]
        :ctx [woof-dom/dom-ctx
              domik-scraper/common-ctx
              domik-scraper/scraper-ctx]
        :steps [domik-scraper/scraper-steps]
        :opts [domik-scraper/common-opt]
        )
      )
    )

  )


(defn blagovist-scraping! []

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
                 blago-scraper/scraper-init
                 ]
          :ctx [common-ctx
                browser-ctx
                woof-dom/dom-ctx
                ws/ws-ctx-fn

                blago-scraper/scraper-ctx
                ]
          :opts [common-opts]

          :steps [blago-scraper/scraper-steps]

          ;; think better name
          :wf-impl (dbg/dbg-wf)
          )
        ]


    (.log js/console

          (run-wf!
            wf-impl
            ))
    ;; how to send message via ws

    ;; (.clear js/console)
    #_(run-wf!
      wf-impl
      )
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


;; return only channel, :ws/gen-msg-handler should be
(defn _ws-init [process-ws-msg params]
  (let [chan-factory (base/&chan-factory params)]
    {
     :ws/chan-fn (fn []
                   (let [ws-chan (base/make-chan chan-factory
                                                 (base/rand-sid "ws-"))]
                        ws-chan))

     ;; disallow to use gen handler
     ;:ws/gen-msg-handler (fn []
     ;                      (u/throw! "scraping wf needs to specify it's own :ws/gen-msg-handler")
     ;                      )
     :ws/gen-msg-handler (fn []
                           (fn [msg-envelope]
                             (.log js/console (d/pretty! msg-envelope))

                             (try
                               (process-ws-msg params msg-envelope)
                               (catch js/Error e
                                 (.error js/console ":ws/gen-msg-handler error:" e)
                                 )
                               )
                             )
                           )

     ;; what is a good way of sending message to socket
     ;; via separate channel
     ;; or via socket directly

     ;                  :ws/msg-handler (fn [msg]
     ;                                    (.log js/console "[WS]" msg))

     }
  )
  )



;; the example of workflow that scrapes data from web page and stores them in the scraping session
(defn scrapping-test-wf! []

  ;; pass configuration to the workflow
  (let [meta-init-fn (fn [params]
                       {
                        :ws? true                 ;
                        ;; custom on-done
                        :wf/display-results-fn (fn [wf-results]
                                                 (.log js/console wf-results))
                        })

        *internal-state (atom {})

        wf-impl (base/wf!
                  :init [(base/build-init-chan-factory-fn chan-factory)
                         (evt-loop/build-evt-loop-init-fn (base/make-chan chan-factory (base/rand-sid "evt-")))

                         (base/build-init-state-fn *internal-state)

                         meta-init-fn

                         (partial _ws-init scraping-test-ws/process-ws-msg)]

                  :ctx [
                        common-ctx
                        browser-ctx
                        woof-dom/dom-ctx

                        ws/ws-ctx-fn
                        evt-loop/evt-loop-ctx-fn

                        scraping-test-ws/scraper-ctx
                        ]
                  :opts [
                         common-opts
                         (base/build-opts-chan-factory-fn chan-factory)
                         ]

                  :steps [
                          ;; for now focus on simpler parsing
                          ;; scraping-test-ws/scraper-steps

                          scraping-test-ws/scraper-steps-parsing-only
                          ]

                  ;; think better name
                  :wf-impl (dbg/dbg-wf)
                  )
        ]

    (run-wf! wf-impl)
    )
  )


;; export runner workflow to be accessible from console dev tools

(defn ^:export run_workflow []
  (let [url (.. js/document -location -href)]
    (woof-dom/<scraping-ui>)

    (cond
      ;; map localhost to a specific wf
      (clojure.string/starts-with? url "http://localhost")        (scrapping-test-wf!)

      ;; dispatch url to a corresponding scraper
      (clojure.string/starts-with? url "https://auto.ria.com")    (autoria-sraping!)
      (clojure.string/starts-with? url "https://blagovist.ua")    (blagovist-scraping!)
      (clojure.string/starts-with? url "http://domik.ua/")        (domik-scraping!)
      (clojure.string/starts-with? url "https://lun.ua/")         (lun-scraping!)

      :else (do
              (let [el (dom/createDom "h3" ""
                                      (str "can't find scraping wf for URL: " url))]

                (woof-dom/ui-add-el! el)
                ))
      )

    )

  )

;; export the function to stop the workflow
(defn ^:export stop_workflow []
  (when-let [xtor @*running-wf]

    (let [end-chan (base/end! xtor)]
      (.log js/console "browser wf: Stopping WF" end-chan)))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; side-effects, start wf automatically if there is certain var on the page
;;

;; start wf automatically - if we are in browser playground
(when (goog.object/get js/window "BROWSER_PLAYGROUND")
  (dbg/__log-start)
  ;(dbg/__log-once "auto-starting browser workflow")
  (run_workflow))



;; run wf - if we are auto-scraping
(defonce *initialized (volatile! false))

(when-not (goog.object/get js/window "BROWSER_PLAYGROUND")
  (.requestIdleCallback js/window
                        (fn []
                          (when-not @*initialized
                                    ;(dbg/__log-once "auto-starting browser workflow")
                                    (vswap! *initialized not)
                                    (run_workflow)
                                    )

                          )
                        )
  )


(defn ^:after-load on-js-reload []
  (dbg/__log "browser wf: JS RELOAD"))
