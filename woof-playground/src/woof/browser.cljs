(ns ^:figwheel-hooks woof.browser
  (:require

    [woof.base :as base]
    [woof.client.dom :as woof-dom]
    [woof.client.dbg :as dbg :refer [__log]]
    [woof.data :as d]
    [woof.utils :as u]

    [goog.dom :as dom]

    ;; todo: use ns per url
    ;; domik
    [woof.client.browser.scraper :as scraper]
    ;; lun
    [woof.client.browser.scraper2 :as scraper2]
    ;; blagovist.ua
    [woof.client.browser.scraper3 :as scraper3]

    [woof.client.ws :as ws]

    [cljs.core.async :refer [go] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]
    ))


;; whether to run wf automatically, or display run button
(defonce AUTO-START-WF? true)

;; ns for doing in-browser scraping

;; for now, build as :browser-min and inject in the host page as
;   (function() { var $script = document.createElement('script'); $script.setAttribute("type","text/javascript"); $script.setAttribute("src", "http://localhost:9500/cljs-out/browser-main.js"); document.body.appendChild($script); })()

;; or copy page contents into browser.html


(enable-console-print!)

;; state

(defonce chan-factory (base/chan-factory (atom {})))



;; common step handlers

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

(defn common-ctx [params]
  {
   :log     {:fn (fn[v] (.log js/console v) v)}
   :prn     {:fn (fn[v] (prn v) "")}

   :copy-to-clipboard   {:fn copy-to-clipboard}

   :wait-rest      {
                    :fn       (fn [[v & rest]] v)
                    :collect? true
                    }

   :ui-progress {
                 :fn (fn [v]
                       ;; todo: use value
                       (let [el (dom/createDom "div" ""
                                               "READY!")]


                            (woof-dom/ui-add-el! el)
                            )
                       )
                 }

   :mem-k*             {
                        :fn       (fn [o]
                                    {(base/rand-sid "mem-k-") [:identity {:k o}]})
                        :expands? true
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

   :collect  {
              :fn       (fn [xs]
                          ; (.warn js/console xs)
                          xs)
              :collect? true
              }


   }
  )

(defn &display-results-fn [params] (get params :wf/display-results-fn identity))

(defn common-opts[params]
  {
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
            woof-dom/dom-ctx
            scraper2/ctx-fn]
      :steps [scraper2/steps-fn]
      :opts [scraper2/opt-fn]
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
               scraper/scraper-init]
        :ctx [woof-dom/dom-ctx
              scraper/common-ctx
              scraper/scraper-ctx]
        :steps [scraper/scraper-steps]
        :opts [scraper/common-opt]
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
                        })]


    ;; (.clear js/console)
    (run-wf!
      (base/wf!
        :init [(base/build-init-chan-factory-fn chan-factory)
               meta-init-fn
               ;; ws-init
               (fn [params]
                 {:ws/chan-fn (fn []
                                (base/make-chan (base/&chan-factory params)
                                                (base/rand-sid "ws-")))

                  :ws/msg-handler (fn [msg]
                                    (.log js/console "[WS]" msg))
                  }
                 )

               scraper3/scraper-init]
        :ctx [common-ctx
              woof-dom/dom-ctx
              ws/ws-ctx-fn

              scraper3/scraper-ctx
              ]
        :opts [common-opts]

        :steps [scraper3/scraper-steps

                (fn [params]
                  (.log js/console "WWWWWWWWWWWSSSSSSSSSSSSSSSS")
                  {
                   ::ws [:ws-socket "ws://localhost:8081/scraper-ws"]
                   }
                  )

                ]

        ;; think better name
        :wf-impl (dbg/dbg-wf)
        )
      )
    )
  )




(defn ^:export run_workflow []
  (let [url (.. js/document -location -href)]
    (woof-dom/<scraping-ui>)

    (cond
      ;; map localhost to a specific wf
      (clojure.string/starts-with? url "http://localhost")       (blagovist-scraping!)

      (clojure.string/starts-with? url "https://blagovist.ua")    (blagovist-scraping!)

      (clojure.string/starts-with? url "http://domik.ua/")        (domik-scraping!)

      ;; todo: check this
      (clojure.string/starts-with? url "https://lun.ua/")         (lun-scraping!)
      :else (do
              (let [el (dom/createDom "h3" ""
                                      (str "can't find scraping wf for URL: " url))]

                (woof-dom/ui-add-el! el)
                ))
      )

    )

  )

;;
;; side-effects, start wf automatically if there is certain var on the page


;; run wf - if we are in browser playground
(when (goog.object/get js/window "BROWSER_PLAYGROUND")
  (dbg/__log-start)
  (dbg/__log-once "auto-starting browser workflow")
  (run_workflow)
  )




;; run wf - if we are auto-scraping
(defonce *initialized (volatile! false))

(when-not (goog.object/get js/window "BROWSER_PLAYGROUND")
  (.requestIdleCallback js/window
                        (fn []
                          (when-not @*initialized
                                    (dbg/__log-once "auto-starting browser workflow")
                                    (vswap! *initialized not)
                                    (run_workflow)
                                    )

                          )
                        )
  )

(defn ^:after-load on-js-reload []
  (dbg/__log "JS RELOAD")
  )
