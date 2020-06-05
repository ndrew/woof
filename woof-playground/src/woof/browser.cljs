(ns ^:figwheel-hooks woof.browser
  (:require
    [woof.base :as base]

    [goog.dom :as gdom]
    [woof.client.dom :as dom]

    ;; todo: use ns per url

    [woof.client.browser.scraper :as scraper]
    [woof.client.browser.scraper2 :as scraper2]

    [cljs.core.async :refer [go] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]
    [woof.utils :as u]
    ))


;; whether to run wf automatically, or display run button
(defonce AUTO-START-WF? true)


;; ns for doing in-browser scraping

;; for now, build as :browser-min and inject in the host page as
;   (function() { var $script = document.createElement('script'); $script.setAttribute("type","text/javascript"); $script.setAttribute("src", "http://localhost:9500/cljs-out/browser-main.js"); document.body.appendChild($script); })()

;; or copy page contents into browser.html


(enable-console-print!)



;; adds a workflow ui panel
(defn run-wf! [wf-impl]
  (let [run-fn! (fn []
                  (.log js/console "RUNNING SCRAPING WF!")
                  (base/run-wf! wf-impl))]


    (if AUTO-START-WF?
      (run-fn!)
      (let [btn-el (gdom/createDom "button" ""
                               "run!")]

        (goog.events.listen btn-el goog.events.EventType.CLICK run-fn!)
        (dom/ui-add-el! btn-el)
        )
      )

    )
  )



(defn lun-scraping! []
  ;; todo: meta workflow
  (run-wf!
    (base/wf!
      :init []
      :ctx [dom/dom-ctx
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
        :ctx [dom/dom-ctx
              scraper/common-ctx
              scraper/scraper-ctx]
        :steps [scraper/scraper-steps]
        :opts [scraper/common-opt]
        )
      )
    )

  )



(defn ^:export run_workflow []

  (let [url (.. js/document -location -href)]
    (dom/<scraping-ui>)

    (cond

      ;; map localhost to a specific wf
      (clojure.string/starts-with? url "http://localhost:9500")   (domik-scraping!)

      (clojure.string/starts-with? url "http://domik.ua/")        (domik-scraping!)

      ;; todo: check this
      (clojure.string/starts-with? url "https://lun.ua/")         (lun-scraping!)
      :else (do
              (let [el (gdom/createDom "h3" ""
                                       (str "can't find scraping wf for URL: " url))]

                (dom/ui-add-el! el)
                )

              )
      )

    )

  )

;;
;; side-effects, start wf automatically if there is certain var on the page

(when (goog.object/get js/window "BROWSER_PLAYGROUND")
  (.log js/console "auto-starting browser workflow")
  (run_workflow)
  )
