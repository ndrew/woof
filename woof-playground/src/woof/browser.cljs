(ns ^:figwheel-hooks ^:figwheel-always
  woof.browser
  (:require
    [woof.base :as base]
    ;[woof.client.browser.scraper :as scraper]
    [woof.client.browser.dom :as dom]
    [woof.client.browser.scraper2 :as scraper2]
    ))


;; ns for doing in-browser scraping

;; for now, build as :browser-min and inject in the host page as
;   (function() { var $script = document.createElement('script'); $script.setAttribute("type","text/javascript"); $script.setAttribute("src", "http://localhost:9500/cljs-out/browser-main.js"); document.body.appendChild($script); })()

;; or copy page contents into browser.html


(enable-console-print!)




;;
;; configure your browser wf here
;;

(def init-fns   [])  ;; [scraper/scraper-init]
(def ctx-fns    [dom/dom-ctx scraper2/ctx-fn])  ;; [dom/dom-ctx scraper/common-ctx scraper/scraper-ctx scraper/ws-ctx-fn]
(def steps-fns  [scraper2/steps-fn])  ;; [scraper/scraper-steps]
(def opt-fns    [scraper2/opt-fn])  ;; [scraper/common-opt]


(defn ^:export run_workflow []
  ;; this will start the wf
  (let [wf-impl
        (base/parametrized-wf!
          (base/combine-init-fns init-fns)
          identity ; wf-params-fn
          identity ; opt-params-fn
          (base/combine-fns opt-fns :merge-results base/merge-opts-maps)
          (base/combine-fns ctx-fns)
          (base/combine-fns steps-fns))
        ]
    (base/run-wf! wf-impl identity)
    )
  )



;; for now - auto-run wf, if we are not in the playground
(when-not (goog.object/get js/window "PLAYGROUND")

  ; (.clear js/console)
  (run_workflow)

  )
