(ns ^:figwheel-hooks woof.browser
  (:require
    [woof.base :as base]
    [woof.client.dom :as dom]
    ;[woof.client.browser.scraper :as scraper]
    [woof.client.browser.scraper2 :as scraper2]

    [cljs.core.async :refer [go] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]
    [woof.utils :as u]))


;; ns for doing in-browser scraping

;; for now, build as :browser-min and inject in the host page as
;   (function() { var $script = document.createElement('script'); $script.setAttribute("type","text/javascript"); $script.setAttribute("src", "http://localhost:9500/cljs-out/browser-main.js"); document.body.appendChild($script); })()

;; or copy page contents into browser.html



(enable-console-print!)


;;
;; configure your browser wf here
;;

;(def init-fns   [])  ;; [scraper/scraper-init]
;(def ctx-fns    [dom/dom-ctx scraper2/ctx-fn])  ;; [dom/dom-ctx scraper/common-ctx scraper/scraper-ctx scraper/ws-ctx-fn]
;(def steps-fns  [scraper2/steps-fn])  ;; [scraper/scraper-steps]
;(def opt-fns    [scraper2/opt-fn])  ;; [scraper/common-opt]





(defn convert [a]
  (if (.isArray js/Array a)
    (js->clj a)
    a)
  )

(defn ^:export run_js_workflow [
                                init-fns
                                ctx-fns
                                steps-fns
                                opt-fns
                                ]
  ;; this will start the wf
  (let [
        wf-impl (base/wf!
                  :init (convert init-fns)
                  :opts (convert opt-fns)
                  :ctx (convert ctx-fns)
                  :steps (convert steps-fns))
        ]
    (base/run-wf! wf-impl)
    )
  )



(defn ^:export run_workflow []

  (run_js_workflow
    []
    [dom/dom-ctx scraper2/ctx-fn]
    [scraper2/steps-fn]
    [scraper2/opt-fn]
    )
  )




(defn ^:export default_ctx_fn [params]
  {
   :identity {:fn identity}
   }
  )

(defn ^:export default_opt_fn [params]
  {
   :op-handlers-map {
                     :done  (fn [result]
                              (.groupCollapsed js/console "Workflow ended.")
                              (.log js/console
                                    "Full results map: " result)
                              (.groupEnd js/console)
                              )

                     :error (fn [result]
                              (.error js/console result))

                     }

   })


(defn ^:export k [s]
  (keyword s))


(defn ^:export qk
  ([k]
   (let [s (str k)
         [raw-ns n] (clojure.string/split s #"/")
         ns (if (clojure.string/starts-with? raw-ns ":")
              (if (clojure.string/starts-with? raw-ns "::")
                (subs raw-ns 2)
                (subs raw-ns 1))
              raw-ns)
         ]
     (if (nil? n)
       (keyword (namespace ::boo) ns)
       (keyword ns n)
       )
     )
   )
  ([ns k]
   (keyword ns k)
   ))

(defn ^:export kv []
  {})



;; wraps js function into a step fn
(defn ^:export steps_fn [js-fn]
  (fn [params]
    (let [js-res (js-fn (clj->js params))]

         ;; convert js result to
         (into {} (for [k (.keys js/Object js-res)]
                    (do
                      ;(js-debugger)

                      (let [nu-k (if (qualified-keyword? k) k (qk k))
                            raw-v (aget js-res k)
                            raw-step (aget raw-v 0)
                            step (if (keyword? raw-step) raw-step (k raw-step))
                            v (aget raw-v 1)
                            ]
                        [nu-k [step v]]
                        )

                      )

                    ))
         )))



;; !!obj && (typeof obj === 'object' || typeof obj === 'function') && typeof obj.then === 'function';

(defn ^:export promise2chan [v]
  (let [ch (async/chan)]
    (go
      (let [v (<p! v)]
        (async/put! ch (if (nil? v) :nil v)))
      )
    ch
    )
  )



#_(defn wrap-ctx-fn [cfg ctx-fn]
  (fn [v]
    (let [params v
          js-result (ctx-fn params)
          ]


         ;; by default do not convert params to js or ?

         )
    )
  )

(defn ^:export ctx_fn [js-fn]
  (fn [params]
    (let [js-res (js-fn (clj->js params))]

         ;; convert js result to
         (into {} (for [k (.keys js/Object js-res)]
                    (do
                      (let [nu-k (if (keyword? k) k (keyword k))
                            raw-cfg (aget js-res k)
                            cfg (js->clj raw-cfg :keywordize-keys true)
                            ]
                        ;; (.log js/console cfg)
                        [nu-k cfg]
                        )

                      )

                    ))
         )))



;; for now - auto-run wf, if we are not in the playground
#_(when-not (goog.object/get js/window "PLAYGROUND")
  ; (.clear js/console)
  (run_workflow)

  )
