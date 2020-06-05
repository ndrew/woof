(ns ^:figwheel-hooks woof.client.browser.js
  (:require
    [woof.client.dom :as dom]

    [woof.base :as base]
    [woof.utils :as u]

    [goog.dom :as gdom]

    [cljs.core.async :refer [go] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]

    ))



;;
;; JS interop block, currently

(defn convert-js-array [a]
  (if (.isArray js/Array a)
    (js->clj a)
    a)
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
                    (let [nu-k (if (qualified-keyword? k) k (qk k))
                          raw-v (aget js-res k)
                          raw-step (aget raw-v 0)
                          step (if (keyword? raw-step) raw-step (k raw-step))
                          v (aget raw-v 1)
                          ]
                      [nu-k [step v]]
                      )
                    ))
         )))


(defn ^:export promise2chan [v]
  (let [ch (async/chan)]
    (go
      (let [v (<p! v)]
        (async/put! ch (if (nil? v) :nil v)))
      )
    ch
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


(defn ^:export run_js_workflow [
                                init-fns
                                ctx-fns
                                steps-fns
                                opt-fns
                                ]
  (let [wf-impl (base/wf!
                  :init (convert-js-array init-fns)
                  :opts (convert-js-array opt-fns)
                  :ctx (convert-js-array ctx-fns)
                  :steps (convert-js-array steps-fns))

        run-fn! (fn []
                  (.log js/console "RUNNING SCRAPING WF!")
                  (base/run-wf! wf-impl))]


    (run-fn!)

    )


  )


