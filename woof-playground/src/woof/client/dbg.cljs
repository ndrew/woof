(ns woof.client.dbg
  (:require
    [woof.base :as base]

    [clojure.string :as str]
    [woof.utils :as u]))

;; capturing wf with nicer logging



(defn log-ctx [ctx-map]
  (.groupCollapsed js/console "CTX")
    (.log js/console ctx-map)
  (.groupEnd js/console)
  )

(defn log-steps [steps]
  (.groupCollapsed js/console "STEPS")
    (.log js/console "STEPS" steps)
  (.groupEnd js/console))


(defn log-init-params [params]
  (.groupCollapsed js/console "INIT PARAMS")
    (.log js/console params)
  (.groupEnd js/console))


(def _log-once_impl (memoize (fn [s]
                               (.log js/console s)
                               s)))

(defn __log-once [s]
  (_log-once_impl s)
  )


(defn __log [& args]
  (apply (.-log js/console) args))

(defn __log-start []
  (.groupEnd js/console)
  (.group js/console (str "dbg-" (u/now))))

(defn __log-end []
  (.groupEnd js/console)
  )


(defn dbg-wf []
  (base/capturing-workflow-fn
    :context-map-fn (fn [ctx-map]
                      (log-ctx ctx-map)
                      ctx-map)
    :steps-fn (fn [steps]
                (log-steps steps)
                steps)
    :params (fn [params]
              (log-init-params params)
              params
              )
    )
  )