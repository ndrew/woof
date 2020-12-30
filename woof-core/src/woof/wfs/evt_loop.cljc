(ns woof.wfs.evt-loop
  (:require
    [woof.utils :as u]

    #?(:clj [clojure.core.async :as async :refer [go go-loop]])
    #?(:cljs [cljs.core.async :as async])
    ))


(defn build-evt-loop-init-map [evt-loop-chan]
  {
   ;; keep the evt loop chan
   ::evt-loop-chan evt-loop-chan
   })


(defn build-evt-loop-init-fn [evt-loop-chan]
  (let [init-map (build-evt-loop-init-map evt-loop-chan)]
    (fn [_] init-map)))


;; params-getter
(defn &evt-loop [params]
  (if-let [evt-loop (get params ::evt-loop-chan)]
    evt-loop
    (u/throw! "no ::evt-loop-chan provided in params. Ensure that init-fn built by `build-evt-loop-init-fn` is added to :init" )))


(defn _emit-steps [params steps]
  (let [evt-loop (&evt-loop params)]
    (async/put! evt-loop steps)))


(defonce EVT-LOOP-CTX-MAP {
                           ;; use this name or something like infinite-expander?
                           :evt-loop {
                                      :fn       (fn [in-chan] in-chan)
                                      :infinite true
                                      :expands? true}
                           })

(defn evt-loop-ctx-fn [params]
  EVT-LOOP-CTX-MAP
)
