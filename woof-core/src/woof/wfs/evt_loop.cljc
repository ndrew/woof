(ns woof.wfs.evt-loop
  (:require
    [woof.utils :as u]))


(defn build-evt-loop-init-fn [evt-loop-chan]
  (fn [_]
    {
     ;; keep the evt loop chan
     ::evt-loop-chan evt-loop-chan
     }))

;; params-getter
(defn &evt-loop [params]
  (if-let [evt-loop (get params ::evt-loop-chan)]
    evt-loop
    (u/throw! "no ::evt-loop-chan provided in params. Ensure that init-fn built by `build-evt-loop-init-fn` is added to :init" )))



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
