(ns woof.v2.wf.stateful
  (:require
    [rum.core :as rum]

    [woof.base :as base]
    [woof.playground.state :as state]

    [woof.utils :as u])

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



(defn chan-factory-init-fn_
  [*channel-storage]
  (let [cf (base/chan-factory *channel-storage)]
    {
     ;; provide a channel factory, so wf should not care about closing channels
     ::cf cf
     }
    ))


(defn &chan-factory [params]
  (if-let [cf (get params ::cf)]
    cf
    (u/throw! "no ::cf provided in params. Ensure that chan-factory-init-fn had been called" )
    )
  )


(defn &chan [params chan-sid]
  (base/make-chan (&chan-factory params) chan-sid))


(defn chan-factory-opts [params]
  (let [close! (fn [result]
                 (base/close-chans! (&chan-factory params))
                 result)]
    {
     :op-handlers-map {
                       :done close!
                       :error close!
                       }
     }
    )
  )

;; should there be exposing channels via steps

;; just storage
(defonce *INTERNAL (atom
                     {
                      ::chans {}
                      }))


(defn &wf-init-param [*wf k]
  (get-in @*wf [:runtime :initial :params k]))


(defn &wf-init-wf [wf]
  (get wf :woof.v2.wf.stateful/init-wf))


(defn wf-init! [*NODE]
  (if-let [f (&wf-init-wf @*NODE)]
    (f *NODE)
    (u/throw! "NO STATEFUL INITIALIZER FUNCTION PROVIDED")
    ))


(defn wf [id init-wf-fn]
  (assoc
    (state/empty-swf id )
    :woof.v2.wf.stateful/init-wf init-wf-fn)
  )


(defn default-actions-map [init-wf! run-wf! stop-wf! wf-actions-map]
  {
   :not-started (conj (get wf-actions-map :not-started [])
                      [] ["run!" run-wf!])
   :running     (conj (get wf-actions-map :running [])
                      [] ["stop!" stop-wf!])
   :done        (conj (get wf-actions-map :done [])
                      [] ["reset!" init-wf!])
   })



(defn &channel-map [wf]
  (:woof.v2.wf.stateful/channel-map wf))


(defn channel-map []
  {
   :woof.v2.wf.stateful/channel-map (rum/cursor-in *INTERNAL [::chans])
   })



