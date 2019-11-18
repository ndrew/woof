(ns woof.v2.wf.stateful
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]

    [woof.base :as base]
    [woof.data :as d]
    [woof.playground.common :as cmn]
    [woof.utils :as utils]

    [woof.playground.v1.ui :as ui]
    [woof.playground.v1.utils :as v1u :refer [dstr kstr vstr]]

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