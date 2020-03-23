(ns woof.client.stateful
  (:require
    [rum.core :as rum]

    [woof.base :as base]
    [woof.playground.state :as state]

    [woof.utils :as u])

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



;; just storage
(defonce *INTERNAL (atom
                     {
                      ::chans {}
                      }))

(defonce channel-factory (base/chan-factory (rum/cursor-in *INTERNAL [::chans])))

(defonce chan-factory-init-fn (base/build-init-chan-factory-fn channel-factory))

(defonce chan-factory-opts-fn (base/build-opts-chan-factory-fn channel-factory))



;; exposing channels for steps function
(defn &chan [params chan-sid]
  (base/make-chan (base/&chan-factory params) chan-sid))

;; exposes init param by it's key
(defn &wf-init-param [*wf k]
  (get-in @*wf [:runtime :initial :params k]))


(defn default-actions-map [init-wf! run-wf! stop-wf! wf-actions-map]
  {
   :not-started (conj (get wf-actions-map :not-started [])
                      [] ["run!" run-wf!])
   :running     (conj (get wf-actions-map :running [])
                      [] ["stop!" stop-wf!])
   :done        (conj (get wf-actions-map :done [])
                      [] ["reset!" init-wf!])
   })




(defn &wf-init-wf [wf]
  (get wf ::init-wf))


(defn wf-init! [*NODE]
  (if-let [f (&wf-init-wf @*NODE)]
    (f *NODE)
    (u/throw! "NO STATEFUL INITIALIZER FUNCTION PROVIDED")
    ))


(defn wf [id init-wf-fn]
  (assoc
    (state/empty-swf id )
    ::init-wf init-wf-fn)
  )





