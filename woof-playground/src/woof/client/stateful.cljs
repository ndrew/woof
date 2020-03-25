(ns woof.client.stateful
  (:require
    [rum.core :as rum]

    [woof.base :as base]

    [woof.utils :as u]

    [woof.wfc :as wfc
     :refer [WoofWorkflow
             get-params
             get-context-map
             get-steps]]

    )

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



;; map structure for wf
(defn empty-swf [id]
  {
   :id id

   :status :not-started ;;

   :runtime {
             :xtor nil
             :wf nil

             :initial {}
             }
   ;; auto-combined functions
   :init-fns []
   :opt-fns []
   :ctx-fns []
   :steps-fns []
   ;; or just single ones?

   :result {

            }

   :error nil

   :ui-fn nil

   :state {

           }
   }
  )



;; fixme: do won't we have this in base/build-init-state-fn ??
(defn swf-bp-store-xtor [*state wf-chan xtor]
  (swap! *state assoc-in [:runtime :xtor] xtor)

  :ok)


(defn swf-opt-fn [*state params]
  {
   :before-process  (partial swf-bp-store-xtor *state)
   :op-handlers-map {
                     :process (fn [result]
                                (swap! *state assoc :result result)
                                result)
                     :done    (fn [result]
                                (swap! *state merge
                                       {
                                        :status :done
                                        :result result
                                        })
                                result)
                     :error   (fn [error]
                                (swap! *state merge
                                       {
                                        :status :error
                                        :result error
                                        })
                                error)
                     }
   })


(defn swf-capturing-WF [*swf params context-map-fn steps-fn wf-params]
  (swap! *swf assoc-in [:runtime :initial :params] params)
  (reify WoofWorkflow
    (get-params [this]
      ;; is this really needed
      params)
    (get-context-map [this]
      (let [ctx-map (context-map-fn params)]
        (swap! *swf assoc-in [:runtime :initial :context-map] ctx-map)
        ctx-map))
    (get-steps [this]
      (let [steps (steps-fn params)]
        (swap! *swf assoc-in [:runtime :initial :steps] steps)
        steps
        )))
  )


;; todo: is this needed
(defn swf-run! [*swf]
  (let [st @*swf

        init-fns (get st :init-fns [])
        ctx-fns (get st :ctx-fns [])
        steps-fns (get st :steps-fns [])
        opt-fns (get st :opt-fns [])

        merge-results-fn (get st :merge-results-fn merge)

        ]

    (if (= 0 (count ctx-fns))
      (u/throw! "no context functions provided!"))

    (if (= 0 (count steps-fns))
      (u/throw! "no steps functions provided!"))

    ;; todo: handle in-out merge - provide the merge function for steps

    (let [wf (base/parametrized-wf!
               (base/combine-init-fns init-fns)
               identity
               identity
               (base/combine-fns
                 (conj opt-fns
                       (partial swf-opt-fn *swf))
                 :merge-results base/merge-opts-maps)
               (base/combine-fns ctx-fns     :merge-results merge-results-fn)
               (base/combine-fns steps-fns   :merge-results merge-results-fn)
               (partial swf-capturing-WF *swf)
               )]

      ;; store prepared wf
      (swap! *swf assoc-in [:runtime :wf] wf)
      (swap! *swf assoc :status :running)

      (base/run-wf! wf identity)

      )
    )
  )

(defn swf-stop! [*swf]
  (let [swf @*swf]
    (base/end! (get-in swf [:runtime :xtor]))
    ;; which status to set
    )
  )


;; state factory
;; provides a sub-atom with id for workflow

(defprotocol StateFactory
  (sub-state [this id initial-v]))

(defn state-factory [*state-map]
  (reify StateFactory
    (sub-state [this id initial-v]
      (swap! *state-map assoc id initial-v)
      (rum/cursor-in *state-map [id])
      )))








(defn &wf-init-wf [wf]
  (get wf ::init-wf))


(defn wf-init! [*NODE]
  (if-let [f (&wf-init-wf @*NODE)]
    (f *NODE)
    (u/throw! "NO STATEFUL INITIALIZER FUNCTION PROVIDED")
    ))


(defn wf [id init-wf-fn]
  (assoc
    (empty-swf id )
    ::init-wf init-wf-fn)
  )





