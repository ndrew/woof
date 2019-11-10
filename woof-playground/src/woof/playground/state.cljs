(ns woof.playground.state
  "woof state mgmt"
  (:require
    [cljs.pprint :as cljs-pprint]
    [cljs.reader]
    [rum.core :as rum]

    [woof.base :as base]

    [woof.utils :as u]))


;; statuses
;:not-started ""
;:done        "done"
;:running     "pending"
;:stopped     "error"
;:error       "error"


(defn empty-swf [id]
  {
   :id id

   :status :not-started ;;

   :runtime {
             :xtor nil
             :wf nil
             }
   ;; auto-combined functions
   :init-fns []
   :opt-fns []
   :ctx-fns []
   :steps-fns []
   ;; or just single ones?


   :actions {

             }

   :result {

            }

   :error nil

   :ui-fn nil

   :state {

           }
   }
  )

(defn state-wf [id]
  (empty-swf id)
  ;; todo: merge stuff
  )


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


(defn swf-run! [*swf]
  (let [st @*swf

        init-fns (get st :init-fns [])
        ctx-fns (get st :ctx-fns [])
        steps-fns (get st :steps-fns [])
        opt-fns (get st :opt-fns [])
        ]

    (if (= 0 (count ctx-fns))
      (u/throw! "no context functions provided!"))

    (if (= 0 (count steps-fns))
      (u/throw! "no steps functions provided!"))


    (let [wf (base/parametrized-wf!
               (base/combine-init-fns init-fns)
               identity
               identity
               (base/combine-fns
                 (conj opt-fns
                       (partial swf-opt-fn *swf))
                 :merge-results base/merge-opts-maps)
               (base/combine-fns ctx-fns)
               (base/combine-fns steps-fns))]

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



