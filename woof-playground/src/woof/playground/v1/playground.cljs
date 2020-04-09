(ns ^:figwheel-hooks woof.playground.v1.playground
  (:require
    [rum.core :as rum]

    ;; client core
    [woof.base :as base]
    [woof.wf :as wf]

    [woof.wfc :as wfc
     :refer [WoofWorkflow
             get-params
             get-context-map
             get-steps]]

    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



(defn capturing-WF [*wf nu-params context-map-fn steps-fn wf-params]
  ;; todo: how to use here your ::ctx and ::steps - substitute via methods
  ;; (swap! *wf assoc ::init-params nu-params)

  (reify WoofWorkflow
    (get-params [this] nu-params)                           ;; is this really needed
    (get-context-map [this] (let [ctx-map (context-map-fn nu-params)]
                              (swap! *wf assoc ::ctx ctx-map)
                              ctx-map))
    (get-steps [this] (let [steps (steps-fn nu-params)]
                        (swap! *wf assoc ::steps steps)
                        steps
                        )))
  )



(defn run-wf! [*wf & wfs]
  (let [empty-cfg {
                   :init-fns  []
                   :ctx-fns   []
                   :steps-fns []
                   :opt-fns   []
                   }
        resulting-cfg (reduce (fn [a cfg]
                                (merge-with into a cfg))
                              empty-cfg wfs)
        {init-fns  :init-fns
         ctx-fns   :ctx-fns
         steps-fns :steps-fns
         opt-fns   :opt-fns
         } resulting-cfg
        ]
    (try
      (let [
            ; latest fn that returns params
            opt-params-fn (fn [params]
                            ;; maybe store params here
                            ;;(.log js/console "params" params)
                            ;(swap! *wf assoc ::params params)
                            params
                            )                                 ;; transforms wf params to opt params

            wf (base/parametrized-wf!
                 (base/combine-init-fns init-fns)
                 ;; transforms initial map to a wf params
                 identity
                 opt-params-fn

                 (base/combine-fns opt-fns :merge-results base/merge-opts-maps)
                 (base/combine-fns ctx-fns)
                 (base/combine-fns steps-fns)
                 (partial capturing-WF *wf)
                 )]

        ; store wf
        (swap! *wf merge wf)
        (swap! *wf merge
               {
                ::status :running
                ::error nil
                }
               )
        (base/run-wf! wf)
        )
      (catch js/Error e
        (.error js/console e)
        (swap! *wf assoc ::error e)
        )
      )

    ))


(defn ui-wf [*wf-state cfg]
  ;; can ds have a keyword key?
  (let [
        init-fns (get cfg :init-fns [])
        ctx-fns (get cfg :ctx-fns [])
        steps-fns (get cfg :steps-fns [])
        opt-fns (get cfg :opt-fns [])
        _init-fns (conj init-fns
                        (fn [params]
                          ;; <!> pass the workflow state,
                          {:wf *wf-state}
                          )
                        )

        ]

    (let [wf-cfg {
                  :init-fns  _init-fns
                  :ctx-fns   ctx-fns
                  :steps-fns steps-fns
                  :opt-fns   opt-fns
                  }]
      (run-wf! *wf-state wf-cfg)
      )))


(defonce *STATE-MAP (atom {
                           :WFs {}
                           :UI-wfs []
                           }))


;; creates a state map for workflow
(defn wf-state []
  {
   :id (wf/rand-sid "wf-")
   :status :compile
       ;:prepare                                         ; :compile

   ; try with atoms now
   :IN     {
            :VALUES []
            }
   :OUT    {

            :EXPORT {
                     ;; export-key {:step-id .., :value ... }
                     }
            }
   })


(defn add-wf! []
  (let [nu-state (wf-state)
        id (:id nu-state)]
    (swap! *STATE-MAP assoc-in [:WFs id] nu-state)
    (swap! *STATE-MAP update-in [:UI-wfs] conj  {:id id})))

(defn remove-wf! [id]
  (swap! *STATE-MAP update-in [:UI-wfs] #(filter (fn [w] (not= id (:id w))) %))
  (swap! *STATE-MAP update-in [:WFs] dissoc id))