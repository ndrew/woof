(ns woof.base
  (:require

    [woof.u :as u]
    [woof.data :as d]

    ;; ns for running wfs

    ; internal
    [woof.wf :as wf]
    [woof.core.processors :as p]

    [woof.utils :as utils]

    ;; higher level workflows
    [woof.wfc :as wfc
     :refer [WoofWorkflow
             get-params
             get-context-map
             get-steps]
     ]

    [woof.core.protocols :as protocols]


    ;; core async
    #?(:clj [clojure.core.async :as async :refer [go go-loop]])
    #?(:cljs [cljs.core.async :as async])
    )

  #?(:cljs
     (:require-macros
       [cljs.core.async.macros :refer [go go-loop]]
       ))
  ;(:gen-class)
  )

;(set! *warn-on-reflection* true)

;; use aliases for some common functions
(def pretty! d/pretty!)



;;;;;;;;;;;;;;;;;;;;
;; fn combination

(defn arg-fn
  "wraps var-args fn to a single arity fn"
  [f]
  (fn [params]
    (let [args (apply concat params)]
      (apply f args))))



(defn- init-combine
  ([init-fns merge-fn]
   (init-combine init-fns merge-fn {}))
  ([init-fns merge-fn initial-params]
   (doall
     (:params (reduce (fn [a x]
                        (merge-fn a (x (:params a))))
                      {:params initial-params}
                      (vec init-fns)))
     ))
  )


(defn combine-init-fns
  "combines init-fns list into a single one init-fn"
  [init-fns]
  (let [merge-fn (fn [{params :params} nu-params]
                   {:params (merge params nu-params)})]
    (partial init-combine init-fns merge-fn)))



(defn combine-fns
  "way of combining single-arity functions used in woof"
  [fns & {:keys [merge-results] :or {merge-results merge}}]
  (let [state-map-fn (fn [params]
                       {:params params
                        :result    {}})

        merge-fn (fn [{result :result
                       params :params} nu-result]
                   {:params params
                    :result (merge-results result nu-result)})

        params-fn (fn [state-map]
                    (:params state-map))

        data-fn (fn [state-map]
                  (:result state-map))
        ]
    (fn [params]
      (doall
        (data-fn (reduce (fn [state-map f]
                           (merge-fn state-map
                                     (f (params-fn state-map))))
                         (state-map-fn params) (vec
                                                 fns
                                                 ;(reverse fns)
                                                 )))
        )))
  )

(defn merge-opts-maps [a b]
  (let [combine-bp (fn [b1 b2]
                     (if (utils/channel? b1)
                       (let [chan (async/chan 1)]
                         (go
                           (async/<! b1)
                           (async/put! chan :ok))
                         chan)
                       b2)
                     b2)


        combine-ops-maps (fn [o1 o2]
                           ;; why not
                           ;; (merge-with juxt op1 op2)
                            (merge-with comp o1 o2))

        bp-b (get b :before-process (fn [wf-chan xtor] :ok))
        bp-a (get a :before-process (fn [wf-chan xtor] :ok))

        ap-a (get b :after-process identity)
        ap-b (get a :after-process identity)

        op-b (get b :op-handlers-map {})
        op-a (get a :op-handlers-map {})

        ]
    {
     :before-process (fn [wf-chan xtor]
                       (combine-bp (bp-b wf-chan xtor)
                                   (bp-a wf-chan xtor))
                       )
     :after-process (comp ap-b ap-a)

     :op-handlers-map (combine-ops-maps op-b op-a)
     }
    )
  )




(defn parametrized-wf!
  "parametrized wf implementation
  init-fn (fn []) => {..}
  "
  ([init-fn        ;; returns initial maps
   wf-params-fn   ;; transforms initial map to a wf params
   opt-params-fn  ;; transforms wf params to opt params
   opts-fn        ;; provides opts map via opt params
   context-map-fn ;; provides context map from wf params
   steps-fn       ;; provides steps map from wf params
   ]
  (let [wf-fn (fn [params]
                (let [nu-params (wf-params-fn params)]
                  {
                   :params nu-params
                   :wf     (fn [wf-params]
                             ;; do we really need to reify it everytime? why not use constructor
                             (reify WoofWorkflow
                               (get-params [this] nu-params) ;; is this really needed
                               (get-context-map [this] (context-map-fn nu-params))
                               (get-steps [this] (steps-fn nu-params)))
                             )
                   }
                  )
                )
        opts-fn (fn [opts-params]
                  (let [nu-opt-params (opt-params-fn opts-params)]
                    {
                     :params nu-opt-params
                     :opts   (opts-fn nu-opt-params)
                     })
                  )
        ]
    ;; todo: do we need a fn that will create this map.
    {
     :init-fn init-fn
     :wf-fn   wf-fn
     :opts-fn opts-fn
     }
    )
  )
  ([init-fn        ;; returns initial maps
     wf-params-fn   ;; transforms initial map to a wf params
     opt-params-fn  ;; transforms wf params to opt params
     opts-fn        ;; provides opts map via opt params
     context-map-fn ;; provides context map from wf params
     steps-fn       ;; provides steps map from wf params
     workflow-fn ;; todo: wft is workflow-fn?
     ]
    (let [wf-fn (fn [params]
                  (let [nu-params (wf-params-fn params)]
                    {
                     :params nu-params
                     :wf     (partial workflow-fn nu-params context-map-fn steps-fn)
                     }
                    )
                  )
          opts-fn (fn [opts-params]
                    (let [nu-opt-params (opt-params-fn opts-params)]
                      {
                       :params nu-opt-params
                       :opts   (opts-fn nu-opt-params)
                       })
                    )
          ]
      {
       :init-fn init-fn
       :wf-fn   wf-fn
       :opts-fn opts-fn
       }
      )
    )
  )


(defn as-fn-list [v]
  (cond
    (or (vector? v) (seq? v)) v ;; leave vector as is
    (map? v) [(fn [params] v)]  ;; wrap map as function
    (fn? v) [v]                 ;; wrap fn into a vector
    :else (do
            ;; what should be returned if unknown type passed?
            [])
    )
  )



;; shorter version of defining workflow
(defn wf! [& {:keys [init ctx steps opts ] :as params}]

  (let [init-fn (combine-init-fns (as-fn-list init))
        opts-fn (combine-fns (as-fn-list opts) :merge-results merge-opts-maps)

        ctx-fns  (as-fn-list ctx)
        steps-fns (as-fn-list steps)]

    (if (or (empty? steps-fns)
            (empty? ctx-fns))
      (utils/throw! ":steps and/or :ctx should be provided for a wf"))

    (parametrized-wf!
      init-fn
      identity
      identity
      opts-fn
      (combine-fns ctx-fns)
      (combine-fns steps-fns))
    )

  )


(defn WF [nu-params context-map-fn steps-fn wf-params]
  (reify WoofWorkflow
    (get-params [this] nu-params) ;; is this really needed
    (get-context-map [this] (context-map-fn nu-params))
    (get-steps [this] (steps-fn nu-params)))
  )


(defn capturing-WF [*wf nu-params context-map-fn steps-fn wf-params]
  ;; todo: how to use here your ::ctx and ::steps
  ;; (swap! *wf assoc ::init-params nu-params)
  (reify WoofWorkflow
    (get-params [this]
      nu-params) ;; is this really needed
    (get-context-map [this] (let [ctx-map (context-map-fn nu-params)]
                              (swap! *wf assoc ::ctx ctx-map)
                              ctx-map))
    (get-steps [this] (let [steps (steps-fn nu-params)]
                        (swap! *wf assoc ::steps steps)
                        steps
                        )))
  )



(defn end! [xtor]
  (protocols/end! xtor))






;; todo: make xtor-fn optional
(defn do-run-wf!
  ([processor wf]
   (do-run-wf! processor wf identity))
  ([processor wf xtor-fn]

   (let [
         init-fn (:init-fn wf) ; (fn [] => defaults )
         wf-fn   (:wf-fn wf)   ; (fn [params] -> {:wf <wf>, :params {}})
         opts-fn (:opts-fn wf) ; (fn [params] -> {:opts <>, :params {}}

         ;; 1. get initial params
         initial-params (init-fn)

         ;; 2. init wf with init params -> get wf xtor fn and new params
         {
          wf :wf                 ;; workflow xtor
          wf-params :params      ;; (merge wf-def-params initial-params)
          } (wf-fn initial-params)


         ;; 3. get opts and opts params
         ;; todo: may return channel
         {
          params :params
          opts :opts
          } (opts-fn wf-params)

         wf-impl (wf params) ;; WoofWorkflow
         ;; todo: may return channel

         ]

     (let [xtor (wfc/wf-xtor wf-impl)
           xtor-impl (xtor-fn xtor)]
       (wf/process-results! (processor xtor-impl opts)))
     )

   #_(runner/run-wf
      (:init-fn wf)
      (:wf-fn wf)   ;; {:params {..}, :wf <wf-xtor>}
      (:opts-fn wf) ;; {:params {..}, :opts {..}}
      (fn [wf-impl opts]
        (let [xtor (wfc/wf-xtor wf-impl)
              xtor-impl (xtor-fn xtor)]
             (wf/process-results! (processor xtor-impl opts)))))
   )
  )

;; for now dont use
(defn run-wf-internal! [& {:keys [processor-fn init-fn wf-fn opts-fn] :or {processor-fn wf/->ResultProcessor} :as params}]

  (do-run-wf!
      processor-fn
      {
       :init-fn init-fn
       :wf-fn   wf-fn
       :opts-fn opts-fn
       })

  )

(def run-wf! (partial do-run-wf! wf/->ResultProcessor))

#?(:clj
   (def sync-run-wf! (partial do-run-wf! p/->FutureWF)))


(def rand-sid wf/rand-sid)


(defn time-chunk-execute [t executor]
  (let [exec-chann-0 (protocols/execute! executor)
        exec-chann (async/pipe
                     exec-chann-0
                     (async/chan 1 (utils/time-update-xf t)))]
    ;; (async/chan 1 (wf/chunk-update-xf 20))
    exec-chann)
  )


;;
;; channel factory

(defprotocol ChannelFactory
  (make-chan [this id])

  ; (make-mult [this id])

  (get-chan [this id])

  (close-chans! [this])
  )


(defn chan-factory [*state-map]
  (reify ChannelFactory

    (make-chan [this id]
      (let [c (async/chan)]
        (swap! *state-map assoc id c)
        c))

    (get-chan [this id]
      (get @*state-map id))

    #_(make-mult [this id]
                 (let [reload-chan (async/chan)
                       reload-mult (async/mult reload-chan)]
                   ;...
                   ))

    (close-chans! [this]
      (utils/close-channels! @*state-map)
      )

    )
  )


;; todo: migrate wf-data usages here

(defn extract-result
  "gets the final values from workflow result for specified keyz"

  ([result k]
   (extract-result (fn [all-keys v] v) result [] k))

  ([f result all-keys k]
   (let [r (get result k)]
     (if (u/sid-list? r)
       (let [nu-keys (conj all-keys k)]
         (mapcat (fn [a]
                   (let [u (extract-result f result nu-keys a)]
                     (if (seq? u)
                       u [u]))) r)
         )

       ;; todo: maybe pass here the parent key also, or smth like [:parent-key :key]
       (f
         (conj all-keys k) r)))))




(defn extract-results
  "get workflow results, but for certain keys"
  ([result keyz f]
   (into (array-map)
         (map (fn[k]
                [k (extract-result f result [] k)])
              keyz)))

  ([result keyz]
   (extract-results result keyz (fn [all-keys v] v))))


;; todo: recursive extract results


(defn inline-results
  "substitutes expanded items and removes intermediary values from the result map"
  [results]

  (let [new-results (reduce
                      (fn [a [k v]]
                        (if (u/sid-list? v)

                          (let [nu-vals (reduce (fn [a k]
                                                  (assoc a k (get results k))
                                                  ) (::tmp a) v)]

                            (assoc a
                              k (map (fn[id] (get nu-vals id)) v)
                              ::tmp nu-vals)
                            )

                          (assoc a k v))
                        ) {::tmp {} } results)]

    (let [{tmp ::tmp} new-results]
      (apply dissoc new-results (conj (keys tmp) ::tmp))
      )))




;;;


(defn _expand-steps-fn [step-id els]
  (reduce (fn [a e]
            ;; todo: check for sid length
            (assoc a (rand-sid (str (name step-id) "-")) [step-id e])) {} els)
  )


(defn expand-into-prefixed
  [step-id]
  {
   :fn       (fn [els]
               (reduce (fn [a e] (assoc a (rand-sid (str (name step-id) "-")) [step-id e])) {} els))
   :expands? true
   }
  )


(defn expand-into-normal
  [step-id]
  {
   :fn       (fn [els]
               (reduce (fn [a e] (assoc a (rand-sid) [step-id e])) {} els))
   :expands? true
   }
  )

(defonce expand-into (if true expand-into-prefixed
                          expand-into-normal))


;;

;; run on-done when wf is stopped (done/error)
(defn build-opt-on-done [on-done]
  (fn [params]
    {:op-handlers-map {
                       :done  (fn [result] (on-done params result))
                       :error (fn [result] (on-done params result))
                       }}))

;;
;; state wf aspect - injects atom into workflow
;;
;;


(defn stateful-init-fn [state* & {:keys [state-key] :or {state-key :STATE}}]
  ;; todo: is this needed
  (fn [params]
    (merge params
           {state-key state*})))



(defn build-init-state-fn [*STATE]
  (fn [_] {::state *STATE}))

;; keep xtor in state as ::xtor
(defn build-opt-state-fn [*state]
  (fn [params]
    {:before-process  (fn [wf-chan xtor]
                        (swap! *state assoc ::xtor xtor)

                        :ok)})
  )

;; state accessor
(defn &state [params]
  ;; todo: throw if no state is provided
  (::state params))

;; xtor accessor
(defn state-get-xtor [*state]
  (get-in @*state [::xtor]))





;; todo: on-stop is useless as we need to use last opt-fn for actually workflow ended
(defn stateful-wf
  ([*state wf on-stop]
   (stateful-wf *state wf on-stop {}))
  ([*state wf on-stop api-map]
   (merge api-map
          {
           :wf        wf

           :state     *state

           :start-wf! (fn [] (run-wf! wf))

           :stop-wf!  (fn []
                        (if-let [xtor (state-get-xtor *state)]
                                (do
                                  (end! xtor)
                                  ;; how to call on stop after all opts were completed?
                                  (on-stop)
                                  ::stopped)
                                (do
                                  ;;(prn ::no-wf-running)
                                  ::no-wf-running)))
           }
          )
   )
  )


;;


;;
;; channel factory
;;

(defn build-init-chan-factory-fn [chan-factory]
  (fn [_]
    {::channel-factory chan-factory}))

(defn &chan-factory [params]
  (if-let [cf (get params ::channel-factory)]
    cf
    (utils/throw! "no ::cf provided in params. Ensure that build-init-chan-factory-fn had been called" )))


(defn build-opts-chan-factory-fn [channel-factory]
  (build-opt-on-done
    (fn [params result] ;; TODO: channel factory can be retrieved here
      (close-chans! channel-factory)
      result)))



;; common ctx

;; add here re-occurring workflow steps

(defonce BASE-CTX-MAP {
                   :id       {:fn identity}
                   :identity {:fn identity}

                   :collect  {:fn       (fn [v] v)
                              :collect? true}

            ;;
                   })