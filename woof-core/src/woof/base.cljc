(ns woof.base
  (:require

    [woof.u :as u]
    [woof.data :as d]

    ;; ns for running wfs

    ; internal
    [woof.wf :as wf]
    [woof.core.processors :as p]

    [woof.utils :as utils]


    [woof.core.api :refer [WoofWorkflow
                             get-params
                             get-context-map
                             get-steps]]

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
                        :result    (array-map)})

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


        ;; todo: how to handle waiting for resources from opts-handler
        combine-ops-maps (fn [o1 o2]
                           ;; why not
                           ;; (merge-with juxt op1 op2)

                           ;; FIXME: can op map return channels??
                            (merge-with comp o1 o2)

                           )

        bp-b (get b :before-process (fn [wf-chan xtor] :ok))
        bp-a (get a :before-process (fn [wf-chan xtor] :ok))

        ;; should after be in the resersed order
        ap-a (get b :after-process identity)
        ap-b (get a :after-process identity)

        op-b (get b :op-handlers-map {})
        op-a (get a :op-handlers-map {})

        ;; todo: merging :execute

        ]

    (merge
      (if-let [exec-a! (get a :execute)]
        {:execute exec-a!}
        (if-let [exec-b! (get b :execute)]
          {:execute exec-b!}
          {}))

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
  )




(defn WF-impl
  "wf representation that could be run via run-wf!

  init-fn   (fn [] => {..initial-params-map})                                - returns initial params map
  wf-fn     (fn [{..initial-params-map}] => {:params {..initial-params-map}
                                             :wf      WoofWorkflow})         - returns wf map (wf implementation + params)
  opts-fn   (fn [{..initial-params-map}] => {:params {..initial-params-map}
                                             :opts    {..opt-map}})          - returns opts map (opts map + params)
  "
  [init-fn
   wf-fn
   opts-fn]
  {
   :init-fn init-fn
   :wf-fn   wf-fn
   :opts-fn opts-fn
   }
  )

(defn _default-wf-impl [init-params ctx-fn steps-fn wf-params]
  ;; do we really need to reify it everytime? why not use constructor
  (reify WoofWorkflow
    (get-params [this] init-params) ;; is this really needed
    (get-context-map [this] (ctx-fn init-params))
    (get-steps [this] (steps-fn init-params)))
  )


;; FIXME: remove wf-params-fn - as it's not used anywhere
(defn parametrized-wf!
  "defines maximally parametrized woof workflow

  init-fn       (fn [] => {..initial-params-map})                   - returns initial params map

  wf-params-fn  (fn [{..initial-params-map}] => {..wf-params-map})  - transforms initial params map to a wf params

  opt-params-fn (fn [{..wf-params-map}] => {..opts-params})         - transforms wf params to opt params
  opts-fn       (fn [{..opts-params}]   => {..opts-map})            - provides opts map via opt params

  ctx-fn        (fn [{..wf-params-map}] => {..ctx-map})             - provides context map from wf params
  steps-fn      (fn [{..wf-params-map}] => {..steps-map})           - provides steps map from wf params
  "
  ([init-fn        ;; returns initial maps
   wf-params-fn    ;; transforms initial map to a wf params
   opt-params-fn   ;; transforms wf params to opt params
   opts-fn         ;; provides opts map via opt params
   ctx-fn          ;; provides context map from wf params
   steps-fn        ;; provides steps map from wf params
   ]
   (parametrized-wf! init-fn wf-params-fn opt-params-fn opts-fn ctx-fn steps-fn _default-wf-impl)
  )
  ([init-fn         ;; returns initial maps
     wf-params-fn   ;; transforms initial map to a wf params
     opt-params-fn  ;; transforms wf params to opt params
     opts-fn        ;; provides opts map via opt params
     context-map-fn ;; provides context map from wf params
     steps-fn       ;; provides steps map from wf params
     workflow-fn    ;; provides a WoofWorkflow protocol function
     ]
    (let [wf-fn (fn [params]
                  ;;
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
      (WF-impl init-fn wf-fn opts-fn)
      )
    )
  )


(defn as-fn-list [v]
  (cond
    (or (vector? v) (seq? v))
      (vec (map (fn [item]
             (cond
               (map? item) (fn [params] item)
               (fn? item) item
               :else item)
             ) v))
    (map? v) [(fn [params] v)]  ;; wrap map as function
    (fn? v) [v]                 ;; wrap fn into a vector
    :else (do
            ;; what should be returned if unknown type passed?
            [])
    )
  )



;; shortest version of defining workflow
(defn wf!
  "main way of defing a woof workflow
  required parameters are
   :ctx - context map, e.g {:s-handler-id <shandler-body>, ...}
        | context map constructor, e.g. (fn [params] => {:s-handler-id <shandler-body>, ...})
        | sequence/vector of context maps or its constructors
   :steps
        | steps map | step map constructor | seq/vector of step maps or its constructors
  optional
   :init
        | params map | params map constructor | seq/vector of params maps or its constructors
   :opts
        | opts map | opts map constructor | seq/vector of opts maps or its constructors
   :wf-impl
       custom wf implementation - for capturing workflows
  "
  [& {:keys [init ctx steps opts wf-impl]}]

  (let [init-fn (combine-init-fns (as-fn-list init))
        opts-fn (combine-fns (as-fn-list opts) :merge-results merge-opts-maps)

        ctx-fns  (as-fn-list ctx)
        steps-fns (as-fn-list steps)

        wf-fn (if (nil? wf-impl) _default-wf-impl wf-impl)
        ]


    (if (or (empty? steps-fns)
            (empty? ctx-fns))
      (utils/throw! ":steps and/or :ctx should be provided for a wf"))

    (parametrized-wf!
      init-fn
      identity
      identity
      opts-fn
      (combine-fns ctx-fns)
      (combine-fns steps-fns)
      wf-fn)
    )

  )





;;
(defn capturing-workflow-fn [& {:keys [params-fn
                                       context-map-fn
                                       steps-fn ]
                                :or {params-fn identity
                                     context-map-fn identity
                                     steps-fn identity
                                     }}]
  (fn [params _context-map-fn _steps-fn wf-params]
    ;; todo: why wf-params are needed
    (let [nu-params (params-fn params)]
      (reify WoofWorkflow

        (get-params [this]
          nu-params)

        (get-context-map [this]
          (context-map-fn (_context-map-fn nu-params)))

        (get-steps [this]
          (steps-fn (_steps-fn nu-params))))
      )
    )
  )

(defn end! [xtor]
  (protocols/end! xtor))





(defn wf-xtor [wwf]
  (let [ctx (wf/make-context (get-context-map wwf))
        steps (get-steps wwf)]
    ;; returns executor
    (wf/build-executor
      ctx
      steps)
    )
  )


;; return
(defn params-wf [params context-fn steps-fn]
  (let [args (apply concat params)
        context-map (apply context-fn args)
        steps (apply steps-fn args)
        ]
    (reify WoofWorkflow
      (get-params [this]         params)
      (get-context-map [this]    context-map)
      (get-steps [this]           steps)
      )
    )
  )



;; todo: will it make sense to pass processor in wf map ??

(defn do-run-wf!
  ([processor wf]
   (do-run-wf! processor wf identity))
  ([processor-fn
    wf
    xtor-fn]

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

     ;; todo: should this be also parametrized?
     (let [ctx-map (get-context-map wf-impl)

           ;; context is
           ;; * WoofExecutorFactory
           ;; * WoofContext
           ctx (wf/make-context ctx-map)
           steps (get-steps wf-impl)

           xtor (wf/build-executor ctx steps)
           xtor-impl (xtor-fn xtor)

           processor-impl (processor-fn xtor-impl opts)]

       ;; processes results received from executor
       (wf/process-results! processor-impl))
     )
   )
  )


;; default result processor
(defonce default-result-processor wf/->ResultProcessor)


(def run-wf! (partial do-run-wf! default-result-processor))

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
               (reduce (fn [a e] (assoc a (rand-sid) [step-id e]))
                       (array-map) els))
   :expands? true
   }
  )


(defn expand-limited [step-id n]
  {
   :fn (fn [els]
         (reduce (fn [a e] (assoc a (rand-sid) [step-id e]))
                 {}
                 (take n els)))
   :expands? true
   }
  )


#_(defn chain-expanded-ctx-cfg [wrapper-step]
  {
   :fn       (fn [sid-list]
               (reduce (fn [a p]
                         (assoc a (wf/rand-sid) [wrapper-step p]))
                       {} sid-list))
   :expands? true
   :collect? false
   }
  )


(defonce expand-into (if false expand-into-prefixed
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

;; state accessor
(defn &state [params]
  ;; todo: throw if no state is provided
  (::state params))

;; xtor accessor
(defn state-get-xtor [*state]
  (get-in @*state [::xtor]))


;; keep xtor in state as ::xtor
(defn build-opt-state-fn [*state] ;;  & {:keys [remove-state-key] :or {state-key :STATE}}
  (fn [params]
    {:before-process  (fn [wf-chan xtor]
                        (swap! *state assoc ::xtor xtor)

                        :ok)})
  )




;;
;; TODO: find ways of notifying that workflow had ended and implement them in base - see (auto-run-wf! ...)
;;
(defn stateful-wf
  "build a stateful workflow map with :start-wf! and :stop-wf! 'methods' to start and stop workflow. Providing :api allows to have other user 'methods' or data "
  [*state wf & {:keys [api] :or {api {}}}]

  (merge api
         {
          :wf        wf

          ;; if the :state is needed to be stored here, it can be done via api
          ;; :state     *state

          :start-wf! (fn [] (run-wf! wf))

          :stop-wf!  (fn []
                        (if-let [xtor (state-get-xtor *state)]
                          (do
                            (end! xtor)
                            ::wf-stop-started)
                          (utils/throw! "no xtor in state")))

          }
         )
  )


;; <?> for stateful-wf we provide an wf instance, here we provide a function that return an wf instance
(defn auto-run-wf!
  "runs workflow, if wf is already running, stops, waits until it is finished and then started"
  [*wf-instance wf-constructor-fn & {:keys [on-stop]}]

  (if-let [old-instance @*wf-instance]
    ;; re-start wf if it's already running
    (let [stop-wf-fn! (:stop-wf! old-instance)]
      ;; trigger wf stop
      (stop-wf-fn!)

      (let [on-stop (:on-stop old-instance)
            stop-signal (if on-stop (on-stop @*wf-instance))
            ]
        (if (and (not (nil? stop-signal))
                 (utils/channel? stop-signal))
          (go
            ;; get signal from stop-channel
            (let [_ (async/<! stop-signal)]
              (reset! *wf-instance (wf-constructor-fn @*wf-instance))
              ((:start-wf! @*wf-instance))
              ))
          (do
            ;; start wf right away
            (reset! *wf-instance (wf-constructor-fn @*wf-instance))
            ((:start-wf! @*wf-instance))
            )
          )
        )
    )
    ;; else, start workflow if atom has no previous workflow
    (let [WF (wf-constructor-fn nil)]
      (reset! *wf-instance (if on-stop (assoc WF :on-stop on-stop)
                                       WF))
      ((:start-wf! @*wf-instance))
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



(defn _chunked-execute-fn [n executor]
  (let [exec-chann-0 (protocols/execute! executor)
        exec-chann (async/pipe
                     exec-chann-0
                     (async/chan 1 (wf/chunk-update-xf n)))]


    exec-chann)
  )


(defn _timed-execute-fn [t executor]
  (let [exec-chann-0 (protocols/execute! executor)
        exec-chann (async/pipe
                     exec-chann-0
                     (async/chan 1 (wf/time-update-xf t)))]
    ;; (async/chan 1 (wf/chunk-update-xf 20))

    exec-chann))


(defn &
  "generic accessor"
  [params k msg]
  (if-let [v (get params k)]
    v
    (utils/throw! msg)))
