(ns woof.base
  (:require

    [woof.u :as u]
    [woof.data :as d]

    ;; ns for running wfs

    ; internal
    [woof.wf :as wf]
    ;[woof.core.processors :as p]

    [woof.utils :as utils]
    [woof.core.runner :as runner]

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
  )



(defn arg-fn
  "wraps var-args fn to a single arity fn"
  [f]
  (fn [params]
    (let [args (apply concat params)]
      ;(.log js/console args)
      (apply f args)
      )))


(defprotocol Woof
  (woof [this])
  )

;; init fn

(defn default-init-fn []
  {::woof (reify Woof
            (woof [this] "woof!"))}
  )

(defn stateful-init-fn [state* & {:keys [state-key] :or {state-key :STATE}}]
  (fn [params]
    (merge params
           {state-key state*})))


(defn meta-init-fn []
  (fn [params]
    (merge params
           {:META (atom {:IN  #{}
                         :OUT #{}})})))


(defn combine-init-fns [init-fns]
  (let [merge-fn (fn [{params :params} nu-params]
                   {:params (merge params nu-params)})]
    (fn []
      (doall
        (:params (reduce (fn [a x]
                          (merge-fn a (x (:params a))))
                      {:params {}}
                      (vec (reverse init-fns))
                      ))
        )))
  )


(defn combine-init-fns-old [init-fns]
  (apply comp
         (conj (vec (reverse init-fns))
               default-init-fn))
  )

;; do we need a keyed version of this?
(defn combine-init-fns* [& init-fns]
  (apply comp
         (conj (vec (reverse init-fns))
               default-init-fn))
  )


(defn combine-fns [fns & {:keys [merge-results] :or {merge-results merge}}]
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
                         (state-map-fn params) (vec (reverse fns))))
        )))
  )

(defn merge-opts-maps [a b]
  (let [bp1 (get a :before-process (fn [wf-chan xtor] :ok))
        bp2 (get b :before-process (fn [wf-chan xtor] :ok))

        ap1 (get a :after-process identity)
        ap2 (get b :after-process identity)


        combine-bp (fn [b1 b2]
                     (if (utils/channel? b1)
                       (let [chan (async/chan 1)]
                         (go
                           (async/<! b1)
                           (async/put! chan :ok))
                         chan)
                       b2)
                     b2)

        op1 (get a :op-handlers-map {})
        op2 (get b :op-handlers-map {})

        combine-ops (fn [o1 o2]
                      (merge-with juxt o1 o2))

        ]
    {:before-process (fn [wf-chan xtor]
                       (combine-bp (bp1 wf-chan xtor)
                                   (bp2 wf-chan xtor))
                       )
     :after-process (comp ap1 ap2)
     :op-handlers-map (combine-ops op1 op2)
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
     workflow-fn ;;
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



(defn process-wf! [xtor opts]
  (wf/process-results! (wf/->ResultProcessor xtor opts))
  )


(defn end! [xtor]
  (protocols/end! xtor))


(defn run-wf! [wf xtor-fn]
  (let [channel (runner/run-wf
                  (:init-fn wf)
                  (:wf-fn wf)   ;; {:params {..}, :wf <wf-xtor>}
                  (:opts-fn wf) ;; {:params {..}, :opts {..}}
                  (fn [wf-impl opts]
                    (let [xtor (wfc/wf-xtor wf-impl)]
                      (process-wf! (xtor-fn xtor) opts))))
        ]

    ; (println "z" channel)

    )
  )


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

  (close-chans! [this])
  )


(defn chan-factory [*state-map]
  (reify ChannelFactory

    (make-chan [this id]
      (let [c (async/chan)]
        (swap! *state-map assoc id c)
        c))

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
