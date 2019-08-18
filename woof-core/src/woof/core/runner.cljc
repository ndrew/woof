(ns woof.core.runner
  (:require
    [woof.wf :as wf]
    [woof.wfc :as wfc]
    [woof.xform :as x]
    [woof.utils :as u]))



;; generic runner for higher order wf

(defn run-wf

  [init-fn ; (fn [] => defaults )

   wf-fn   ; (fn [params] -> {:wf <wf>, :params {}})
   opts-fn ; (fn [params] -> {:opts <>, :params {}}

   run-fn ; (fn [wf_impl opts])
   ]


  ; params propagating - init -> wf -> opts  ; (opts (wf (init)))


  (let [
         ;; 1. get initial params
         initial-params (init-fn)

         ;; 2. init wf with init params -> get wf xtor fn and new params
         {
           wf :wf                 ;; workflow xtor
           wf-params :params          ;; (merge wf-def-params initial-params)
           } (wf-fn initial-params)

         ;; todo: may return channel

         ;; 3. get opts and opts params

         {
           params :params
           opts :opts
           } (opts-fn wf-params)

         ;; todo: may return channel

         wf-impl (wf params) ;; WoofWorkflow

         ]

    (run-fn wf-impl opts)
    )
  )


;; def wf processing via result processor

(defn default-run-fn [wf-impl opts]
  (let [xtor (wfc/wf-xtor wf-impl)]
    (wf/process-results! (wf/->ResultProcessor xtor opts))))

;; todo: wrap to a function with outer params (state for ui, socket chan for ws)



(defn merge-opts [opt1 opt2]
  (let [{bp1 :before-process
         op1 :op-handlers-map
         } opt1
        {bp2 :before-process
         op2 :op-handlers-map
         } opt2

        opts (merge opt1 opt2
         {
           :before-process (fn [wf-chan xtor]
                             (if (fn? bp1)
                               (bp1 wf-chan xtor))
                             (if (fn? bp2)
                               (bp2 wf-chan xtor))
                             )
           :op-handlers-map (merge-with juxt op1 op2)

           })
        ]
    opts
    ))



(defn merge-full-opts [opts1 opts2]
  {
    :params (merge (:params opts1)
                    (:params opts2))

    :opts (merge-opts (:opts opts1)
                      (:opts opts2))
    }
  )
