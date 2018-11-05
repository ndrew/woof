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

   run-fn ; (fn [wf-impl opts])
   ]


  ; params propagating - init -> wf -> opts  ; (opts (wf (init)))


  (let [
         ;; 1. get initial params
         initial-params (init-fn)

         ;; 2. init wf with init params -> get wf xtor fn and new params
         {
           wf :wf                 ;; workflow xtor
           wf-params :params          ;; (merge wf-default-params initial-params)
           } (wf-fn initial-params)

         ;; todo: may return channel

         ;; 3. get opts and opts params

         {
           params :params
           opts :opts
           } (opts-fn wf-params)

         ;; todo: may return channel

         wf-impl (wf params)

         ]

    (run-fn wf-impl opts)
    )
  )


;; default wf processing via result processor

(defn default-run-fn [wf-impl opts]
  (let [xtor (wfc/wf-xtor wf-impl)]
    (wf/process-results! (wf/->ResultProcessor xtor opts))))

;; todo: wrap to a function with outer params (state for ui, socket chan for ws)
