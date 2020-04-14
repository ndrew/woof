(ns woof.core.processors
  "woof processors stuff"
  (:require [woof.wf :as wf]
            [woof.core.protocols :as proto]

            #?(:clj [woof.utils :as u :refer [put!? debug! inline--fn inline--fn1]])
            #?(:cljs [woof.utils :as u])


            #?(:clj [clojure.core.async :as async :refer [go go-loop]])
            #?(:cljs [cljs.core.async :as async])
            )

  #?(:cljs
     (:require-macros
       [cljs.core.async.macros :refer [go go-loop]]
       [woof.utils-macros :refer [put!? debug! inline--fn inline--fn1]]
       ))
  (:gen-class)
  )


;; convenience functions
;; ========================================================

(defn- process-wf-result [v]
  ;; todo: do we need to check meta here?
  (if (u/exception? v)
    (u/throw! v)
    (if (nil? v)
      (u/throw! "workflow stopped due to timeout!")
      v)))


;;
;; WoofResultProcessor

;;
;; where options
;; {
;;   ::timeout <ms>
;;
;;   ; custmer handler for each result message
;;
;;   :error
;;   :done
;; }

(defrecord FutureWF [executor options]
  wf/WoofResultProcessor

  ;; clojure only!
  (process-results! [this]
    #?(:clj
       ;; TODO: how to handle timeouts, as :timeout?
        (let [t (get this ::timeout 5000)]
          (future
            (let [timeout-chan (async/timeout t)
                  result-chan (proto/execute! executor)]

              (go-loop []
                       (if-let [[status data] (async/<! result-chan)]
                         (condp = status
                           :error (do
                                    ;; TODO: error handling strategies
                                    (async/>! timeout-chan data) ;; send the exception as a result
                                    (u/throw! data)              ;; re-throw it, so wf will stop
                                    )
                           :done (async/>! timeout-chan data)

                           (let [custom-handler (get options status)]
                             (if custom-handler
                               (custom-handler data))
                             (recur))
                           )))

              (process-wf-result (async/<!! timeout-chan)) ;; todo: close channel?
              ))
          )))

  ;; js promise?
  )


#?(:clj

   ;; can this be done as tranducer?
   (defn sync-execute!
     "executes workflow and returns result as a future"
     ;; TODO: process-results-sync!
     ([executor]
      (wf/process-results! (->FutureWF executor {})))
     ([executor t]
      (wf/process-results!
        (assoc (->FutureWF executor {}) ; pass the optional params
          ::timeout t))
       )
     ([executor t options]
      (wf/process-results!
        ;; funny way of passing optional params
        (assoc
          (->FutureWF executor options)
          ::timeout t))
       )
     )
   )


#_(defrecord ThreadWF [xtor opts]
  wf/WoofResultProcessor

  (process-results! [this]
    #?(:clj

        (let [*wf (atom nil)
              thread-blocker (async/chan 1)
              t (async/timeout 1500)


              ]

          ;; blocking get
          (let [[v c] (async/alt!! [thread-blocker t])]
            (println v)
            )


          ;; (async/<!! thread-blocker)

          )

       )
    )
  )



