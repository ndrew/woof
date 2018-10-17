(ns woof.wfc
  "woof workflow composition"
  (:require [woof.data :as d]
            [woof.wf :as wf]

            #?(:clj [woof.utils :as u :refer [put!? debug! inline--fn inline--fn1]])
            #?(:cljs [woof.utils :as u])


            #?(:clj [clojure.core.async :as async :refer [go go-loop]])
            #?(:cljs [cljs.core.async :as async])
            )

  #?(:cljs
      (:require-macros
          [cljs.core.async.macros :refer [go go-loop]]
          [woof.utils-macros :refer [put!? debug! inline--fn inline--fn1]]
        )))




;;
(defprotocol WoofWorkflow

  (get-params [this])
;; get args?

  (get-context-map [this])

  (get-steps [this])


)


;; creates a workflow via params map
;;  -> (context-fn params...)
;;  -> (steps-fn params...)

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


;; todo: ring style chaining
 ;; params-fn -> context-fn -> steps-fn


;; todo: composing wfs

;;

(defn wf-xtor [wwf]
  (wf/build-executor
    (wf/make-context (get-context-map wwf))
                      (get-steps wwf)))

;; FIXME: deprecated, use ResultProcessor directly - as we need the xtor e.g. for ending the wf
(defn wf-async-process! [wwf & [opts]]
  (let [xtor (wf-xtor wwf)
        processing-opts (if (nil? opts) {} opts)
        ]

    (wf/process-results! (wf/->ResultProcessor xtor processing-opts))
    )
  )


