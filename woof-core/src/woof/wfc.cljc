(ns woof.wfc
  "woof workflow composition"
  (:require [woof.data :as d]
            [woof.wf :as wf]

            #?(:clj [woof.utils :as u :refer [put!? debug! inline--fn inline--fn1]])
            #?(:cljs [woof.utils :as u])


            #?(:clj [clojure.core.async :as async :refer [go go-loop]])
            #?(:cljs [cljs.core.async :as async])

            ; [woof.core.protocols :as protocols :refer [WoofWorkflow get-params get-context-map get-steps]]
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

;; todo: migrate params to a function
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
  (let [ctx (wf/make-context (get-context-map wwf))
        steps (get-steps wwf)]
    ;; returns executor
    (wf/build-executor
      ctx
      steps)
    )
  )



;; FIXME: deprecated, use ResultProcessor directly - as we need the xtor e.g. for ending the wf
(defn wf-async-process! [wwf & [opts]]
  (let [xtor (wf-xtor wwf)
        processing-opts (if (nil? opts) {} opts)
        ]

    (wf/process-results! (wf/->ResultProcessor xtor processing-opts))
    )
  )



#_(let [params {}
      context-fn (fn [& opts]
                   {:hello {:fn (fn[x]
                                  (println x)
                                  x
                                  )}})
      steps-fn (fn[& opts]
                 {::hello [:hello "woof!"]})

      wwf (params-wf params context-fn steps-fn)

      ]
  (wf-async-process! wwf
                     {
                       :before-process (fn[wf-chan xtor]
                                         (println "STARTED!")
                                         )
                       :op-handlers-map {
                          :done (fn [data]
                                 (println "DONE!\n" (d/pretty data)))

                          :error (fn [data]
                                  (println "ERROR!\n" (d/pretty data))
                                   )
                          }
                       })
  )



#_(defn init-runner-wf!
  "initializes ui with specific ui workflow
   *STATE     - map in atom, ui state, for rum updates
   params     - workflow map of wf parameters

   context-fn - (fn [& {:keys [...]}]) -> context-map
   steps-fn   - (fn [& {:keys [...]}]) -> initial steps map

   actions-fn - (fn [& {:keys [...]}]) -> {
      :start! (fn[]) -> channel/nil ;  if channel — wait for it before starting wf
      :stop! (fn[]) -> ; clean-up
      :reset! (fn[]) -> ; some global clean-up
      :actions <menu-actions> ; available actions during wf in format as for menus
   }

   ui-fn      - (fn [& {:keys [...]}])
  "
  [*STATE
   params

   context-fn
   steps-fn
   actions-fn

   ui-fn
   & {:keys [auto-start]}
   ]

  (let [args (apply concat params) ;; maybe add here some other params

        context-map (apply context-fn args)
        steps (apply steps-fn args)

        xtor (wf/build-executor (wf/make-context context-map) steps)

        wf-actions (runner-actions *STATE (apply actions-fn args) xtor)]


    (swap! *STATE merge
           {
             :wf (merge
                   {
                     :steps steps
                     :context-map context-map
                     }
                   wf-actions)

             :status :woof.app/not-started
             :history []

             :rum-ui (apply ui-fn args)
             :wf-args args
             })

    (if auto-start
      ((:start! wf-actions)))

    )
  )




#_(let [
       prepare-params-fn (fn [{:keys [foo]} & hhh]
                           (into {} (map vec (partition 2 hhh) ))
                           )
       ;; base params from the ws endpoint
       params {
                :_socket-chan :foo;;socket-chan

                :send! (fn [v]
                         ;(httpkit/send! socket-chan v)
                         )

                :send-transit! (fn [v]
                                 ;(httpkit/send! socket-chan (write-transit-str v))
                                 )

                :on-socket-close (fn [& args]
                                   ;(apply u/close-channels! args)
                                   )

                } ;; (blog/prepare-params socket-chan)


       new-params (apply prepare-params-fn (apply concat params))

       ;args (apply concat new-params) ;; maybe add here some other params

       ;context-map (apply context-fn args)
       ;steps (apply steps-fn args)
       ]

  #_(let [xtor (wf/build-executor (wf/make-context context-map) steps)]

    ;; (println "SOCKET" (d/pretty socket-chan))

    (httpkit/on-receive socket-chan
                        (fn [payload]

                          ;;

                          #_(let [msg (read-transit-str payload)]
                              ; (println "SERVER: got " (d/pretty msg))
                              (apply blog/msg-fn (concat args [:msg msg] )
                                     ))

                          ))

    (httpkit/on-close socket-chan
                      (fn [status]
                        ; (println "SERVER: end wf")
                        (wf/end! xtor)
                        ;; maybe, get the wf results some how
                        ;;
                        ;; (apply blog/close-fn args)

                        ))


    ;; run the workflow
    (wf/process-results! (wf/->ResultProcessor xtor {})) ;; todo: processing opts
    )

  new-params
  )
