(ns woof.simple-tests
  (:require
    [cljs.core.async :as async]
    [cljs.test :refer-macros [use-fixtures deftest is testing async]]


    [woof.alpha.wf :as awf]
    [woof.v2.wf.stateful :as st-wf]
    [woof.playground.state :as state]

    [woof.utils :as u]

    [woof.base :as base]
    [woof.data :as d]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]])
  )


(defn test-wf [init-fns
               ctx-fns
               steps-fns
               opt-fns
               ]

  (async ready
    (let [wf-impl
          (base/parametrized-wf!
            (base/combine-init-fns init-fns)
            identity                                        ;wf-params-fn
            identity                                        ;opt-params-fn
            (base/combine-fns
              (conj opt-fns
                    (fn [params]
                                 {
                                  ;; timeout

                                  ;; todo: for now use this, even this one is called before done/error
                                  :after-process (fn [exec-chann]
                                                   ;; (.log js/console ::after-process)
                                                   (ready)
                                                   exec-chann)

                                  ;; todo: these will be overriden
 ;                                 :done  (fn [result]
 ;                                                           (.log js/console ::after-process)
 ;                                                           (ready))
 ;                                 :error (fn [result]
 ;                                          (.log js/console ::after-process)
 ;                                          (ready))
                                  })



                    )
              :merge-results base/merge-opts-maps)
            (base/combine-fns ctx-fns)
            (base/combine-fns steps-fns))
          ]
      (base/run-wf! wf-impl identity)
      )))


;; todo: start with minimal wf example
;; init-fn + opts-fn + ctx-fn + steps-fn



(deftest simplest-wf-1

  (let [init-fn (fn [params] {
                              :t (u/now)
                              })

        ctx-fn (fn [params]
                 {
                  :id {:fn (fn [v]
                             ;(prn v (mod v 2))
                             #_(if (= 1 (mod v 3))
                                 (u/throw! "aaaaaaaa")
                                 v
                                 )
                             v

                             )}
                  })


        steps-fn (fn [params]
                   {
                    ::ping [:id (:t params)]
                    }
                   )


        opts-fn (fn [params]
                  {

                   :op-handlers-map {
                                     :done  (fn [result]
                                              (.log js/console ::done (d/pretty result))

                                              ;; (is (= :foo :bar))
                                              )
                                     :error (fn [result]
                                              (.log js/console ::error (d/pretty result))

                                              )
                                     }
                   })]

    (test-wf [init-fn]
             [ctx-fn]
             [steps-fn]
             [opts-fn]
             )


    )
  )




(defn simplest-wf-initializer [*SWF]

  ;;
  (prn "initializer")
  {

   :init-fns   [(fn [params]

                  (prn "init-fns")

                  {:IN :some-wf-parameters}
                  )
                ]

   :ctx-fns    [(fn [params]
                  {
                   :test  {:fn (fn [v] v)}
                   :wait {
                          :fn       (fn [t]
                                      (let [ch (async/chan)]

                                           (go
                                             (async/<! (u/timeout t))
                                             (async/put! ch "DONE")
                                             )

                                           ch
                                           ))

                          }
                   }
                  )]

   :steps-fns  [(fn [params]
                  {
                   ::step [:test "Hello!"]
                   ::wait [:wait 300]
                   })

                ]
   :opt-fns    [
                (fn [params]
                    {
                     :op-handlers-map {
                                       :done (fn [result]
                                               (swap! *SWF assoc ::result result)
                                               (.log js/console "Test WF is DONE" result)
                                               )
                                       ;; :error close!
                                       }
                     })]


   }
  )


;; test runner based on async


(deftest alpha_wf_test__1

  (testing "stateful representation of the wf"
    ;; wrap wf to a state atom.
    ;; start with an empty (default) state map
    ;; then add needed aspects atop - for example ui
    
    ;; fixme: maybe there is some nicer way of wrapping state

    (let [initial-state (state/empty-swf :dummy)
          wf-init-fn simplest-wf-initializer


          t-fn (fn [*SWF]
                 (let [nu-wf-state (wf-init-fn *SWF)
                       updated-state (merge
                                       initial-state
                                       nu-wf-state)
                       ]

                      (swap! *SWF merge  updated-state)
                      (.log js/console @*SWF)
                      )
                 )
          ; produce a map with :woof.v2.wf.stateful/init-wf
          wf-map  (st-wf/wf :dummy t-fn)

          *wf (atom wf-map)
          ]

      ;; this one is ugly as we use state as source of the code

      ;; do init a wf
      (st-wf/wf-init! *wf)

      ;; run wf
      (state/swf-run! *wf)

      (async ready
        (do
          (prn "wait for wf..")
          (js/setTimeout (fn []
                           (.log js/console "FULL RESULT" @*wf)

                           (ready)
                           ) 3000))))))

