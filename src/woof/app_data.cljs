(ns woof.app-data
  "model for woof tester"
  (:require
    [cljs.core.async :as async]

    [woof.utils :as u]
    [woof.wf :as wf]

    [woof.wf-ui :as wf-ui] ;; todo: move to different ns
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]
    [woof.utils-macros :refer [put!?]])
  )




(defn default-context []
  {

    :clojure {:fn (fn [a]
                    a
                    )
              :server? true
              }


    :identity {:fn (fn [a] a)}

    :identity-async {:fn (fn [a]
                           (let [c (async/chan)]
                             (go
                               (async/put! c a))
                             c))}
    :hello {:fn (fn [a]
                  (let [c (async/chan)]
                    (go
                      (async/put! c (str "Hello! " (pr-str a))))
                    c))}

    :hello-wait {:fn (fn [a]
                       (let [c (async/chan)]
                         (go
                           (async/<! (u/timeout 5000))

                           (async/put! c (str "Hello! " (pr-str a))))
                         c))}

    :8 {:fn (fn [max-num]
              ;; (.warn js/console "max-num" max-num)

              ; (str "Hello " s "!")
              (let [chan (async/chan)
                    t (volatile! (u/now))]

                (go-loop [i 0]
                         (async/<! (u/timeout 500))

                         ;; (.warn js/console "i" i (< i max-num) (- (u/now) @t) )
                         (vreset! t (u/now))

                         (async/>! chan (str i ": " (int (rand 100))))

                         (if (< i max-num)
                           (recur (inc i))))

                chan))
        :infinite true
        }
    }
  )




(defprotocol IWF

  ; adds/replaces context
  (merge-context [this xpand-context])

  ; adds/replaces steps
  (merge-steps   [this xpand-steps])


  ; returns underlining context map atom
  (get-context* [this])


  ; returns workflow config
  (get-workflow-cfg* [this])


  (get-xctor [this])
  (set-xctor [this xctor])

  (get-xctor-chan [this])
  (set-xctor-chan [this xctor-chan])


  (start! [this callback])

  )



(defn wf-state [*context *workflow-cfg *xctor *xctor-chan]

  (let [CONTEXT (wf/make-context *context)]
    (reify IWF

      (merge-context [this new-context]
                     (swap! *context merge new-context))

      (merge-steps   [this new-steps]
                     (swap! *workflow-cfg update-in [:steps] merge new-steps)
                     )


      (get-context* [this] *context)
      (get-workflow-cfg* [this] *workflow-cfg)


      (get-xctor [this] @*xctor)
      (set-xctor [this xctor] (reset! *xctor xctor))

      (get-xctor-chan [this] @*xctor-chan)
      (set-xctor-chan [this xctor-chan] (reset! *xctor-chan xctor-chan))


      (start! [this callback]

              ;(wf/get-)

              (set-xctor this (wf/build-executor CONTEXT (:steps @*workflow-cfg)))

              (let [exec-chan (wf/time-updated-chan ;; todo: move to protocol
                                (wf/execute! (get-xctor this))
                                wf-ui/UI-UPDATE-RATE)]

                (set-xctor-chan this exec-chan)

                (callback this)
                )
              )
      )
    )
  )

