(ns woof.app-data
  "model for woof tester"
  (:require
    [cljs.core.async :as async]

    [woof.utils :as u]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]
    [woof.utils-macros :refer [put!?]])
  )



(defn default-context []
  {
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
