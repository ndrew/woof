(ns woof.playground.old.example.infinite
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]

    [woof.data :as d]
    [woof.wf :as wf]

    [woof.utils :as u]
    [woof.ui :as ui]

    [woof.playground.old.wf-ui :as wf-ui]

    [woof.playground.old.results :as r]


    [markdown.core :refer [md->html]]

    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



(defn- close-channels! [r]
  (doall
    (doseq [c r]
      (when (u/channel? c)
        (async/close! c)
        )
    ))
  )



(defn chan-factory []
  (let [store (atom [])]

    (fn
      ([]
      (let [c (async/chan)]
        (swap! store conj c)
        c
        ))
      ([close]
       (close-channels! @store)
       (reset! store nil)
       )
      ))
  )


(defn prepare-params! []
   {
     ;; init the channel, so it will be properly closed
     :chan-factory (chan-factory)
     }
  )

;; factory for channels


(defn context-map-fn [& {:keys [chan-factory]}]
  {
    :identity {:fn (fn [a] a)}

    :identity-async {:fn (fn [a]
                           (let [c (chan-factory)]
                             (go
                               (async/put! c a))
                             c))}
    :hello {:fn (fn [a]
                  (let [c (chan-factory)]
                    (go
                      (async/put! c (str "Hello! " (pr-str a))))
                    c))}

    :hello-wait {:fn (fn [a]
                       (let [c (chan-factory)]
                         (go
                           (async/<! (u/timeout 5000))

                           (async/put! c (str "Hello! " (pr-str a))))
                         c))}

    :8 {:fn (fn [max-num]
              (let [chan (chan-factory)
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


    :xpand-8 {:fn (fn [s]
             (let [chan> (chan-factory)]
                (go []
                 (let [v (async/<! (u/timeout 1500))]

                   (async/put! chan>
                               {::x1 [:8 20]
                                ::x2 [:hello ::x1]
                                })))
             chan>)
           )
     :expands? true}
    }
  )


(defn steps-fn [& {:keys []}]
  {
    ::xpnd [:xpand-8 {}]

    ::8 [:8 10]
    ::i [:identity ::8]
    ::h [:hello ::i]    }
  )


(defn actions-fn [& {:keys [chan-factory]}]
    {
     ;; :start! (fn[])
     :stop!  (fn[]
               (chan-factory :close))
     ;; :reset! (fn[])

     :actions [

      ]
  })
