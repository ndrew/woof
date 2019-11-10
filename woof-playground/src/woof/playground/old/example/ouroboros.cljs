(ns woof.playground.old.example.ouroboros
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]

    [woof.data :as d]
    [woof.wf :as wf]

    [woof.utils :as u]

    [woof.playground.old.wf-ui :as wf-ui]

    [woof.playground.old.results :as r]


    [markdown.core :refer [md->html]]

    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



(defn prepare-params! []
   {
     ;; init the channel, so it will be properly closed
      :chan> (async/chan)
     }
  )



(defn context-map-fn [& {:keys [chan>]}]
  {
    :in {:fn (fn[msec]


              (go-loop []
                  (async/put! chan> (u/now))
                  (async/<! (u/timeout msec))

                  (recur)
                  )
              chan>
              )
        :infinite true}

   :out {:fn (fn[x]
               (str "current time is " x))
         }
   }
  )


(defn steps-fn [& {:keys []}]
  {
    ::IN  [:in 1000]
    ::OUT [:out ::IN]
    }
  )


(defn- close-channels! [r]
  (doall
    (doseq [c r]
      (when (u/channel? c)
        (async/close! c)
        )
    ))
  )

(defn actions-fn [& r]
    {
     ;; :start! (fn[])
     :stop!  (fn[]
               (close-channels! r))
     ;; :reset! (fn[])

     :actions [

      ]
  })
