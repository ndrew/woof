(ns woof.client.browser.local-storage
  (:require
    [cljs.core.async :as async]

    [woof.utils :as u]
    [woof.wf :as wf]
    [woof.client.stateful :as st-wf]

    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


(defn &ls-watcher-map [params]
  (if-let [watch-map (::watch-map* params)]
    watch-map
    (u/throw! "no ::watch-map* provided in the params")))


(defn ls-init-fn [params]
  (let [watch-map* (atom {})
        POOL-INTERVAL 1000]
    {::watcher-id (js/setInterval (fn[]
                                    (let [watchers @watch-map*]
                                      (doseq [[k [v ch]] watchers]
                                        (let [nu-v (.getItem js/localStorage k)]
                                          (when (not= nu-v v)
                                            (swap! watch-map* assoc k [nu-v ch])
                                            ;; todo: handle nil
                                            (async/put! ch nu-v)
                                            )
                                          ))))
                                  POOL-INTERVAL)
     ::watch-map* watch-map*}
    )
  )


(defn ls-ctx-fn [params]
  {
   :ls-write {:fn (fn[[k v]]
                    (.setItem js/localStorage k v)
                    v)}

   :ls-read {:fn (fn[k]
                   (.getItem js/localStorage k))}

   :ls-infinite-read {
                      :fn (fn [k]
                            (let [watcher-chan (st-wf/&chan params (wf/rand-sid (str "ls-watch-" k)))
                                  current-v (volatile! (.getItem js/localStorage k))]

                              (swap! (&ls-watcher-map params) assoc k [@current-v watcher-chan])
                              (if-not (nil? @current-v)
                                (go
                                  ;; todo: will this timeout be enough?
                                  (async/<! (u/timeout 1))
                                  (async/>! watcher-chan @current-v))
                                )
                              watcher-chan
                              )
                            )
                      :infinite true
                      }
   })


(defn ls-opts-fn [params]
  (let [close! (fn [result]
                 (js/clearInterval (::watcher-id params))

                 result
                 )]
    {
     :op-handlers-map {
                       :done close!
                       :error close!
                       }
     }))