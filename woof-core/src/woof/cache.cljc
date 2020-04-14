(ns woof.cache
  "cache for workflow executor"
  (:require
    [woof.utils :as u]

    #?(:clj  [clojure.core.async :as async :refer [go go-loop]])
    #?(:cljs [cljs.core.async :as async]))

  #?(:cljs
      (:require-macros [cljs.core.async.macros :refer (go go-loop)]))
  (:gen-class)
  )

;; is this still needed?

(defprotocol ICache

  (get! [this id params])
  (put! [this id params v])

  (memoize! [this f id]))


(defn- run-and-memoize [this k f params]
  (let [res (f params)] ;; TODO: rethrow exception here
    (if-not (u/channel? res)
      (do
        (put! this k params res)
        res)
      (let [memoize-transducer (fn[rf] (fn
                                         ([] (rf))
                                         ([result] (rf result))
                                         ([result v]
                                          (put! this k params v)
                                          (rf result v))))

            cached-chan (async/chan 1 memoize-transducer)]

        (go
          (let [async-result (async/<! res)]
            (async/put! cached-chan async-result)))
        cached-chan))))



(defrecord Cache [*cache-map]
  ICache
  (get! [this id params]
       (if-let [result (get @*cache-map [id params])]
         result))

  (put! [this id params v]
       (swap! *cache-map assoc [id params] v))

  (memoize! [this f id]
            (fn [params]
              (let [k (keyword (str "f-" id))] ;; TODO: move cache id keyword generation somewhere else
                (if-let [v (get! this k params)]
                  v ; cache hit
                  (run-and-memoize this k f params))))))




