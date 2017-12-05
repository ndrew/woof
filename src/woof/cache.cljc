(ns woof.cache
  "cache for workflow executor"
  (:require
    [woof.data :as d]
    [woof.utils :as u]

    #?(:clj  [clojure.core.async :as async :refer [go go-loop]])
    #?(:cljs [cljs.core.async :as async]))

  #?(:cljs
      (:require-macros [cljs.core.async.macros :refer (go go-loop)])))


(defprotocol ICache

  (get! [this id params])
  (put! [this id params v])

  (memoize! [this f id]))


(defn- run-and-memoize [this k f params]
  ;#?(:cljs (println "run-and-memoize" k params))

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
            ;; (println "GOT RESULT: " async-result)
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




;; TODO: move this to test


#_(let [*cache (atom {})
        cache (->Cache *cache)
        sync-f (fn [params]
                 (println "sync(" params ")")
                 (str "hello " params))

        c (async/chan)
        async-f (fn [params]
                  (println "async(" params ")")
                  (go
                    (timeout 1000)
                    (async/put! c (str "ahello " params)))
                  c)]


  ;(put! cache :foo :bar :baz)
  ;(get! cache :foo :bar)
   ((memoize! cache sync-f :foo) :bar)
   ((memoize! cache sync-f :foo) :bar)
   ((memoize! cache async-f :foo) :bar)

   (let [nf (memoize! cache async-f :async-foo)]
     (println (d/pretty @*cache))
     (println "1" (nf :bar))
     (println (d/pretty @*cache))
     (println "2" (nf :bar))
     (timeout 2000)
     (println "3" (nf :bar)))




   #_(go
      (timeout 2000)
      (println (d/pretty @*cache))
      (println "result!" ((memoize! cache async-f :foo :bar) :bar))))
