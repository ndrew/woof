(ns woof.node
  (:require 
    [cljs.nodejs :as nodejs]

    [clojure.core.async :as async]
    [woof.base :as woof]
    [woof.utils :as u]
    
    ))

(nodejs/enable-util-print!)


(defn sleep-and-return [timeout v] 
  (let [ch (async/chan)]
    (async/go
      (async/<! (u/timeout timeout))
      (async/put! ch v))
  ch))


(defn run-test-wf[] 
  
  ;; check that woof library is working
  
  (println (woof/rand-sid))
  (let [
        opts {
                :op-handlers-map {
                     :done  (fn [result]
                              (println "WF IS READY:")
                              (println (woof/pretty! result))

                              result)

                     :error (fn [result]
                              (prn result))
                     }
        }
        ctx-map {
                         :print {:fn (fn [v]
                          (prn v)
                          v
                          )}
                       :sleep {:fn (fn [[t v]]
                            (sleep-and-return t v))}


                         }

        steps-map {
                     ::v     [:sleep [1000 "Hello Woof Node"]]
                     ::print [:print ::v]
                  }

        wf (woof/wf! :ctx ctx-map 
                     :steps steps-map
                     :opts opts)]

    ;; run async
    (woof/run-wf! wf)
    )
  )


(defn -main [& args]
  (run-test-wf))


(set! *main-cli-fn* -main)