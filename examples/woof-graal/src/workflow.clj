(ns workflow
  (:require ;[clojure.tools.cli :as cli]
            [clojure.core.async :as async]
            [woof.base :as woof]
            [woof.utils :as u]
            [clojure.string :as string]
    )
  (:gen-class))

;(set! *warn-on-reflection* true)

(defn sleep-and-return [timeout v] 
  (let [ch (async/chan)]
    (async/go
      (async/<! (u/timeout timeout))
      (async/put! ch v))
  ch))


(defn run-test-wf[] 
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
                         }

        steps-map {
                    ::print  [:print "Hello Woof GraalVM"]
                  }

        wf (woof/wf! :ctx ctx-map 
                     :steps steps-map
                     :opts opts)]

;; run async
    (woof/run-wf! wf)
    )
  )


(def CTX-MAP
  {
   :print {:fn (fn [v]
                 (prn v)
                 v)}

   :sleep {:fn (fn [[t v]]
                 (sleep-and-return t v))}
   }
  )

(defn run-test-wf-sync []
  (let [wf (woof/wf!
             :ctx CTX-MAP
             :steps {
                     ::v     [:sleep [1000 "Hello Woof GraalVM"]]
                     ::print [:print ::v]
                     })]

    ;; run async
    (let [results @(woof/sync-run-wf! wf)]
      (println "===results===")
      (println (woof/pretty! results))
      (println "===inlined===")
      (println (woof/pretty! (woof/inline-results results)))

      (shutdown-agents)
      )
    )
  )



(defn -main [& args]

  ;; (prn (str "start workflow. clojure" (clojure-version)))
  ;; (prn (woof/rand-sid))

  (run-test-wf-sync)


  ;; async wf

  #_(let [ch (async/chan)]

    (async/go 
        (async/<! (u/timeout 5000))
        (async/put! ch "value")

        )

    ;; run test wf in separate thread
    (async/thread
        (run-test-wf))


    (async/<!! ch)

    (prn "EXITING")
    )



  )
