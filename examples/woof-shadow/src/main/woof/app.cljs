(ns woof.app
      (:require [clojure.core.async :as async]
                [woof.base :as woof]
                [woof.utils :as u]
                [clojure.string :as string])
      )

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
                       ::print  [:print "Hello Woof Shadow-CLJS"]
                       }

            wf (woof/wf! :ctx ctx-map
                         :steps steps-map
                         :opts opts)]

           ;; run async
           (woof/run-wf! wf)
           )
      )

(defn init []
      (println "Hello World")
      (prn (woof/rand-sid))

      (run-test-wf)
      )


;; stop start metadata

(defn ^:dev/before-load stop []
      (js/console.log "stop"))

(defn ^:dev/after-load start []
      (js/console.log "start")
      (run-test-wf)
      )