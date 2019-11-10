(ns woof.common
  "commonly used context"
  (:require
    ; clj
    ;; [clojure.core.async :as async :refer [go go-loop]]
    ;; [clojure.data :as cd]
    ; woof
    [woof.base :as base]

    [woof.data :as d]
    [woof.utils :as u]
    ))



(defn stateful-wf [*state wf on-stop]
  {
   :wf        wf
   :state     *state

   :start-wf! (fn []
                (base/run-wf! wf identity))
   :stop-wf!  (fn []
                (if-let [xtor (get-in @*state [:xtor])]
                  (do
                    (base/end! xtor)
                    (on-stop)
                    ::stopped)
                  (do
                    (prn ::no-wf-running)
                    ::no-wf-running)))
   }
  )


;; for now - I use build- prefix to distinguish between higher order functions and already ready ones?
;;

;; todo: duplicated in woof.playground.state
;; keep xtor in state as :xtor
(defn build-opt-keep-xtor [*state]
  (fn [params]
    {:before-process  (fn [wf-chan xtor]
                        (swap! *state assoc :xtor xtor)

                        :ok)})
  )

;; run on-done when wf is stopped (done/error)
(defn build-opt-on-done [on-done]
  (fn [params]
    {:op-handlers-map {
                       :done  (fn [result] (on-done))
                       :error (fn [result] (on-done))
                       }}))



;;
;; common step handlers


(defn edn-ctx
  ":EDN - returns data that had been passed to it"
  []
  {
   ;; just return the edn data
   :EDN  {
          :fn identity
          }
   }
  )

(defn map-ctx
  ":kv [[k v]]
   :map [kv..]"
  []
  {
   ;; build map by combining kv
   :kv  {
         :fn (fn [[k v]]
               {k v})
         }

   :map {
         :fn       (fn [maps]
                     (apply merge maps))
         :collect? true
         }

   }
  )