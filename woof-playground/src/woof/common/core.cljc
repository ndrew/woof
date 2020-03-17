(ns woof.common.core
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


;;
;; state aspect
;;


;; for now - I use build- prefix to distinguish between higher order functions and already ready ones?
;; other option is to use funtion_ notation - to show that it has to be 'partial'-ed

;;
;; state wf aspect - injects atom into workflow
;;
(defn build-init-state-fn [*STATE]
  (fn [_] {::state *STATE}))

;; keep xtor in state as ::xtor
(defn build-opt-state-fn [*state]
  (fn [params]
    {:before-process  (fn [wf-chan xtor]
                        (swap! *state assoc ::xtor xtor)

                        :ok)})
  )

;;
(defn &state [params]
  (::state params))

(defn state-get-xtor [*state]
  (get-in @*state [::xtor]))




(defn stateful-wf
  ([*state wf on-stop]
   (stateful-wf *state wf on-stop {}))
  ([*state wf on-stop api-map]
   (merge api-map
          {
           :wf        wf

           :state     *state

           :start-wf! (fn []
                        (base/run-wf! wf identity))

           :stop-wf!  (fn []
                        (if-let [xtor (state-get-xtor *state)]
                                (do
                                  (base/end! xtor)
                                  (on-stop)
                                  ::stopped)
                                (do
                                  (prn ::no-wf-running)
                                  ::no-wf-running)))
           }
          )
   )
  )



;; run on-done when wf is stopped (done/error)
(defn build-opt-on-done [on-done]
  (fn [params]
    {:op-handlers-map {
                       :done  (fn [result] (on-done result))
                       :error (fn [result] (on-done result))
                       }}))


;;
;; channel factory
;;

(defn build-init-chan-factory-fn [cf]
  (fn [_]
    {
     ::channel-factory cf
    }))

(defn &chan-factory [params]
  (if-let [cf (get params ::channel-factory)]
    cf
    (u/throw! "no ::cf provided in params. Ensure that chan-factory-init-fn had been called" )
    ))




(defn build-chan-factory-opts [channel-factory]
  (build-opt-on-done
    (fn [result]
      (base/close-chans! channel-factory)
      result)))






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