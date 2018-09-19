(ns woof.example.ui-loop
  (:require
    [cljs.core.async :as async]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.utils :as u]

    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]
    [woof.utils-macros :refer [put!?]])
  )

;;
;; UI loop workflow

(defn- DBG [a]
  (.warn js/console (d/pretty a)))


;;
;; initializers - context, steps

(defn steps-fn
  ""
  [& {:keys [ui-chan]}]
    {
    ;; todo: why wf is not updated on async expand
    ;  ::t [:timeout 7000]
    ;  ::end [:id ::t]
;;    ::hello [:id "woof!"]
    ::ui    [:ui-loop ui-chan]

;;;;;

    ;;::addp [:+< [1 2 3]]
    ;;::add [:+> ::addp]

    ::1 [:v 1]
    ::2 [:v 2]

    ::add [:+> [::1 ::2]]

    }
  )


(defn- get-math-context-map []

  {
    :+< {:fn (fn [xs]
                   (into (array-map)
                         (map-indexed (fn [i x]
                                        [(wf/rand-sid) [:v x]]) xs)))
             :expands? true
             }

    :v {:fn (fn [x] x)}

    :+> {
               :fn (fn[xs]

                     (reduce + xs))
               :collect? true
               }



}
  )



(defn context-map-fn [& {:keys [ui-chan]}] ;;
  (merge
  {
    :id (wf/step-handler (fn [a]
                           (DBG a)
                           a))

    :exception (wf/step-handler (fn [a]
                           (.warn js/console a)
                           (throw (js/Error. (str "Error: " (d/pretty a))))
                           a))

    :timeout (wf/step-handler (fn [t]
                                ; (str "Hello " s "!")
                                (let [chan (async/chan)]
                                  (go
                                    (async/<! (u/timeout t))
                                    (async/put! chan "timeout!")
                                    )

                                  chan)))



    :ui-loop {:fn (fn [in-chan]
                    (u/wiretap-chan in-chan (partial println "UI LOOP:")))

              :infinite true
              :expands? true
              }

    }
    (get-math-context-map)
    )
  )



(defn actions-fn [& {:keys [ui-chan]}]
  (let [send-ui-action (fn [steps]
                          (go
                            (async/>! ui-chan steps)))]
    {;; :start! (fn[])
     :stop!  (fn[]
               (println "close")
               (async/close! ui-chan)
               )
     :reset! (fn[]
               (println "reset!")
               )

     :actions [
      ["click" (fn []
                 (send-ui-action
                   { (wf/rand-sid) [:id (str "click - " (.getTime (js/Date.)))] })
                 )]
      ["emit!" (fn[]
                 (let [step (d/to-primitive (js/prompt "provide step as [:handler-id <params>]"))]
                   (send-ui-action
                     { (wf/rand-sid)
                       step
                       }))
                 )]

      ]
     }))
