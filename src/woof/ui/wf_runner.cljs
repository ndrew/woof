(ns woof.ui.wf-runner
  (:require
    [cljs.core.async :as async]
    [cljs.test :as test :refer-macros [deftest is testing run-tests async]]

    [clojure.data :as cd]


    [goog.string :as gstring]
    [goog.string.format]

    [rum.core :as rum]

    [woof.app-data :as app-model]

    [woof.data :as d]
    [woof.graph :as g]
    [woof.wf :as wf]
    [woof.wf-data :as wdata]

    [woof.ui :as ui]
    [woof.wf-ui :as wf-ui]
    [woof.ui.context :as ctx-ui]
    [woof.ui.steps :as steps-ui]


    [woof.utils :as u]

    [woof.test-data :as test-data]
    ; [woof.wf-tester-ui :as tester-ui]
    )


  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]
    [woof.utils-macros :refer [put!?]]))


(defn- DBG [a]
  (.warn js/console (d/pretty a)))



;;
;; initializers - context, steps


(defn- get-steps [ui-loop-chan]
    {
    ;; todo: why wf is not updated on async expand
    ;  ::t [:timeout 7000]
    ;  ::end [:id ::t]
;;    ::hello [:id "woof!"]
    ::ui    [:ui-loop ui-loop-chan]

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


(defn- get-context-map []
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



;;
;; state


(defonce *UI-STATE (atom {

  :steps {}
  :context-map {}

  :status :woof.app/not-started
  :wf nil


  :ui-chan nil
  :history []
}))


(def cursor (partial rum/cursor-in *UI-STATE))

(defn init-wf []

  (let [context-map (get-context-map)]

    (swap! *UI-STATE merge {
                             :context-map context-map
                             })
    ;; todo: autostart
))



;;
;;
;; actions

(defn send-ui-action [steps]
  (go
    (async/>! @(cursor [:ui-chan]) steps)))


(defn- start-wf []

  (let [ui-loop-chan (async/chan)
        steps (get-steps ui-loop-chan)


        context (wf/make-context @(cursor [:context-map]))
        executor (wf/build-executor context steps)

        p { ;:execute start-wf-with-transducers!
            ; :before-process before-processing!
            ;; :process-handler op-handler
            :op-handlers-map {
                               :process (fn[data]
                                          (swap! (cursor [:history]) conj
                                                 (wdata/inline-results data)
                                                 )
                                          ;(swap! *result merge data)
                                          )
                               :done (fn [data]
                                       (swap! (cursor [:history]) conj
                                                 (wdata/inline-results data))

                                       (reset! (cursor [:status]) :woof.app/done)
                                        ; (async/close! @(cursor [:ui-chan]))
                                       )
                               :error (fn [data]
                                        (.error js/console "ERROR" data)
                                        (reset! (cursor [:status]) :woof.app/error)
                                        )

                               }
            ;; :timeout 1000
            }

        res-chan (wf/process-results! (wf/->ResultProcessor executor p))]

    (swap! *UI-STATE merge {
                             :wf executor
                             :status :woof.app/running

                             :history []

                             :ui-chan ui-loop-chan
                             :steps steps
                             })
        )
  )


(defn- stop-wf []
  (wf/end! @(cursor [:wf])))


(defn- click-action []
  (send-ui-action
    { (wf/rand-sid) [:id (str "click - " (.getTime (js/Date.)))] }))




(defonce menu4status
  {
    :woof.app/not-started [["start" start-wf]]

    :woof.app/done        [["start" start-wf]]
    :woof.app/running     [
                            ["click" click-action]
                            ["emit!" (fn[]

                                       (let [step (d/to-primitive (js/prompt "provide step as [:handler-id <params>]"))]
                                       (send-ui-action
                                          { (wf/rand-sid)
                                            step
                                            }))
                                       )]
                            []
                            ["stop" stop-wf]
                           ]
    ; :woof.app/stopped     "error"
    :woof.app/error       [["start" start-wf]]
    })



(rum/defcs <wf-runner-ui> < rum/reactive
  (rum/local nil  ::update)
  [local *STATE]


  (let [status @(cursor [:status])
        menu-actions (get menu4status status [])]
    [:div.wfui
     [:h5 "Example of using workflow with infinite expand handler as ui-loop."]

     (ctx-ui/<context> (cursor [:context-map]))

     (steps-ui/<steps> (cursor [:steps]) @(cursor [:context-map]))

    [:div.main-menu
       [:span "  " (wf-ui/<wf-status-ui> status)]
       (ui/menubar "wf:" menu-actions)]


     (let [history (reverse @(cursor [:history]))]
       [:.log
        [:h2 "result:"]
        [:pre (d/pretty (first history))]
        [:h4 "last added"]
        [:pre
         (let [[cur prev] (first (partition 2 history))
               [added _ _] (cd/diff cur prev)
               ]
           (d/pretty added)

           )
         ]
        ]
       )

    ]
))
