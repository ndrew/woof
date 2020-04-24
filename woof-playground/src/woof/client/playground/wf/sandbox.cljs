(ns ^:figwheel-hooks woof.client.playground.wf.sandbox
  (:require
    [cljs.core.async :as async]
    [rum.core :as rum]

    [woof.client.playground.ui.wf :as wf-ui]
    [woof.client.stateful :as st-wf]
    [woof.data :as d]
    [woof.wf :as wf]
    [woof.utils :as utils]
    [woof.base :as base])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))

;;
;;
;; use this workflow as a sandbox for
;;
;;

;;
;; context


(defn sandbox-init-params-fn [params]
  {
   }
  )

(defn sandbox-ctx-fn [params]
  (let [make-chan (partial st-wf/&chan params)]
    {
     :identity   {:fn (fn [a] a)}

     :hello      {:fn (fn [a]
                        (let [c (make-chan (wf/rand-sid))]
                             (go
                               (async/put! c (str "Hello! " (pr-str a))))
                             c))}

     :meta-hello {
                  :fn (fn [a]
                        )
                  }

     :hello-wait {:fn (fn [a]
                        (let [c (make-chan (wf/rand-sid))]
                             (go
                               (async/<! (utils/timeout 3000))

                               (async/put! c (str "Hello! " (pr-str a))))
                             c))}

     ;; return time for max-num times
     :8          {:fn       (fn [max-num]
                              (let [chan (make-chan (wf/rand-sid))
                                    t (volatile! (utils/now))]

                                   (go-loop [i 0]
                                            (async/<! (utils/timeout 500))

                                            ;; (.warn js/console "i" i (< i max-num) (- (u/now) @t) )
                                            (vreset! t (utils/now))

                                            (async/>! chan (str i ": " (int (rand 100))))

                                            (if (< i max-num)
                                              (recur (inc i))))

                                   chan))
                  :infinite true
                  }


     ;; the expand step emits several values — question: how to use this in the end

     :xpand-8    {:fn       (fn [cfg]
                              (let [chan> (make-chan)]
                                   (go []
                                       (let [v (async/<! (utils/timeout 1500))
                                             producer-sid (base/rand-sid "xpand-intermediary-")
                                             consumer-sid (base/rand-sid "xpand-result-")
                                             ]

                                         ;; here we expand once
                                         (async/put! chan>
                                                     {producer-sid [:8 20]
                                                      consumer-sid [:hello producer-sid]
                                                      })))
                                   chan>)
                              )
                  :expands? true}

     :xpand-8-result {
                 :fn (fn [sids]
                       (let [nu-steps (reduce (fn [a sid]
                                         (if (clojure.string/starts-with? (name sid) "xpand-result-")
                                             (assoc a (base/rand-sid "result") [:identity sid])
                                             a))
                                       {} sids)]

                            nu-steps
                            )
                       )
                 :expands? true
                 }

     :first-collect {
                      :fn (fn [v]

                            (if (= :nil v)
                                ;; do we need to throw exception here?
                                (utils/throw! "empty sid list provided for :first-collect"))
                            ;(.log js/console :first-collect v)

                            (if (and (seq? v) (seq v))
                                (first v)
                                v)
                            )

                      :collect? true
                      }

     }
    )

  )

;;
;; steps
(defn sandbox-steps-fn [params]

  {

   ::xpnd [:xpand-8 {}]

;
;  [:xpand-8 {}] -> ::xpand-intermediary-... [:8 20]
;                   ::xpand-result-.....     [:hello producer-sid]
;
;
   ::expand-result [:xpand-8-result ::xpnd]

   ;; (::xpand-result-..,::xpand-intermediary-..) -> (::xpand-result-..)

   ::result [:first-collect ::expand-result]

   ::evt-loop   [:infinite-expander (::evt-loop-chan params)]



   ;;::8 [:8 10]
   ;;::i [:identity ::8]
   ;;::h [:hello ::i]


   }
  )

;;
;; UI
;;
(rum/defcs <custom-ui> < rum/reactive
                         (rum/local true ::inline-results?)
                         (rum/local true ::sort-results?)
  [local *wf]

  (let [wf @*wf]
    [:div

     #_(if (not= :not-started (:status wf))
         [:div
          "you custom ui here"
          ]
         )
     ;   [:pre "↓ this is the default UI for displaying wf results ↓"]
     (wf-ui/<default-body> *wf)
     ]
    )
  )




(declare init-sandbox-wf!)

(defn sandbox-wf-init! [*NODE]
  (merge
    {
     :ui-fn       (partial wf-ui/<wf-UI> <custom-ui>)
     :title       "S A N D B O X"

     :explanation [:div.explanation
                   ;[:p "return context map via " [:code "sandbox-ctx-fn"]]
                   ;[:p "return steps via " [:code "sandbox-steps-fn"]]
                   ;[:p "custom UI in " [:code "<custom-ui>"]]

                   [:div

                    [:header "defining wf"]

                    [:p "create new state map for wf-id"]
                    [:p "generate init-fn/ctx-fn/steps-fn/opts-fn"]
                    [:p "how to add subst step ids - via special s-handler, via modifying steps before running wf"]

                    [:p "<?> should run be called after IN are configured??"]

                    [:p "generate IN/UI/OUT"]

                    [:p "<?>"]
                    [:p "a) will the wf be started manually, and then ended"]


                    [:header "IN"]
                    [:ul
                     [:li "can be returned via init-fn."]
                     [:p "<?> chaining init-fns?/restarting/etc"]
                     [:li "can be returned via s-handler."]

                     [:p "{::value-needed-for-wf [:IN-UI :in-key]}"]
                     [:p "in state: :in-cfg [ [:in-key :label 'Provide' :input-type '...' :etc] ...]"]
                     ]
                    [:p ]

                    [:header "UI"]
                    [:p ""]

                    [:header "OUT"]
                    [:p "::out in state set via done through opts-fn"]
                    [:p "::out in state set via separate step handler"]

                    [:p "duality between state change after wf ended/during wf execution"]
                    [:p "if wf is infinite - use s-handlers, else opts-fn"]

                    [:p "b) or this will be achieved via single infinite wf?"]

                    [:header "IN"]
                    [:p ""]

                    [:header "UI"]
                    [:p ""]

                    [:header "OUT"]
                    [:p ""]

                    [:hr ]
                    ]


                   ]
     }

    (init-sandbox-wf! *NODE)
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; impl





;; have an event loop, by default

(defn evt-init-fn [params]
  {
   ::evt-loop-chan (st-wf/&chan params (wf/rand-sid "evt-loop"))
   })


(defn evt-ctx-fn [params]
  {
   :infinite-expander {
                      :fn       (fn [in-chan] in-chan)
                      :infinite true
                      :expands? true
                      }

   :id {:fn identity}

   :collect  {
              :fn       (fn [v]
                          v)
              :collect? true
              }
   })

;;

(defn emit-step_ [*wf]
  (let [step (d/to-primitive (js/prompt "provide step as [:handler-id <params>], e.g. [:print \"some value\"]"))]
       (let [loop-chan (st-wf/&wf-init-param *wf ::evt-loop-chan)]
         (async/put! loop-chan
                     {(wf/rand-sid "emit-") step})
         )
       )
  )


;;
;; export
;;
(defn init-sandbox-wf! [*wf]
  {

   :init-fns   [st-wf/chan-factory-init-fn
                evt-init-fn
                sandbox-init-params-fn]

   :ctx-fns    [evt-ctx-fn
                sandbox-ctx-fn]

   :steps-fns  [sandbox-steps-fn]

   :opt-fns    [st-wf/chan-factory-opts-fn]

   :wf-actions {
                :not-started []
                :running     [
                              ["emit step" (partial emit-step_ *wf)]
                              ]
                :done        []
                }

   }
  )
