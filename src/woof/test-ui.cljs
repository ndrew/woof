(ns woof.test-ui
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
    [cljs.test :as test :refer-macros [deftest is testing run-tests async]]
    ;[woof.actions :as a]
    [woof.core :as c]
    [woof.data :as d]
    ;[woof.pipeline :as p]
    [cljs.core.async :as async]))

(comment
;;(enable-console-print!)

;; executor


;; process can be represented as list of steps. each step should be finite.
;;
;; process with one step
;; [[:step params]] => result, where result can be a channel or value
;; there should be a function for :step
;; ??? should step any nubmer of parameters or just one?

;; several steps could be executed parallely, then result will be a list
;; [
;;   [:step-1 params]
;;   [:step-2 params]
;; ]
;; => [v1 v2]
;;
;; parameters for step can be obtained from other step - nesting
;; [ [:step-1 params] => r1
;;   [:step-0 r1]] => step-1
;; intermidiate results will be stored, and step-0 will wait for the step-1

;; ! execution list should be flat

;; we can describe such processes as a tree
;;
;; [:root [:step-1 [:step-2 params]]]
;; ->
;; [
;;   [step-2 params] => r2
;;   [step-1 r2] => r1
;;   [root r1]
;; ] => [v]
;;
;; or if we want several values
;; [:gate [:step-a params] [:step-b params]]
;; ->
;;  [
;;    [:step-a params] => a
;;    [:step-b params] => b
;;    [:gate a b]
;;  ] => [v]

;;[:id attrs params] => (id attrs) => ((id attrs) params)

;; {:handler {:fn ...}}


[:workflow {:global :data}
  [:action {:param :pam-pam} [:sub-action {:pum :pum} "ururu"] ]
  [:action {:param1 :pam-pam} "234"]
  [:action {:param2 :pam-pam} "345"]
 ]

;[m1 [:sub-action {:pum :pum} "ururu"] ]

; d1) :workflow {:global :data}
;{}

; d2) :action (merge-attrs {:global :data} {:param :pam-pam} ) => m1
{:action {:fn (gen-fn (merge-attrs {:global :data} {:global :my-global} ) )}}  (fn [{:global :my-global} x])
; d3) :sub-action (merge-attrs m1 {:pum :pum}) => m2
{:action {:fn (gen-fn (merge-attrs {:global :my-global} {:global :my-turbo-global} ) )}}  (fn [{:global :my-turbo-global} x])

; b1) ((sub-action m2) "ururu") -> r1

{::b1 [:sub-action "ururu"] }

; b2) ((action m1) r1)
{::b2 [:action ::b1]}

; d4) :action (merge-attrs {:global :data} {:param1 :pam-pam} ) => m3
; b3) ((action m3) "234")
; d5) :action (merge-attrs {:global :data} {:param2 :pam-pam} ) => m4
; b3) ((action m4) "345")




;; this execution mechanism will be used for 2 step execution,
;; the step will be described as
;;  [:step {} params]
;; first, we will configure the steps,
;;  [:step {}] -> will add :step-executor fn
;;             => params for further steps
;; then [:step-executor params] will be executed
;; [
;;   [:step {}] => add :step-executor ;; ? merge-fn?
;;   [:step-executor params]
;   ]
;;

;; nesting
;; [:step {} [:step-1 {} params]]
;; ->
;; [
;;    [:step {}] => :step-executor => step-merge-fn
;;    [:step-1 (step-merge-fn {})] => :step-1-executor => step-merge-fn
;;    [:step-1-executor params] => r0
;;    [:step-executor r0]
;;  ]
;; if there is merge function provided

;; this way we can initialize some steps before. Also the process may be run in a session
;; so there could be commit/rollback

;; how steps should work if multiple results are
;; [debug* 1 2 3]
;; ->
;; [
;;   [debug 1]
;;   [debug 2]
;;   [debug 3]
;; ]

;;   [markdown*
;;     [fs* path]]
;; ->
;; [
;;  [file-system { :include "*.md"}] => :fs
;;  [fs* {:path "/.."}] => files*
;;  [markdown files*] => m*
;; ]
;; ->
;; [
;;  [file "/file1.md"] => f1 ; could the files be lazy
;;  [file "/file2.md"] => f2
;;  [file "/fileN.md"] => f3
;;  [markdown f1] => m1
;;  [markdown f2] => m2
;;  [markdown fN] => mN
;; ]
;; => [m1 m2 .. mN]

;; ;;[:gated [:step-0 {} parmas]]

;;

;; [:step-0 {merge-fn fn} [step-1 {} data]] => [:step-0-executor-0 (step-1-executor data)]

;; which will become
;;  (step-1 :construct (merge-fn parent-params step-params)) => executor-1
;;  (step-0 :construct parent-params) => executor-0
;; and will the new process
;; [:step-0-executor {}
;;   [:step-1-executor {}]]
;;


;; the nesting will work the following way

;; where step function is a multimethod that dispatch via first param
;;
;; (step :construct! {}) => :step-executor-0 (fn [attrs params])

;; data can be another step (for multi-step processes)
;;  top process should provide the merge function (reduce ?)
;;

;;
;; nesting could go on
;; [:process {:merge-result fn}
;;   [:step-1 {}
;;      [:step-2 {}]]]
;; so it will be
;; (p (step-1 (step-2)))
;;

;; some steps could run paralelly
;; [:gate-action {}
;;   [:step-1 {}]
;;   [:step-2 {}] ]
;; so it will be
;; (p (step-1) (step-2))
;; ? how
;



(test/deftest actions

  ;;
  ;; define actions
  ;;   action is a unit of execution.
  ;;   action is defined as [:action-id {:config :map} <params>]
  ;; First the action is 'initialized'
  ;;   (action {:config :map}) => fn  ; where action is described somewhere else
  ;; After the params are processed
  ;;   (fn <params>) => channel|edn"

  (let [a-id :hello
        a-config {}
        a-param "world"
        action (p/normalize-action (a/build-action a-id a-config a-param))

        ;; actions are processed via handlers ;; todo: find proper name
        ;; actions executors are stored in the action registry (as a kv of action-id: handler and the def handler)
        default-handler (a/build-action-fn
                          (fn [selector tag cfg params] ;; todo: do we need selectors here
                            (println " handler " (d/pretty [selector tag cfg params]))
                            ;; for now return the ;; todo: can there be more actions spawned?
                            (p/build-handler-result selector tag cfg :done)
                            ))
        handler (p/build-handler {} default-handler)
        ;; handler can return edn data (syncronous process) or can return channel (async)
    ]

    ;;
    ;; 'compile' action to a plan
    (let [*plan (p/build-result-atom)]
      ;; this will be the 'compiled' execution plan for single action

      ;    {
      ;     :history [[[] :hello 1 ("world")]],           ;; actions in order they should be executed
      ;     :attrs [[:initial nil nil] [:hello 0 {}]]     ;; attrs in order they should be merged
      ;     :result nil,                                  ;; result
      ;     :status #object[cljs.core.async.impl.channels.ManyToManyChannel]
      ;    }

      (swap! *plan merge
             {
               :history [[[] :hello 1 (list a-param)]] ; todo: move the selector inside attributes
               :attrs [[:initial nil nil] [:hello 0 {}]] ;; todo: remove the :initial crap
               })

      (let [rc (p/run-plan! @*plan handler)]

        (go
          (loop [] ; can handle state via loop bindings
            (let [r (async/<! rc)
                  [status data] r]

              (condp = status
                :initial (do
                           (println "INITIAL:" (d/pretty data))
                           ;(init-fn result r)
                           (recur))
                :final (do
                         (println "FINAL:" (d/pretty data))
                         ;(println "PIPELINE: end!")
                         ;(finalize-fn result r)
                         )
                :error (do
                         ;
                         )
                (do
                  (println "PROCESSING:"  (d/pretty r))
                  ;; (process-fn result r)
                  (recur))
                ))))

        )
      )
    ))




;(test/deftest plan
  (let [PIPELINE [:site {:global :params}
                  [:foo {}]
                  [:fs* {}]
                  ;[:save {:id ::posts} [:posts {} [:markdown [:fs {}]]]]
                  ;[:save {:id ::index} [:landing {} ::posts]]
                  ]
        ;              [:debug {:id 1} "ASYNC 1"]   ; 1
        ;              [:inner {:id 1}            ; 3
        ;               [:debug {:id 2} "ASYNC 2.1"] ; 2
        ;               [:debug {:id 3} "ASYNC 2.2"]
        ;               [:debug {:id 4 :overwrite-test "debug.4" :debug-only "debug"}
        ;                [:inner {:id 2 :overwrite-test "inner.2" :inner-only "inner"}
        ;                 [:debug {:id 5} "ASYNC 2.2.1 inside ASYNC 2.2"]]]]
        ;              [:debug {:id 6} "ASYNC 3"]]

        ]
(:history (p/plan! PIPELINE))

  )
  ;)



(defn- timeout [ms]
  (println "waiting " ms)

  (let [c (async/chan)]
    (js/setTimeout (fn [] (async/close! c)) ms)
    c))


(test/deftest pipeline-running-async

   (let [PIPELINE [:_ {:tip :top}
                     [:hello {} "world"]
                     [:selo {} ""]
                   ]
         PLAN (p/plan! PIPELINE)]

     (println "—PLAN" (d/pretty PLAN))

    ;(is (= (:history PLAN) [[[2] :hello 2 '("world")] [[] :_ 1 nil]]))
    ;(is (= (:attrs PLAN) [[:initial nil nil] [:_ 0 {}] [:hello 1 {}]]))
  ;; TODO: fix the eventually
    (let [sync-handler-fn (fn [selector tag cfg params]
                            ;; for now return the ;; TODO: can there be more actions spawned?
                            (println "HANDLER started: " tag "(" (dissoc cfg :channel) ")" params)
                            (p/build-handler-result selector tag cfg params))

          async-handler-fn (fn [selector tag cfg params]
                            (println "HANDLER started: " tag "(" (dissoc cfg :channel) ")" params)
                            (go
                             (async/<! (timeout (+ 1000 (rand 13000))))
                            ; (println "HANDLER ended: " tag "(" cfg ")" params )
                             (async/put! (:channel cfg) [selector tag cfg params]))

                          ; end anyncronously by telling it's :pending
                          ; TODO: or just remove the :channel, so the key will be preserved
                            (p/build-handler-result selector :pending cfg params))


          handler-config {:hello (a/build-action-fn async-handler-fn)}
          handler (p/build-handler handler-config
                                (a/build-action-fn async-handler-fn))]

      (test/async done

        (p/execute-plan! PLAN handler
                         (fn [c [status data]]
                           (println " init" data)
                           data) ;; init-fn
                         (fn [c [status data]]
                            (println " process" data)
                           data) ;; process-fn
                         (fn [c [status data]] ; finalize-fn
                       ;; actual result (as sync)
                             ;; if there had been init than final result will be here
                             (println "—FINALIZE" (d/pretty data))
                             (done)
                       ; (is (= data [[[2] :hello 2 '("world")] [[] :_ 1 nil]]))
                            ;; this is not needed as p/run-plan! always return channel
                            #_(go
                              (let [[status final-result] (async/<! c)] ;; got final from the beggining
                                (println "—RESULT" (d/pretty final-result))

                             ;; (is (= final-result [[[2] :hello 2 (quote "world")] [[] :_ 1 nil]]))
                                (done)))
                           ))))))



(cljs.test/run-tests)

  )







