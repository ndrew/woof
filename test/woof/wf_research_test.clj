(ns woof.wf-research-test
  (:require
    [clojure.core.async :as async :refer [go go-loop]]
    [clojure.test :refer :all]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.wf-data :as wdata]

    [woof.utils :as u]
    [woof.test-data :as test-data]
))

;; test and refine some ideas about woof workflows here
;;


;;
;; defining a workflow

;; workflows are low level and tend to get messy - as there will be many intermediary steps
;; same applies for context - as it will grow, there can be confusion on how to compose step handlers

;; context

;; defining context via map or via helper functions?
;;   helper funcs
;;     - have to learn them
;;     - less to type
;;   step handler map
;;     - more obvious
;;     - typo prone


;; metadata
;;   - :expands?  <?> maybe use emit?
;;   - :collect?
;;   - :infinite
;;   ? :spec

;; <?> have some step handlers available by def?

;; :id — {:fn identity}
;;   returns a single value from sbody parameter or via reference
;;
;;   <?> maybe name :identity or :value

;; :id* — {:fn identity :collect? true}
;;    <?> :collect*


;;   :zip, :group



;; arity
;;   - step handlers are arity 1

;; - doing transformations is straight forward
;;     especially enrichment, like ring handlers — pass map and assoc needed keys
;;     map like transforms - :t1->t2

;; - transforming lists/seqs

(let [ctx-map {
                :v  {:fn identity}
                :v* {:fn identity :collect? true}
                }
      steps {

              ::v-from-value  [:v* "v1"]  ;; single value, directly
              ::v-from-sid    [:v* ::v1]  ;; or via sid reference

              ;; sequences directly, known size
              ::v-from-values [:v* ["v1" "v2" "v3"]]

              ;; or via sid references directly
              ::v1 [:v "param1"]
              ::v2 [:v "param2"]
              ::v-from-sid-list [:v* [::v1 ::v2] ]

              ;; or via intermediary sid-list
              ::&vs [:v [::v1 ::v2]]
              ::v-from-sid-list-ref [:v* ::&vs]

              ;; sequences, unknown size
              ;; only via intermediary step, usually from extends handler

              }]
  @(wf/sync-execute!
     (wf/build-executor (wf/make-context ctx-map) steps)
     100))


;; <?> transforming maps

;; <?> branching
;;  emitting steps via extend handler

(let [ctx-map {
                :v  {:fn identity}
                :router {:fn (fn [x]
                               (if (> 10 x)
                                 {::more [:v "more"]}
                                 {::less  [:v "less"]}
                                 )

                               )
                         :expands? true}
                }
      steps {
              ::if [:router 5]

              }]
  (wdata/inline-results
     @(wf/sync-execute!
        (wf/build-executor (wf/make-context ctx-map) steps)
        100)))


;;
;; conventions and notions

;; * step handlers
;;   - simple keyword, e.g :id
;;   - if returns single value — :handler
;;     - if handler transforms an object — :t1->t2
;;   - if has side-effects     — :handler!
;;   - if predicate            — :handler?
;;   - if returns sid list
;;     - :collect? — :handler*
;;     - :expands? — :handler+
;;     - both :collect? and :expands?
;;   ? if infinite — :handler
;;
;;   examples:
;;     :id
;;

;; ...

;; stateful steps, same way as rum mixins
;; or just (partial *STATE ...)



;; * sids
;;    - namespaced keyword, e.g. ::sid
;;
;; * sid list
;;   - sids from extends handler — ::&sids
;;   - sids from collect handler — ::values*
;;





;;
;; usage patterns





;;
;; 2 + 2 example


#_(let [ctx-map {
                :v  {:fn identity}



                :v* {:fn identity :collect? true}

                :sum {:fn (fn[nums]
                            (reduce + nums))
                      :collect? true
                      }



                :nums* {:fn (fn [nums]
                              (into {}
                                    (map (fn[x] [(wf/rand-sid)
                                                 (wf/sbody :v x)]) nums))
                              )
                         :expands? true}


                }
      steps {


              ::ns [:nums* [2 3 4 ]]
              ::sum [:sum ::ns]


              ::v1 [:v 2]
              ::v2 [:v 3]

              ::sum1 [:sum [::v1 ::v2]]

              }]
  (println (d/pretty  @(wf/sync-execute!
     (wf/build-executor (wf/make-context ctx-map) steps)
     100)))
)


;;

;; intermediary step for ui

;;


