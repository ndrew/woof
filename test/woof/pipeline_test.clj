(ns woof.pipeline-test
  (:require
    [clojure.test :refer :all]

    [clj-async-test.core :refer :all]

    [clojure.core.async :as async :refer [go go-loop]]


    ; [woof.actions :as a]
;    [woof.core :as c]
;    [woof.data :as d]
;    [woof.pipeline :as p]


;    [woof.executor :as exec]
;    [woof.utils :refer :all]

;    [woof.test-data :as test-data]
))

(comment

;; dsl

;; expand params
;; merge attrs
;; exec action


(defn build-action [action-id attrs & params]
  (into [action-id attrs] params))


;(build-action :foo {} :1 :2 :3)




;; TODO: pass channel here if we want to control merge

(defn build-result-atom
  "Default impl of build-result-atom.
  Prepares a list of actions and action parameters in order as they have to be executed (merge-key).
  Provides async channel for further updates
  {
  :history []
  :attrs []
  :result nil
  }
  "
  []
  ;; TODO: migrate to namespaced keywords
  ;; TODO: rename :history to something :actions?
  (atom {:history []
         :attrs []
         :result nil
         :status (async/chan)}))



;;
(declare plan!) ;; TODO: rename to prepare-plan!

(defn expand-params!  ;; TODO: rename
  "def impl of ??? config merging func"
  [tag attrs params *results]

  (println "expand params " (d/pretty [tag attrs params *results]))
  params)

(def ^:dynamic *expand-params* expand-params!)

(defn merge-attrs! ;; TODO: rename to merge-configs!
  "def impl of merging configs.
   It's merged without merge, the configs are stored as indexed list
  (where index is called merge-key), so later these can be merged easily."
  [tag old nu *results]
  (println "merge-attrs!" (d/pretty [tag old nu *results]))

  (swap! *results update-in [:attrs] conj [tag old nu])
  (dec (count (:attrs @*results))))


(defn execute-action!
  "def impl of action execution"
  [selector tag attr params *results] ;; substitute

  (println "execute-action!" (d/pretty [selector tag attr params *results]))

  (let [new-params (*expand-params* tag attr params *results)
        result [selector tag attr new-params]]
    (swap! *results update-in [:history] conj result))
    ;; TODO: how to know that we have more children

  attr)




(def ^:dynamic *merge-attrs* merge-attrs!)

(def ^:dynamic *execute-action* execute-action!)

(def ^:dynamic *build-result-atom* build-result-atom)

;; TODO: add dynamic fn for tag name checking?

(defn normalize-action
  "casts action to a proper [tag config params] form"
  [[first second & rest]]
  (when-not (or (keyword? first)
                (symbol? first)
                (string? first))
    (throw (ex-info "Expected a keyword as a tag" { :tag first})))
  (let [tag first
        ; TODO: check whether tag name is correct
        [attrs children] (if (or (map? second)
                                 (nil? second))
                           [second rest]
                           [nil    (cons second rest)])]
    [tag attrs children]))


(defn- parse-action
  ""
  [selector rule prev-attr *results]
  ; (println "|| parse-action" selector rule prev-attr)
  (let [[tag attrs children] (normalize-action rule)
        composite? (vector? (first children))]
    (if composite?
      (let [merge-key (*merge-attrs* tag prev-attr attrs *results)
            *tmp-attrs (atom merge-key)]

        (doall (map-indexed (fn[i element]
                              (if-let [res (plan! (conj selector (+ 2 i)) element @*tmp-attrs *results)]
                                (reset! *tmp-attrs res)))
                            children))

        (*execute-action* selector tag @*tmp-attrs nil *results))

      (let [merge-key (*merge-attrs* tag prev-attr attrs *results)]
        (*execute-action* selector tag merge-key children *results)))))


; plan should be something like execute. so actual plan (preparing for execution should be different fn)

(defn plan!
  "Prepares execution plan"
  ([actions]
   (plan! nil actions))

  ([initial data]
     ; (println "pipeline/plan!" (d/pretty initial) (d/pretty data))
   (let [*results (atom {
                         :history []
                         :attrs []
                         :result nil})
         selector []
         merge-key (*merge-attrs* :initial nil initial *results)]

    (plan! selector data merge-key *results)

    @*results))

  ([selector data attrs *result]
   (cond
     (vector? data) (parse-action selector data attrs *result))
     ; TODO: can there be sequence in the pipeline?
     ;(seq? data) (do
     ;              (println "seq")
     ;              (doseq [element data]
     ;                  (plan! element attrs *result)))
   attrs))






(let [PIPELINE [:_ {:i :top}
                     [:1 {:i :1}
                        [:1_2]]
                     [:2]
                     [:3 {:i :1}
                      [:3_1 {:i :2}
                        [:3_1_1]
                        [:3_1_2]

                       ]
                      ]]
      ]

  ; (print (d/pretty (plan! PIPELINE)));


  (let [plan (plan!  :initial PIPELINE)

        {
          history :history
          attrs :attrs
          } plan

        process-action (fn [i input]
                         (let [merge-key (get input 2)

                               ;params (merge-attrs attrs merge-key merge)

                                             ]
                               ;(process-sync ...)

                           ;[(get attrs merge-key) ]

                           [(get input 1) (map #(nth % 2) (drop 1 (take (inc merge-key) attrs)))]


                           ))

        ]

    ;(map-indexed process-action history)
;    history
    ;(count history)
    ; (drop 1 attrs)

    ; history
    attrs
    )

)


(defmacro with-custom-fn [build-result-fn merge-fn execute-fn & body]
  `(binding [*build-result-atom* ~build-result-fn
             *merge-attrs* ~merge-fn
             *execute-action* ~execute-fn]
     (do ~@body)))




(comment
(deftest actions

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








#_(deftest pipeline-running-async

   (let [PIPELINE [:_ {:tip :top}
                     [:hello {} "world"]
                     [:selo {} ""]]

         PLAN (p/plan! PIPELINE)]

       (println "—PLAN" (d/pretty PLAN))

         (let [sync-handler-fn (fn [selector tag cfg params]
                            ;; for now return the ;; TODO: can there be more actions spawned?
                            (println "HANDLER started: " tag "(" (dissoc cfg :channel) ")" params)
                            (p/build-handler-result selector tag cfg params))

          async-handler-fn (fn [selector tag cfg params]
                            (println "HANDLER started: " tag "(" (dissoc cfg :channel) ")" params)

                            (-> (Thread.
                                  (fn []
                             (go
                                 (async/<! (timeout (+ 1000 (rand 13000))))
                                ; (println "HANDLER ended: " tag "(" cfg ")" params )
                                 (async/put! (:channel cfg) [selector tag cfg params]))
                                    )) .start)



                          ; end anyncronously by telling it's :pending
                          ; TODO: or just remove the :channel, so the key will be preserved
                            (p/build-handler-result selector :pending cfg params))


          handler-config {:hello (a/build-action-fn sync-handler-fn)}
          handler (p/build-handler handler-config
                                (a/build-action-fn sync-handler-fn))]

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

                       ; (is (= data [[[2] :hello 2 '("world")] [[] :_ 1 nil]]))
                            ;; this is not needed as p/run-plan! always return channel
                            (go
                              (let [[status final-result] (async/<! c)] ;; got final from the beggining
                                (println "—RESULT" (d/pretty final-result))

                             ;; (is (= final-result [[[2] :hello 2 (quote "world")] [[] :_ 1 nil]]))
                                ))
                           ))



     )))
)


  )


; pipelines: provider/transformer/consumer

;    *Provider
;     these generate Assets via ui (side-effect free) or via some io
;    *Transformer
;     these take assets (some as constructor, some as params) and transform these into different assets. These should have an editor assigned to them.
;     Think of it as a reducer.
;    *Consumer
;     these take assets and do io with them


;; 1: config
;; 2: (enrich config) -> assoc config :foo :bar

;; 3: (init-template config) -> assoc config :template

;; 3: (read-posts %) -> assoc config :posts []

;; 4: (render-posts (:posts config) (:template config) ) -> plan -> Assets

;; 4: (build-index %) -> Asset[]


;; 5: (output %) -> Result

;; providers (meta)
;;                              [ [:load-defaults [*] additional-data          -> [*]]                       ; meta-data editor
;; providers (data)
    ;; add collection plugin
;;      [:edn [*]     -> [:collection]]  ; data editor
;; transformers
;;   [:template [:collection] fn -> [:index] ]
;; consumers
;;   [:print [:index]]

;; more complicated setup
    ;; add template plugin -> template configs are added to config into [:tempate]
;;                                [:init-template [*] template-params          -> [:tempate] ]               ; template editor

  ;; add meta-data plugin ->
    ;; add file-system plugin -> fs params are added to config, pipeline is added to [:posts]
;;                                [:fs-read [*] [:metadata]                    -> [:posts] ]                 ; plan editor
;; transformers
    ;; add markdown plugin, markdown specific configs are added to config, specify input (plan)
;;                                [:markdown [*] [:posts]                      -> [:markdown-posts] ]        ; plan editor
;; consumers
;;                                [:index [*] [:markdown-posts] [:template]]   -> [:index] ]                 ; plan editor
;;                                [:filesystem [*] [:index] [:markdown-posts]  -> [:result] ]                ; plan editor
;;                                [:deploy [*] [:index]                        -> [:result] ]


;; ]




