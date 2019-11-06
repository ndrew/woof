(ns woof.impl.state
  "woof state impl"
  (:require [woof.graph :as g]

            #?(:clj [woof.utils :as u :refer [put!? debug! inline--fn inline--fn1]])
            #?(:cljs [woof.utils :as u])


            #?(:clj [io.aviso.ansi :as ansi])

            #?(:clj [clojure.core.async :as async :refer [go go-loop]])
            #?(:cljs [cljs.core.async :as async])

    ;; for now refer all the protocols and all their methods
            [woof.core.protocols :as protocols
             :refer [WoofDebug dbg!
                     WoofContext get-step-fn get-step-config
                     WoofWorkflowMsg send-message
                     WoofSteps
                     initial-steps steps get-steps* get-steps2add* get-steps-left*
                     do-update-steps! get-infinite-steps get-infinite-step set-infinite-step
                     steps-ready? add-steps! add-pending-steps! step-ready! step-working! is-step-working? has-infinite?
                     WoofState
                     get-initial-steps get-steps get-steps2add get-steps-left get-results do-commit! do-update!
                     do-update-sync! do-expand! commit! expand! update-steps! ready? sync? get! get!* get-all!
                     WoofStepProcessor
                     process-step! produce-steps! consume-steps!
                     WoofBackPressure
                     start-producing? stop-producing?
                     ]]

            )

  #?(:cljs
     (:require-macros
       [cljs.core.async.macros :refer [go go-loop]]
       [woof.utils-macros :refer [put!? debug! inline--fn inline--fn1]]
       )))


;; shorthands

(def sid u/sid)
(def sid? u/sid?)  ;; predicates that check parameter in workflow is a link to other action
(def sid-list? u/sid-list?)
(def rand-sid u/rand-sid)


(defn- sid-list-from-expand-map [expand-map]
  (let [sids (keys expand-map)]

    ;; fixme: handle expand key for client-server communication
    #_(if-let [{expand-key :expand-key} (meta expand-map)] ;; ???
        (list expand-key)
        sids)

    sids
    )
  )


(defn- do-expand-impl! [STEPS *results id actions]

  (if-not (get-infinite-step STEPS id)
    (do

      (swap! *results merge {id (sid-list-from-expand-map actions)})

      (add-steps! STEPS actions)
      (add-pending-steps! STEPS actions)

      (step-ready! STEPS id)

      )
    (do
      ;; save all the expanded ids

      (swap! *results update-in [id]
             (fn[a b]
               (if (seq? a)
                 (concat a b)
                 b))
             (keys actions))

      (add-steps! STEPS actions)
      (add-pending-steps! STEPS actions)

      )
    )
  )



(defrecord WFState [cfg STEPS]

  WoofDebug ;; is this needed?
   (dbg! [this k] STEPS)

  WoofState
  ;; most of the methods are proxied to a reified STEPS model

  (get-initial-steps [this] (initial-steps STEPS))
  (get-steps [this] (get-steps* STEPS))
  (get-steps2add [this] (get-steps2add* STEPS))
  (get-steps-left [this] (get-steps-left* STEPS))
  (get-results [this] (get this :RESULTS))

  (do-commit! [this id step-id result]
    (let [*results (get-results this)
          step-cfg (get-step-config (get this :CTX) step-id)
          infinite? (and
                        (:infinite step-cfg)
                      (not (nil? (get @(get this :INF) id))))
          ]

      #?(:clj  (dosync
                 (swap! *results assoc id result)

                 (if-not infinite?
                   (step-ready! STEPS id))))

      #?(:cljs (do
                 (swap! *results assoc id result)
                 (if-not infinite?
                   (step-ready! STEPS id))
                 ))))

  (do-update!
    [this msg]
    (let [[id step-id params result] msg
          step-cfg (get-step-config (get this :CTX) step-id)
          infinite? (:infinite step-cfg)
          dependant-steps (g/get-dependant-steps (steps STEPS) id)
          ]


      ;#?(:clj (locking *out* (println (ansi/magenta (pr-str (steps STEPS))))))
      ;#?(:clj (locking *out* (println (ansi/magenta (pr-str id)))))

      (let [d-steps (rest dependant-steps)]
        (if (not infinite?)
          (do
            (swap! (get-results this) (partial apply dissoc) d-steps)
            (add-pending-steps! STEPS d-steps)
            (do-commit! this id step-id result)

            ;; todo: does this is needed?
            (go
              (put!? (:process-channel cfg) [:steps [:update d-steps]] 1000)))
          (do
            ;; (locking *out* (println "clean-in up"))

            ;(do-commit! this id step-id result)
            (swap! (get-results this) (partial apply dissoc) d-steps)
            (add-pending-steps! STEPS d-steps)
            ;(do-commit! this id step-id result)
            (go
              (put!? (:process-channel cfg) [:steps [:update d-steps]] 1000))
            )))))

  (do-update-sync!
    [this msg]
    (let [[id step-id params result] msg
          step-cfg (get-step-config (get this :CTX) step-id)
          infinite? (:infinite step-cfg)]

      (let [d-steps (rest (g/get-dependant-steps (steps STEPS) id))]
        (if (not infinite?)
          (do
            (swap! (get-results this) (partial apply dissoc) d-steps)
            (add-pending-steps! STEPS d-steps)
            (do-commit! this id step-id result)
            ;; todo: does this is needed?
            (go
              (put!? (:process-channel cfg) [:steps [:update d-steps]] 1000))
            ;(do-update-steps! this d-steps)
            )
          (do
            ;; (locking *out* (println "do-update-sync!" d-steps))

            (if (empty? d-steps)
              (do
                ;;
                ; (locking *out* (println msg))
                ;; update id step
                )
              (do
                ;(do-commit! this id step-id result)
                (swap! (get-results this) (partial apply dissoc) d-steps)
                (add-pending-steps! STEPS d-steps)
                ;(do-commit! this id step-id result)
                ; (do-update-steps! this d-steps)
                )
              )
            ;; FIXME: what to send in :update
            (go
              (put!? (:process-channel cfg) [:steps [:update d-steps]] 1000)))))))


  (update-steps! [this params]
    ; previously it indicated that steps were added

    ; FIXME: check if the value of infinite steps changed

    #_(locking *out* (println
                       "\nupdate-steps!: \t"
                       (d/pretty @(get-steps-left* STEPS))
                       "\n" (d/pretty (get-infinite-steps STEPS))))
    (do-update-steps! STEPS params))


  (commit!
    [this id step-id params result]

    (if (u/channel? result)
      (let [;*steps-left (get-steps-left this)
            step-cfg (get-step-config (get this :CTX) step-id)
            infinite? (:infinite step-cfg)
            *inf (get this :INF)]

        (if-not (is-step-working? STEPS id)
          (do
            (swap! (get-results this) assoc id result)
            (step-working! STEPS id)

            ;; todo: do we need to close the channel
            ;; todo: stop recurring on workflow stop
            (if infinite?
              (do
                (swap! *inf assoc id :working) ; save that we have infinite action

                (go-loop
                  []
                  (when-let [v (async/<! result)] ;; u/<?
                    (put!? (:infinite-channel cfg) [:save [id step-id params v]] 1000)
                    (recur))))
              (go
                (let [v (async/<! result)] ;; u/<?
                  (put!? (:process-channel cfg) [:save [id step-id params v]] 1000)))))
          ))
      (do
        ;; (locking *out* (println "\ndo-commit!:\n" id "\n" result))
        (do-commit! this id step-id result)
        )))


  (expand! [this id step-id params actions]
    (let [step-cfg (get-step-config (get this :CTX) step-id)]
      (if (u/channel? actions)
        (let [*steps-left (get-steps-left this)
              infinite? (:infinite step-cfg)]

          (when-not (is-step-working? STEPS id)
            (swap! (get-results this) assoc id actions) ;; store channel
            ; (swap! *steps-left assoc id :working)
            (step-working! STEPS id)

            (if infinite?
              (do
                (when-not (get-infinite-step STEPS id)
                  (set-infinite-step STEPS id actions)

                  (go-loop []
                    (when-let [v (async/<! actions)]
                      (put!? (:infinite-channel cfg) [:expand [id step-id params v]] 1000)
                      (recur)
                      )
                    )
                  )
                )
              (go ;; normal
                (let [v (async/<! actions)]
                  ;; TODO: handle if > 1000 actions are added
                  ;; TODO: handle if cycle is being added

                  ;(println (d/pretty [:expand [id step-id params v]]))

                  (put!? (:process-channel cfg) [:expand [id step-id params v]] 1000)))
              ))
          )
        (let [collect? (:collect? step-cfg)]
          (if (and collect? (not (map? actions)))
            (do-commit! this id step-id actions)
            (do-expand! this id actions)
            )))))


  (do-expand! [this id actions]
    ;; FIXME: do we have to add this to linked infinite actions?
    #_(locking *out* (println "do-expand!" id "\n\n" (d/pretty actions)))

    (let [*results (get-results this)]
      #?(:clj (dosync (do-expand-impl! STEPS *results id actions)))
      #?(:cljs (do-expand-impl! STEPS *results id actions))))


  (ready? [this]
    (and
      (steps-ready? STEPS)
      (empty? @(get this :INF))))


  (sync? [this]
    (has-infinite? STEPS))


  (get! [this id]  ; get!
    (u/nil-get @(get-results this) id))


  (get!* [this id-list]
    ;; todo: nils?
    (map (partial get! this) id-list))


  (get-all! [this vs]
    (if (seq? vs)
      (map (fn [s]
             (if (sid-list? s)
               (if-let [z (get!* this s)]
                 (get-all! this z)
                 s)
               s)
             ) vs)
      vs))
  )


(defn make-state-cfg
  "stores state needed for executor, like channels and steps."
  [steps process-channel inf-process-chan] ; bs
  {:steps steps
   :process-channel process-channel
   :infinite-channel inf-process-chan}
  )


(defn- make-steps-model! [steps]

  (let [*state (atom {:infinite {}})
        ;; *results (atom (array-map))

        *steps (atom steps)
        *steps2add (atom (array-map))
        *steps-left (atom (reduce-kv (fn [a k v] (assoc a k :pending)) {} steps))]


    (reify WoofSteps

      (initial-steps [this] steps)

      (steps [this] @*steps)

      (get-steps* [this] *steps)
      (get-steps2add* [this] *steps2add)
      (get-steps-left* [this] *steps-left)


      (do-update-steps! [this [op nu-steps]]
        ; do not distinguish between :add and :update

        (let [new-steps @*steps2add]
          (if (empty? new-steps)
            (do
              ;; update
              ;; (locking *out* (println "update steps:\t" nu-steps))
              )
            (do
              (swap! *steps merge new-steps)
              (reset! *steps2add (array-map))))

          ))

      (get-infinite-steps [this]
        (get-in @*state [:infinite]))

      (get-infinite-step [this id]
        (get-in @*state [:infinite id]))


      (set-infinite-step [this id result]
        #_(locking *out* (println "\tinfinite step" id))

        (swap! *state update-in [:infinite] assoc id result))


      (is-step-working? [this id]
        (= :working (get @*steps-left id)))

      (steps-ready? [this]
        (let [ready? (and (empty? (get @*state :infinite {}))
                          (every? #(= % :ok) (vals @*steps-left)))]
          ready?))

      (add-steps! [this actions]
        #_(locking *out* (println "\tadd steps" actions))

        (swap! *steps2add into actions))


      (add-pending-steps! [this actions]
        #_(locking *out* (println "\tadd pending steps" actions))

        #?(:clj
            (if (satisfies? clojure.core.protocols/IKVReduce actions)
              (swap! *steps-left merge (reduce-kv (fn [a k v] (assoc a k :pending)) {} actions))
              (swap! *steps-left merge (reduce #(assoc %1 %2 :pending) {} actions)))
           )

        #?(:cljs
           (if (satisfies? cljs.core/IKVReduce actions)
             (swap! *steps-left merge (reduce-kv (fn [a k v] (assoc a k :pending)) {} actions))
             (swap! *steps-left merge (reduce #(assoc %1 %2 :pending) {} actions)))))

      (step-ready! [this id]
        #_(debug "\tstep ready" id)

        (swap! *steps-left dissoc id))

      (step-working! [this id]
        #_(locking *out* (println "\tworking" id))

        (swap! *steps-left assoc id :working))

      (has-infinite? [this]
        (empty? (get @*state :infinite {})))
      )
    )
  )




;; constructor

(defn make-state! [context state-config]

  (let [*results (atom (array-map))
        *infinite (atom {})
        steps-model (make-steps-model! (:steps state-config))
        ]

    (assoc
      (->WFState state-config steps-model)

      ; pass the optional params
      :CTX context ;; FIXME: pass context implicit
      :RESULTS *results

      :INF *infinite
      )
    )
  )
