(ns woof.impl.backpressure
  "woof workflows"
  (:require #?(:clj [woof.utils :as u :refer [put!? debug! inline--fn inline--fn1]])
            #?(:cljs [woof.utils :as u])

            #?(:clj [clojure.core.async :as async :refer [go go-loop]])
            #?(:cljs [cljs.core.async :as async])

    ;; for now refer all the protocols and all their methods
            [woof.core.protocols :as protocols
             :refer [WoofState
                     get-initial-steps get-steps get-steps2add get-steps-left get-results do-commit! do-update!
                     do-update-sync! do-expand! commit! expand! update-steps! ready? sync? get! get!* get-all!
                     WoofBackPressure
                     start-producing? stop-producing?
                     ]]
    ;; impls
            [woof.impl.state :refer [make-state-cfg make-state!]]

            )

  #?(:cljs
     (:require-macros
       [cljs.core.async.macros :refer [go go-loop]]
       [woof.utils-macros :refer [put!? debug! inline--fn inline--fn1]]
       )))


(defn- freq-map!
  "builds map with number of working and pending steps"
  [steps-left]
  (merge {:working 0} (frequencies steps-left)))

(def freq-map (memoize freq-map!))


(defrecord WFSimpleBackPressure [STATE *bp options]
  WoofBackPressure



  (start-producing?  ;; ! should be called first
    [this]

    (let [freqs (merge {:working 0 :pending 0}
                       (freq-map (vals @(get-steps-left STATE))))
          min-t (get options :notify-ms-min 10)
          max-t (get options :notify-ms-max 10000)
          ]

      (swap! *bp merge {
                        :freqs freqs
                        :current-i 0
                        :timer (u/exp-backoff min-t 2 max-t)
                        })

      (let [b (> (:working-threshold options 20) (:working freqs))]
        ; (if b (println "can-produce? " (d/pretty freqs) (> (:working-threshold options 20) (:working freqs))))
        b
        )))


  (stop-producing?
    [this i]

    (let [CHECK-BACKPRESSURE-ON 20 ;; todo: do we need to checking for bp every time or this should be in produce?
          bi (get @*bp :current-i 0)]

      (if (= bi CHECK-BACKPRESSURE-ON)
        (let [freqs (merge {:working 0 :pending 0}
                           (freq-map (vals @(get-steps-left STATE))))

              max-t (get options :notify-ms-max 4000)
              bp-t (get options :working-ms-threshold 80)

              {prev-freqs :freqs
               timer-fn  :timer} @*bp

              {currently-working :working
               currently-pending :pending} freqs

              {prev-working :working
               prev-pending :pending} prev-freqs]


          (swap! *bp merge { :freqs freqs
                            :current-i 0})

          (if (and
                (> currently-working (:working-threshold options 20))
                (> currently-working 0))

            (let [pending-grows? (> (- currently-pending prev-pending) 0)
                  working-grows? (> (- currently-working prev-working) 0)
                  over-working? (> currently-working (:working-max options 50))

                  wait-time (timer-fn)
                  over-threshold? (>= wait-time bp-t)]


              #_(if (and
                      (= prev-freqs freqs)
                      (= max-t wait-time)
                      (not (= {:working 0, :pending 0} freqs)))
                  (println "wf is stuck!") ;; todo: notifying of backpressure
                  )



              (let [stop-produce? (or over-working?
                                      (cond
                                        (and pending-grows?       working-grows?)       true
                                        (and pending-grows?       (not working-grows?)) over-threshold?
                                        (and (not pending-grows?) working-grows?)       over-threshold?
                                        :else false
                                        ))]

                stop-produce?
                )
              )
            )
          )
        (do
          (swap! *bp update :current-i inc)
          false))
      )
    )
  )


;; constructor
(defn make-backpressure! [STATE CTX]
  (->WFSimpleBackPressure
    STATE
    (atom {:freqs {:working 0 :pending 0}
           :current-i 0
           :bp-time 0
           })
    {
     :working-threshold 30

     :working-max 50

     :working-ms-threshold 160

     :notify-ms-max 4000
     :notify-ms-min 20

     :CTX CTX
     }
    )
  )