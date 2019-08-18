(ns woof.wf-publish-subscribe
  (:require
    [clojure.core.async :as async :refer [go go-loop]]
    [clojure.test :refer :all]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.xform :as x]
    [woof.wf-data :as wdata]

    [woof.utils :as u :refer [inline--fn inline--fn1]]

    [woof.test-data :as test-data]

    [woof.test-wf :as t]

))



;;
;; pub-sub for workflows



;; store executors, so the can be stopped

(def *server-wf (atom nil))
(def *client-wf (atom nil))

;; references for in/out channels

(def *server-in (atom nil))
(def *server-out (atom nil))



;; shared context between the client/server
(def context-base
  {
    ;; identity functio
    :v  {:fn (fn[a] (identity a))}

    ;; debug
    :log  {:fn (fn[a]
                 (locking *out* (println "DBG:" a))
                 (identity a))}

    :log*  {:fn (fn[a]
                 (locking *out* (println "DBG:" a))
                 (identity a))
            :collect? true
            }

    ;; reference
    :&v {:fn (fn[a]
               {(wf/rand-sid) [:v a]})
         :expands? true}

    ;; collect
    :v* {:fn identity :collect? true}

    ;; zipper
    :zip {:fn (fn [vs]
                (partition (count vs) (apply interleave vs)))
          :collect? true}

    ;; just for testing infinite workflows
    :tick {:fn (fn [t]
                 (let [chan (async/chan)]
                   (go-loop []
                            (async/<! (u/timeout t))
                            (async/>! chan (System/currentTimeMillis))
                            (recur))
                   chan))
           :infinite true
           }

    })



;; todo: use handler generators


(def *pub-chan (atom nil))
(def *sub-chan (atom nil))

;; client subs to server


(defn infinite-expand-handler
  "infinite expand handler that waits for the steps from the channel param"
  []
  {
    :fn (fn[in>]
          (let [chan> (async/chan)]
            (go-loop []
                     (when-let [v (async/<! in>)]
                       (locking *out* (println "INFINITE " (d/pretty v)))
                       (if-not (map? v)
                         (u/throw! (str "invalid expand map passed to :in " (d/pretty v))))

                       ;; (locking *out* (println "PUB: IN" (d/pretty v)))
                       (async/put! chan> v)
                       (recur))
                     )
            chan>))
    :infinite true
    :expands? true
    }

  )


;; pub via :tags array
(defn- pub [pub-chan]
  ;; todo: add pub-fn

  {:fn (fn[msg] ;; {:tag [] :msg ...}
         (println "!PUB:" (d/pretty msg))

         (doseq [tag (:tags msg)]
           (println "sending... " tag {
                                        :tag tag
                                        :msg (:msg msg)})
           (go
             (async/>! pub-chan {
                        :tag tag
                        :msg (:msg msg)}))

           #_(async/>!! pub-chan {
                        :tag tag
                        :msg (:msg msg)})
           )
         :ok
         )
   :infinite true
   }

  )



;;;


(defn- pub-context-fn [& {:keys [pub-loop pub-chan publisher]}]
  ;; todo: check if params are present
  {
    :dbg {:fn (fn[v]
                (locking *out* (println "PUB: " (d/pretty v)))
                v)}

    :dbg* {:fn (fn[v]
                (locking *out* (println "PUB*: " (d/pretty v)))
                v)
           :collect? true}

    :pub-loop (infinite-expand-handler)


    :pub (pub pub-chan)

    :sub {:fn (fn [x]
                (locking *out* (println "subscribing: " (d/pretty x)))

                (let [channel (:chan x)]
                  (doseq [tag (:tags x)]
                    (async/sub publisher tag channel))
                  :ok))
          :infinite true}

    :unsub {:fn (fn[x]
                  (let [channel (:chan x)]
                    (doseq [tag (:tags x)]
                      (async/unsub publisher tag channel))

                    :ok))}

    :timeout-msg {:fn (fn [x]
                        (let [chan (async/chan)]
                              (go
                                ;; test sending with pauses
                               (async/<! (u/timeout (:t x)))
                               (async/put! chan (dissoc x :t))
                                )
                          chan
                          ))


                      }
    }
  )


(defn- pub-steps-fn [& {:keys [pub-loop]}]

  {
    ::loop [:pub-loop pub-loop]

    ;; ::log [:dbg "Hello Pub!"]
    ;; ::expand [:expand "Hello from expand!"]

    ::msg [:timeout-msg {:tags [:sub]
                          :msg "Hello Sub!"
                          :t 1000
                          }]
    ::pub [:pub ::msg]

 ;   ::msg1 [:timeout-msg {:tags [:sub]
 ;                         :msg "Hola Sub!"
 ;                         :t 12500
  ;                        }]
  ;  ::pub1 [:pub ::msg1]

 ;   ::msg2 [:timeout-msg {:tags [:sub]
 ;                         :msg "Saaaaad"
 ;                         :t 15000
 ;                         }]
 ;   ::pub2 [:pub ::msg2]


    }
  )



(defn- sub-context-fn [& {:keys [pub-loop sub-loop sub-chan]}]
  {
    :dbg {:fn (fn[v]
                (locking *out* (println "SUB: " (d/pretty v)))
                v)}

    :dbg* {:fn (fn[v]
                (locking *out* (println "SUB*: " (d/pretty v)))
                v)
           :collect? true}

    :sub-loop (infinite-expand-handler)


    :unsubscribe! {:fn (fn[x]
                       (go
                         (async/put! pub-loop
                                     {(wf/rand-sid) [:unsub (merge x
                                                                 {
                                                                   :chan sub-chan
                                                                   })]}
                                     ))
                       {(wf/rand-sid) [:dbg "Unsubscribed!"]}
                       )
                 :expands? true
                 }


    :timeout-msg {:fn (fn [x]
                        (let [chan (async/chan)]
                              (go
                                ;; test sending with pauses
                               (async/<! (u/timeout (:t x)))
                               (async/put! chan (dissoc x :t))
                                )
                          chan
                          ))
                      }

    ;; loop through subscriptions
    :sub*  {
             :fn (fn [in>] ;; add callback
                   (let [chan> (async/chan)]
                     (go-loop []
                              (when-let [v (async/<! in>)]
                                ;(locking *out* (println "SUB: IN" (d/pretty v)))
                                (async/put! chan> {
                                                    (wf/rand-sid) [:dbg v]
                                                    })
                                (recur)
                                )
                              )
                     chan>))

             :infinite true
             :expands? true

             }

    :subscribe! {:fn (fn[x]
                       (go
                         (async/>! pub-loop
                                   {(wf/rand-sid)
                                      [:sub x]}
                                   ))
                       {(wf/rand-sid) [:dbg "Subscribed!"]}
                       )
                 :expands? true
                 }



    }
  )


(defn- sub-steps-fn [& {:keys [sub-loop sub-chan]}]

  {
    ::loop [:sub-loop sub-loop]

    ::subscription [:sub* sub-chan]


    ;;::log [:dbg "Hello Sub!"]

    ;;::t [:timeout-msg {:t 5000
    ;;                   :tags [:sub]}]

    ::subscribe [:subscribe! {:tags [:sub]
                              :chan sub-chan
                              }]


 ;   ::t [:timeout-msg {:t 5000
 ;                       :tags [:sub]
 ;                      }]
 ;   ::unsubscribe [:unsubscribe! ::t]

    }
  )




(defonce *stop (atom []))

(let [pub-chan (async/chan)
      pub-loop (async/chan)
       pub-params {
                   :prefix "PUB:"
                   :pub-loop pub-loop
                   :pub-chan pub-chan
                   :publisher (async/pub pub-chan :tag)
                   }
      {pub-start! :start
       pub-stop!  :stop} (t/thread-wf! pub-params
                                      pub-context-fn
                                      pub-steps-fn)

      sub1-params {
                    :prefix "SUB1:"
                    :sub-loop (async/chan)
                    ;; :pub-chan pub-chan
                    :pub-loop pub-loop
                    :sub-chan (async/chan)
                    }

      {sub1-start! :start
       sub1-stop!  :stop} (t/thread-wf! sub1-params
                                       sub-context-fn
                                       sub-steps-fn)
      ]
  (reset! *stop [])
  (swap! *stop conj pub-stop! sub1-stop!)

  (do

    (pub-start!)

    (sub1-start!)



    )

  )



(async/thread

  (Thread/sleep 5000)
  (doseq [stop-fn @*stop]
    (stop-fn)
    )
)





