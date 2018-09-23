(ns woof.example.files-ws
  (:require
    [cljs.core.async :as async]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.ws :as ws]

    [woof.utils :as u]
    [woof.ui :as ui]
    [woof.wf-ui :as wf-ui]

    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


  ;; example of client server communication

  ;; list files from folder


(declare responce-fn)



(defn prepare-params! [endpoint-url]
  (let [server-in (async/chan)
        server-out (async/chan)
        endpoint (ws/ws-server endpoint-url
                                  :on-message (fn [msg]
                                    (if-let [steps (responce-fn msg)]
                                      (go
                                        (async/put! server-out steps)))))]

  {:server-in server-in
   :server-out server-out

   :endpoint endpoint

   :ui-chan (async/chan)

   })
  )


(defn responce-fn [msg]
  (println "got response" (d/pretty msg))

  ;; todo
  {(wf/rand-sid) [:log msg]}
  )


(defn context-map-fn [& {:keys [ui-chan server-in server-out endpoint ]}] ;;
  {
    :ui-loop {:fn (fn [in-chan]
                    (u/wiretap-chan in-chan (partial println "FILES UI LOOP:")))
              :infinite true
              :expands? true
              }


    ; waits for server responses from channel passed as parameter
    :server< (wf/receive-steps-handler
               :step-fn
               (fn [steps]
                 (println ["server->client:" steps])
                 steps))


    ;; sends steps to be executed by server
    :server> (wf/send-value-handler* server-in
                            :v-fn (fn[v]
                                    (let [v-sid (wf/rand-sid)
                                          v-ref (wf/rand-sid "&")
                                          v*    (wf/rand-sid "*")
                                          zip   (wf/rand-sid "zip-")

                                          out   (wf/rand-sid ">")

                                          ]
                                      ;; emits the following steps

                                      {
                                        ;; add a value (optional if edn)
                                        v-sid [:v v]
                                        v-ref [:&v v-sid] ;; get a reference to it
                                        v*    [:v* [v-sid]] ;; get a value list
                                        zip   [:zip [v-ref v*]] ;; interleave them

                                        ;; specify what to return to client
                                        out   [:client> zip]

                                        })

                                    )
                            :out-fn (fn [z]
                                      (ws/send! endpoint z)
                                      )

                            :return-fn (fn[v vs]
                                         (let [sid (wf/rand-sid "out-")]
                                           {sid [:&v vs]
                                            (wf/rand-sid) [:log ["client-server:" v "->" vs]]})
                                         )
                            )


    :server! ;; {:fn (partial client-send! server-in) :expands? true }
    (wf/send-value-handler* server-in
                            :v-fn (fn[v]
                                    v
                                    )
                            :out-fn (fn [z]
                                      (ws/send! endpoint z)
                                      )

                            :return-fn (fn[v vs]
                                         (let [sid (wf/rand-sid "out-")]
                                           {sid [:&v vs]
                                            (wf/rand-sid) [:log ["client-server:" v "->" vs]]})
                                         )
                            )


    :log {:fn (fn[v]
                (println v)
                v
                ) }
    })


(defn steps-fn [& {:keys [ui-chan server-in server-out]}]
    {
      ::ui  [:ui-loop ui-chan]

      ::server-loop [:server< server-out]

      ::hello [:log "Hello!"]

      ;; ::payload [:server> "HELLO"]


      ;; uncomment this for infinite send
      ; ::tick [:tick 3000]
      ; ::payload [:server> ::tick]

      }
  )


(defn actions-fn [& {:keys [ui-chan server-in server-out endpoint]}]
  {:start! (fn []
             ;; may return channel
             (ws/start endpoint))
   :stop!  (fn []
             (ws/close! endpoint)

             (async/close! ui-chan)
             (async/close! server-in)
             (async/close! server-out)
             )
   :reset! (fn[]
             ; (println "reset!")
             )

   :actions [
              ["send!" (fn[]
                         (go ;; add new :server> step via :ui-chan
                           (async/>! ui-chan
                                     {(wf/rand-sid) [:server> (str "click - " (.getTime (js/Date.)))]}
                                     )))]

             ;  ["expand"
             ;   (fn []

              ;    (let [steps (d/to-primitive
              ;                  (js/prompt "provide step as map, like {:a/a [:server! {:a/a [:log \"hello\"]}]}"))]
              ;      (go ;; add new :server> step via :ui-chan
               ;       (async/>! ui-chan
               ;                 steps))))
               ; ]
              ]
   })
