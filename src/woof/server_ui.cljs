(ns woof.server-ui
  (:require
    [cljs.core.async :as async]
    [rum.core :as rum]

    [woof.app-data :as app-model]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.ws :as ws]

    [woof.utils :as u]
    [woof.ui :as ui]
    [woof.wf-ui :as wf-ui]

    [clojure.data :as cd])
    (:require-macros
      [cljs.core.async.macros :refer [go go-loop]]
      [woof.utils-macros :refer [inline--fn inline--fn1]])
  )



;;
;; state

(defonce *UI-STATE (atom {
  :server nil

  ;; save communication history
  :log []
}))


(def cursor (partial rum/cursor-in *UI-STATE))

(def *LOG (cursor [:log]))



(defn- client-context-map []
  {
    ;; identity function
    :v  {:fn (fn[a] (identity a))}

    ;; debug
    :log  {:fn (fn[a]
                 (swap! *LOG conj ["DBG" a])
                 (identity a))}

    :input  {:fn (fn[a]

                 (swap! *LOG conj ["INPUT" a])
                 ; (locking *out* (println "INPUT:" (d/pretty a)))
                 (identity a))}


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

    :tick {:fn (fn [t]
                 (let [chan (async/chan)]
                   (go-loop []
                            (async/<! (u/timeout t))
                            (async/>! chan (.getTime (js/Date.)))
                            (recur))
                   chan))
           :infinite true
           }
  }
  )



;; todo: pass transform fn for ws
;; todo: pass send value handlers


;; ws-url

;; context fn

;; steps fn

(defn init-wf []
  ;; needed to update state map

  (swap! *UI-STATE merge {
                           :log []

                           :server nil

                           :ctx {}
                           })
)

;; menu buttons


(defn prepare-context-map [endpoint server-in server-out & {:keys [ui-chan]}] ;;
  (merge (client-context-map)
  {
    ;; main ui loop
    :ui-loop {:fn (fn [in-chan]
                    (u/wiretap-chan in-chan (partial println "UI LOOP:"))
                    )

              :infinite true
              :expands? true
              }

    ; waits for server responses from channel passed as parameter
    :server< (wf/receive-steps-handler
               :step-fn
               (fn [steps]
                 (swap! *LOG conj ["server->client:" steps] )
                 steps))


    ;; sends steps to be executed by server
    :server> ;; {:fn (partial client-send! server-in) :expands? true }
    (wf/send-value-handler* server-in
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
    })
  )


(defn responce-fn [msg]
  (println "got response" (d/pretty msg))
  {(wf/rand-sid) [:input msg]}
  )


(defn steps-fn [server-in server-out & {:keys [ui-chan]}]
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


(defn init-test-wf [*STATE
                    endpoint-url ;; "/api/websocket"
                    responce-fn  ;; ws output to steps
                    context-fn
                    steps-fn
                    ]
  (let [server-in (async/chan)
        server-out (async/chan)

        ui-chan (async/chan)

        endpoint (ws/ws-server endpoint-url
                                  :on-message (fn [msg]
                                    (if-let [steps (responce-fn msg)]
                                      (go
                                        (async/put! server-out steps)))))

        ctx-map (context-fn endpoint server-in server-out :ui-chan ui-chan)
        steps (steps-fn server-in server-out :ui-chan ui-chan)

        xtor (wf/build-executor (wf/make-context ctx-map) steps)

        processing-opts {
            :op-handlers-map {
                               :done (fn [data]
                                       (println "DONE!")
                                       )
                               :error (fn [data]
                                        (.error js/console "ERROR" data)
                                        ;(reset! (cursor [:status]) :woof.app/error)
                                        )

                               }
            }

        ]

  {
    :xtor xtor
    :endpoint endpoint


    :start! (fn[]
      (let[ch (ws/start endpoint)]
        (go
          (if-let [socket (async/<! ch)]
            (do
              ; (reset! *socket socket)
              (let [proc (wf/->ResultProcessor xtor processing-opts)]
                ;;(reset! *processor proc)
                (wf/process-results! proc)
                )
              )))
        )

              )

    :stop! (fn[]
      (ws/close! endpoint)
      ; (reset! *socket nil)

      (wf/end! xtor)

      (async/close! ui-chan)

      (async/close! server-in)
      (async/close! server-out)


      (init-wf)
    )

    :actions [
               ["send!" (fn[]
                         (go ;; add new :server> step via :ui-chan
                           (async/>! ui-chan
                                     {(wf/rand-sid) [:server> (str "click - " (.getTime (js/Date.)))]}
                                     )))]

               ["expand"
                (fn []

                  (let [steps (d/to-primitive
                                (js/prompt "provide step as map, like {:a/a [:server! {:a/a [:log \"hello\"]}]}"))]
                    (go ;; add new :server> step via :ui-chan
                      (async/>! ui-chan
                                steps))))
                ]

               ]


    }

  )
)





(rum/defcs <ws-tester> < rum/reactive

  (rum/local nil  ::wf)

  [local *STATE]

  (let [actions
        (if-let [wf @(::wf local)]
          (into
            [["connect to WS:" (:start! wf)]
             ["stop WS:" (fn[]
                           ((:stop! wf))
                           (reset! (::wf local) nil)
                           )]
             []
             ]
            (:actions wf)
            )
          ;; contexts
          [ ["test context" (fn []
                              (reset! (::wf local)
                                      (init-test-wf *STATE
                                                    "/api/websocket"
                                                    responce-fn
                                                    prepare-context-map
                                                    steps-fn
                                                    ))
                              )]]
          )



        ]
    [:div
     (ui/menubar "Server:" actions)


      [:pre (d/pretty @(::wf local))]
      [:h4 "Log:"]
      [:pre (d/pretty @*LOG)]
     ]
    )
  )
