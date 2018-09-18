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

  :history []
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
(defn init-wf []
  (let [server-in (async/chan)
        server-out (async/chan)

        ui-chan (async/chan)

        context-map {
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
                                                        (ws/send! (:server @*UI-STATE) z)

                                                        )
                                              :return-fn (fn[v vs]
                                                           (let [sid (wf/rand-sid "out-")]
                                                             {sid [:&v vs]
                                                              (wf/rand-sid) [:log ["client-server:" v "->" vs]]})
                                                           )
                                              )
                      }

        ctx-map (merge (client-context-map) context-map)
        steps {

               ::ui  [:ui-loop ui-chan]

               ::server-loop [:server< server-out]

               ::hello [:log "Hello!"]

                ;; ::payload [:server> "HELLO"]

                ;; todo:
                ::tick [:tick 3000]
                ::payload [:server> ::tick]

            }

        xtor (wf/build-executor (wf/make-context ctx-map) steps)

        processing-opts { ;:execute start-wf-with-transducers!
            ; :before-process before-processing!
            ;; :process-handler op-handler

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



  (swap!
    *UI-STATE
    merge {
            :log []
            :server (ws/ws-server "/api/websocket"
                                  :on-message (fn [msg]
                                    ;; (println "got " (d/pretty msg))
                                    (go
                                      ;; todo: transform msg to a client wf action
                                      ;; todo: add to infinite step?
                                      (async/put! server-out
                                                  {
                                                    (wf/rand-sid) [:input msg]})))

                                  )

            :server-in server-in
            :server-out server-out

            ; :client-context-map ctx-map
            :client-steps steps

            :ctx ctx-map

            :ui-chan ui-chan

            :xtor xtor
            :process-opts processing-opts

            }))

  )


;; menu buttons

(defn connect-action [state *socket *processor]
  (let [{xtor :xtor
         processing-opts :process-opts
         server :server
         } state

        ch (ws/start server)]
    (go
      (if-let [socket (async/<! ch)]
        (do
          (reset! *socket socket)
          (let [proc (wf/->ResultProcessor xtor processing-opts)]
            (reset! *processor proc)
            (wf/process-results! proc)
            )
          )))))

(defn disconnect-action [state *socket *processor]
  ;;
  (ws/close! (:server state))
  (reset! *socket nil)

  ;; close

  (wf/end! (:xtor state))
  ;; FIXME: close all channels
  (async/close! (:ui-chan state))
  (async/close! (:server-in state))
  (async/close! (:server-out state))


  (init-wf))

;;


(rum/defcs <ws-tester> < rum/reactive
  (rum/local nil  ::socket)
  (rum/local nil  ::processor)

  [local *STATE]

  (let [actions [ ["connect"
                   (partial connect-action @*STATE
                                          (::socket local)
                                          (::processor local)
                            )]]
        ]
    [:div
     (ui/menubar
       "Server:"
       (if @(::socket local)
         (conj actions
               ["send click"
                (fn []
                  (go ;; add new :server> step via :ui-chan
                    (async/>! @(cursor [:ui-chan])
                              {(wf/rand-sid) [:server> (str "click - " (.getTime (js/Date.)))]}
                              ))
                  )]

               ["send custom"
                (fn []
                  (let [steps (d/to-primitive (js/prompt "provide step as map "))]
                    (go ;; add new :server> step via :ui-chan
                      (async/>! @(cursor [:ui-chan])
                                steps))))
                ]


               ["stop" (partial disconnect-action @*STATE (::socket local) (::processor local))]
               )
         actions
         )
       )

      [:h4 "Log:"]
      [:pre (d/pretty @*LOG)]
     ]
    )
  )
