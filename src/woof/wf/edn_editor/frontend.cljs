(ns woof.wf.edn-editor.frontend
  "frontend for the edn editor"
  (:require
    [cljs.core.async :as async]
    [woof.data :as d]
    [woof.wf :as wf]
    [woof.wfc :as wfc]
    [woof.xform :as x]
    [woof.utils :as u]

    [rum.core :as rum]
    [woof.ws :as webservice]
    [woof.ui.state :as ui-state]
    [woof.wf-ui :as wf-ui]

    [woof.core.runner :as runner]

    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



;;;;;;;;;;;;;;;;;
;;
;; context
;;
;;   todo: add steps


(defn context-fn [& {:keys [
                             *local                    ; ui
                             send! in-chan> out-chan<  ; ws
                             ]}]

  {

    :log  {:fn (fn[a]                                     ;; debug
                 (locking *out* (println "DBG:" a))
                 (identity a))}

    :send! {:fn (fn[a]
                  ;; send is via out-chan< or not?
                  (send! a)
                  )}


    :state {:fn (fn[selector]
                  (get-in @*local selector))
            }


    ;; todo: how to sync if it's expand selector
    :state! {:fn (fn [[selector data]]
                  (d/update-value *local selector data))
             }


    :ui-loop  {
                :fn (x/global-shandler
                      (x/infinite-expand-rf (fn
                                              ([] in-chan>) ;; return channel

                                              ([steps]
                                               (locking *out* (println "EDN: new commands: " (d/pretty steps)))
                                               steps)

                                              ([in-chan out-chan]

                                               (locking *out* (println "EDN: closing :wait-for-commands" in-chan out-chan))
                                               ;; when this is called

                                               )
                                              )))
                :infinite true
                :expands? true
                }


    }
  )


;;;;;;;;;;;;;;;;;
;;
;; steps
;;

(defn steps-fn [& {:keys [initial-steps]}]
  {
    ;; infinite loop
    ::main-loop [:ui-loop initial-steps]
  })



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; workflow function



(defn wwf-fn [in-chan> out-chan< *local  ;; <- these should be partially applied ;; <?> why these are not in params?
           params]

  (let [defaults {
                   :in-chan> in-chan>
                   :out-chan< out-chan<
                   :*local *local
                   }
        all-params (merge params ;; pass the in/out channel in case they'll be needed in context or steps constructors
                          defaults)]

    (wfc/params-wf all-params context-fn steps-fn)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; workflow constructor





(defn wf!
  "*local - state atom for ui updates"
  [*local initial-params]

  ;; inits necessary channels/resources for wf

  (let [in-chan> (async/chan)
        out-chan< (async/chan)


        server-msg->steps (fn [msg]
                            (let [[op v] msg]

                              {
                                ;; just save the data from server to state
                                (wf/rand-sid) [:state! [[:wf :wf-state op] v]]
                                }
                              )
                            )

        client-msg->server-steps (fn [msg]
                                   msg)

        receive-fn (fn [msg]

                     (println "SERVER RECEIVE:\n" (pr-str msg) "\n")

                     ;;
                     ;; =>  uncomment for adding actual steps
                     ;;

                     (let [new-steps (server-msg->steps msg) ]
                       (go
                         (async/put! in-chan> new-steps)))

                     )
        close-fn (fn [status]
                   (async/close! in-chan>)
                   (async/close! out-chan<)
                   )


        server-msg! (fn [msg]
                   (async/put! in-chan>
                               {
                                 (u/seq-sid "send-") [:send! msg]
                                 })
                   )
        ]

    {
      :wf (partial wwf-fn in-chan> out-chan< *local)

      :params (merge initial-params
                     {
                :receive-fn receive-fn
                :server-msg-fn server-msg->steps

                :api {
                       :server-msg server-msg!
                       :current (fn[]
                                  (server-msg! [:current nil]))
                       }

                :actions [
                           ["currently selected file"
                            (fn[]
                              (server-msg! [:current nil]))
                            ]

                           ["test change file" (fn[]
                                                 (server-msg! [:file "/Users/ndrw/m/woof/test/data/config1.edn"])
                                                 )]

                           ["test writting to file" (fn[]
                                                      (server-msg! [:contents! "azazazaz"]))]

                           ] ;; todo: add some actions
                })
      }
    )
  )










(defn opts-fn
  "default wf-fn for the config workflow

  * registers a ws endpoint /api/config
  * uses default ui-opts handler
  "
  [*STATE params]

  (runner/merge-full-opts
    (webservice/ws-opts "/api/config" *STATE params)
    (ui-state/ui-opts *STATE params))

  )







(rum/defc <file-editor> < rum/reactive [*STATE]
  (let [cursor (partial rum/cursor-in *STATE)
        *wf (cursor [:wf])
        *ui (cursor [:wf :wf-state])
        ;{{} :file} @*ui
        ]
    [:div

     (wf-ui/<wf-menu-ui>
           "config editor:"
           @(cursor [:wf :status])
           @(cursor [:wf :status-actions]))

     (if-let [ui @*ui]
       (do
        [:div
         [:pre (d/pretty @*ui)]
         ]
         )
       (do
        [:div
          "UI NOT INITIALIZED"
         [:pre (d/pretty (keys @*wf))]
         ]
         )

       )


     ]
    )

  )


(defn ui-fn
  [WF opts woof-ui-fn]
   ;; how to access the wf inner state?
   (let [params (wfc/get-params WF)
         context-map (wfc/get-context-map WF)
         steps (wfc/get-steps WF)

         {
           *STATE :*state
           actions :actions
           } params

         xtor (wfc/wf-xtor WF)


         start-fn (fn []
                    ;;(js-debugger)

                    (wf/process-results! (wf/->ResultProcessor xtor opts))
                    ;; default processing
                    )

         stop-fn  (fn []
                    (wf/end! xtor))

         reset-fn (fn []
                   (woof-ui-fn *STATE) ;;
                   )

         ]


     ;; pass here initial ui state

     (woof-ui-fn *STATE
       {
         :steps steps
         :context-map context-map

         :opts params  ;; rename

         ; :args args
         :status :woof.app/not-started
         :status-actions (ui-state/status-actions start-fn stop-fn reset-fn actions)

         :start! start-fn
         :stop! stop-fn
         :reset! reset-fn


         ;; the place where workflow can store data
         :wf-state {}


         }

       (fn [*STATE]
         (<file-editor> *STATE))
       )
     )
)

















;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; test



;; server-wf emulator

(defn test-backend-wf [in out *server-state initial-steps frontend-send-fn ]
  ;; mimic the server workflow
  (let [
         init-fn (fn [wf-chan xtor]
                   ;; todo:
                   )
         opts {
                :before-process init-fn
                :op-handlers-map {
                                   :done  (fn [data] (locking *out* (println "SERVER: done!\ndata:\n" (d/pretty data))))
                                   :error (fn [data] (locking *out* (println "SERVER: error!\n" (d/pretty data))))
                                   }
                 :timeout 15000
                }

         params {
                  :*state *server-state
                  :initial-steps initial-steps
                  }


         loop-xform (fn
                      ([] in) ;; return channel

                      ([steps]
                       (locking *out* (println "SERVER: new commands: " (d/pretty steps)))
                       steps)

                      ([in-chan out-chan]
                       (locking *out* (println "SERVER: closing :event-loop" in-chan out-chan))
                       ;; when this is called

                       )
                      )



         out-xform (fn
                     ([]
                      out)
                     ([v]

                      (frontend-send-fn [:file v])

                      ;(send-transit! [:to-client v])
                      v)
                     ([a b]
                      ;(locking *out* (println "end!" a b))
                      )
                     )

         fake-slurp (fn [path]
                       "{:edn :test}"
                      )

         ;; todo: use read backend wf?
         context-fn (fn[& {:keys [*state]}]
                      {

                        ;; state functions
                        :state {:fn (fn [selector]
                                      (get-in @*state selector))
                                }


                        :state! {:fn (fn [[selector data]]
                                       (d/update-value *state selector data))
                                 }

                        ;; instand read
                        :read-current {:fn (fn[_]
                                             (let [current (get-in @*state [:current])
                                                   {path :path} current]

                                               (:current
                                                 (d/update-value *state [:current]
                                                                 (merge current {:contents
                                                                                 (fake-slurp path)}))
                                                 )

                                               )
                                             )
                                       }

                        :client! {:fn (x/global-shandler (x/channel-collect-rf out-xform))
                                  :collect? true
                                  }

                        :log  {:fn (fn[a]
                                     (locking *out* (println "SERVER DBG:" a))
                                     (identity a))}
                            ;; loop that waits for steps from the in> channel
                        :event-loop  {
                                       :fn (x/global-shandler (x/infinite-expand-rf loop-xform))
                                       :infinite true
                                       :expands? true
                                       }


                      }
                      )

         steps-fn (fn[& {:keys [initial-steps]}]
                    {
                      ;;::log [:log "Hello"]

                      ::loop [:event-loop initial-steps]
                      }
                    )

         wwf (wfc/params-wf params context-fn steps-fn)]

    (let [xtor (wfc/wf-xtor wwf)]

      {
        :close (fn []
                 (wf/end! xtor)

                 (async/close! in)
                 (async/close! out)
                 )

        :run (fn[]
               (wf/process-results! (wf/->ResultProcessor xtor opts)))
        }

      )


    )
  )



(defn test-frontend-wf [in out *local initial-steps backend-send-fn]
  (let [


         ;; internal stuff
         end-chan (async/chan)

         ; simulate WS workflow API
         socket-send         (fn [v] (go (async/put! out v)))
         socket-send-transit (fn [v] (go (async/put! out v)))

         ;; opts
         init-fn (fn [wf-chan xtor]
                   (go
                     (when-let [v (async/<! end-chan)]
                       (locking *out* (println "stopping wf"))
                       (wf/end! xtor)))

                   ;; simulate receiving from socket                         ;; todo: move to a fn
                   (go-loop []
                            (if-let [v (async/<! out)]
                              (do ;; ws

                                (backend-send-fn v)
                                (recur))
                              (do
                                (locking *out* (println "EDN: stopping wf"))

                                (async/close! in)
                                (async/close! out))))
                   :ok)

         opts {
                :before-process init-fn
                :op-handlers-map {
                                   :done  (fn [data] (locking *out* (println "EDN: done!\ndata:\n" (d/pretty data))))
                                   :error (fn [data] (locking *out* (println "EDN: error!\n" (d/pretty data))))
                                   }
                :timeout 15000
                }

         ; wf params
         params {
                  :send! socket-send
                  :send-transit! socket-send-transit
                  :initial-steps initial-steps
                  }

         ;;
         wwf ((partial wwf-fn in out *local) params)
         ]

    (let [xtor (wfc/wf-xtor wwf)]
      {
        :close (fn [] (wf/end! xtor))
        :run (fn [] (wf/process-results! (wf/->ResultProcessor xtor opts)))
        }

      ))
)


;; edn editor frontend wf

;; * get name and it contents from the ws
;;   [:server! {
;;             ::current [:current ""]
;;             (wf/rand-sid) [:client! ::current]
;;            }]
;; * change filename
;; * change content
;; * link to ui wf



#_(let [
       ;; shared channels
       backend-in (async/chan)
       backend-out (async/chan)

       frontend-in (async/chan)
       frontend-out (async/chan)

       ;; msg from backend -> wf steps
       frontend-msg-fn (fn [[cmd v]]
                        (locking *out* (println "EDN: got" v))

                         (condp = cmd
                          :file (let [s (wf/rand-sid)]
                                  {
                                  s [:state! [[:file] v]]
                                  ;; (wf/rand-sid) [:log s]

                                  })

                          :else {(wf/rand-sid) [:log v]}
                           )
                        )

       ;; msg from frontend -> wf steps
       backend-msg-fn (fn [[cmd v]]
                        (locking *out* (println "SERVER: got" [cmd v]))

                        (condp = cmd
                          :set-file (u/subsitute-with-rand-sids {:client! {:collect? true}}
                                      {

                                        ::set-current [:state! [[:current] v]]
                                        ::read-current [:read-current ::set-current]
                                        ::send-client [:client! ::read-current]
                                        }
                                      )

                          :else {(wf/rand-sid) [:log v]}
                          )

                        )


       ;; emulate sending msg to frontend
       frontend-send-fn (fn [v]
                          (locking *out* (println "SERVER: sending to client" v))

                          (go
                            (async/put! frontend-in (frontend-msg-fn v))))

       ;; emulate sending msg to backend
       backend-send-fn (fn [v]
                        (locking *out* (println "EDN: sending to server" v))
                        (go
                            (async/put! backend-in (backend-msg-fn v)))
                         )

       ;; BACKEND
       *backend-state (atom { ;; intial state - the default file
                             :current {:path "Users/ndrw/m/woof/example.edn"}})


       backend-initial-steps {
                              ;; get default config file from state
                               ::initial-file [:state [:current]]

                               ::log [:log ::initial-file]

                              ;; read file
                               ::initial-read [:read-current ::initial-file]
                              ;; send to client
                              ::initial-client [:client! ::initial-read]
                               }





       ;; FRONTEND

       *frontend-state (atom {
                               ;;:current {:path "Users/ndrw/m/woof/example.edn"}
                               :file nil

                               })  ;; todo: substitute with cursor

       frontend-initial-steps {
                                ;; ::yo [:send! "yo"]
                                }


       { ;; init frontend
         run-frontend :run
         close-frontend :close
       } (test-frontend-wf frontend-in frontend-out *frontend-state frontend-initial-steps backend-send-fn)


       { ;; init backend
         run-backend :run
         close-backend :close
       } (test-backend-wf backend-in backend-out *backend-state backend-initial-steps frontend-send-fn)

       ]


  (run-frontend)

  (run-backend)

  (go ;; emulate frontend sending
    ;;
    (async/<! (u/timeout 100))
    (println "11111111111")
;; send to backend via step handler
    (backend-send-fn [:set-file {:path "abcdef"}])


    (async/<! (u/timeout 800))
    (println "222222222222")
    (async/put! frontend-in
                {
                  ::zzz [:state! [[:file :contents] "ot givno"]]
                  ::lll [:log ::zzz]

                  ;; ::sss [:send! ::zzz]
                }
                )
    (u/timeout 200)
    ;;(backend-send-fn [:set-contents {:path "abcdef"}])





    )


;; todo: close wfs implicitly

  ;; backend wf tester (via messages)
  ;; frontend wf tester (via fake backend wf)
  ;;
  )
