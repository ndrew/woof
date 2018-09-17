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
}))


(def cursor (partial rum/cursor-in *UI-STATE))




(defn- client-context-map []
  {
    ;; identity function
    :v  {:fn (fn[a] (identity a))}

    ;; debug
    :log  {:fn (fn[a]
                 (locking *out* (println "DBG:" a))
                 (identity a))}

    :input  {:fn (fn[a]
                 (locking *out* (println "INPUT:" (d/pretty a)))
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
  }
  )



;; CLIENT:
;;   send steps to server
;;   return back steps as :client>

(defn client-send! [server> v]  ;; todo add callback

  ; (.warn js/console (d/pretty v))

  (let [v-sid (wf/rand-sid)
        v-ref (wf/rand-sid "&")
        v*    (wf/rand-sid "*")
        zip   (wf/rand-sid "zip-")

        out   (wf/rand-sid ">")

        ;; emits the following steps
        server-steps {
                       ;; add a value (optional if edn)
                       v-sid [:v v]
                       v-ref [:&v v-sid] ;; get a reference to it
                       v*    [:v* [v-sid]] ;; get a value list
                       zip   [:zip [v-ref v*]] ;; interleave them

                       ;; specify what to return to client
                       out   [:client> zip]


                       }
        ]
    ;sends steps to server
    (go
      (async/>! server> server-steps))

    ; how to return added steps
    {(wf/rand-sid "out-") [:&v server-steps]}
))









(defn init-wf []
  (let [server-in (async/chan)
        server-out (async/chan)

        ui-chan (async/chan)

        context-map {
                      ;; main ui loop
                      :ui-loop {:fn (fn [in-chan]
                                      (u/wiretap-chan in-chan (partial println "UI LOOP:")))

                                :infinite true
                                :expands? true
                                }

                      ; waits for server responses from channel passed as parameter
                      :server< (wf/receive-steps-handler
                                 :step-fn
                                 (fn [steps]
                                   (println "CLIENT RECEIVE: " (d/pretty steps))
                                   steps))


                      ;; todo: move to wf
                      ;; sends steps to be executed by server
                      :server> {:fn (partial client-send! server-in)
                                :expands? true
                                }}

        ctx-map (merge (client-context-map) context-map)
        steps {

               ::ui  [:ui-loop ui-chan]

               ::server-loop [:server< server-out]

              ;; ::tick [:tick 3000]
              ;; ::test [:v "HELLO"]
              ;; :payload [:server> "HELLO"]
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
            :server (ws/ws-server "/api/websocket"
                                  :on-message (fn [msg]
                                    (println "got " (d/pretty msg))
                                    (go
                                      (async/put! server-out {
                                          ;; todo: transform msg to a client wf action
                                         (wf/rand-sid) [:input msg]}))))

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


(defn start-server-action [server]
  (ws/start server)
  (ws/get-socket server))





(rum/defcs <ws-tester> < rum/reactive
  (rum/local nil  ::socket)

  [local *STATE]

  (let [state @*STATE
        actions [ ["init ws"
                   (fn[]
                     (reset! (::socket local)
                             (start-server-action (:server state)))


                     ;; todo: wait for socket to open

                     )]]]
    [:div
      (ui/menubar "Server:"
                  (if @(::socket local)
                    (conj actions ["start ws!"
                                   (fn[]
                                     ;; puts transit on a wire
                                     ;;          (ws/send! (:server state) [:server-time ""])

                                     (wf/process-results! (wf/->ResultProcessor (:xtor @*STATE)
                                                                                 (:process-opts @*STATE)))

                                     (go-loop []
                                              (let [z (async/<! (:server-in @*STATE))]
                                                (inline--fn1 (fn [v]
                                                               (println "sending to server " (d/pretty z))
                                                               (ws/send! (:server @*STATE) z)
                                                               ) z)



                                                )
                                              (recur))


                                     )]

                                  ["ui" (fn []
                                          (go
                                              (async/>! @(cursor [:ui-chan])
                                                        {(wf/rand-sid) [:server> (str "click - " (.getTime (js/Date.)))]}
                                                        ))
                                          )]
                                  ["stop" (fn []
                                            (ws/close! (:server state))
                                            (reset! (::socket local) nil)

                                            ; (async/close! (:server-in @*STATE))

                                            )]
                                  )
                    actions
                    )
                  )

      [:pre (d/pretty state)]
      [:hr]
      [:pre (d/pretty @(::socket local))]
     ]
    )
  )
