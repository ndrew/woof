(ns woof.wf.edn-editor.backend
  "backend wf for the edn editor"
  (:require
    [clojure.core.async :as async :refer [go go-loop]]

    [woof.data :as d]

    [woof.wf :as wf]
    [woof.wfc :as wfc]
    [woof.utils :as u]
    [woof.xform :as x]

    [woof.server.utils :refer [read-transit-str write-transit-str]]


    )
)


;;
;; stateful workflow for loading/saving text (edn) files.


;; ;; c) wf communication



;; state is map
;;
;;   {:current nil|<file>}
;;
;; where <file> is
;; {
;;    :path "/Users/ndrw/m/woof/test/data/config.edn"
;;    :contents ""
;; }


;; a) stateful step handlers

;;   [:state [selector]]      - gets data from state via selector
;;   [:state! [[selector] v]] - puts v into state map at selector


;; b) io

;;   [:read-current _]
;;   [:write-current _]


;; c) send to client
;;
;;   [:client> ...]



;;;;;;;;;;;;;;
;;
;; context


(defn- current [_]



  (let [sid1 (u/seq-sid "state-")
        sid2 (u/seq-sid "read-current-")]

      ;(locking *out* (println "CURRENT:"))
    {
      sid1 [:state [:current]]
      sid2 [:read-current sid1]
      (u/seq-sid "client>-") [:client> sid2] ;;sid2
      }

    )

  )


(defn- cd [f]
  (let [sid1 (u/seq-sid "state!-")
        sid2 (u/seq-sid "read-current-")]
    {
      sid1 [:state! [[:current] {:path f}]]
      sid2 [:read-current sid1]
      (u/seq-sid "client>-") [:client> sid2]
      }

    )

  )


(defn- contents! [data]
  (let [sid (u/seq-sid "content-")]
    {
      sid [:state! [[:current :contents] data]]
      (u/seq-sid "write-") [:write-current sid]

      (u/seq-sid "client>-") [:client> sid] ;[:client> sid]
      }))



(defn- safe-slurp [path]
  (try
    (slurp path)
    (catch java.io.FileNotFoundException e
      "")))



;;
;;

;; client message -> steps
(defn cmd->steps [msg]
  (locking *out* (println "GOT CMD:" (d/pretty msg)))


  (if-let [[op params] msg]
    (condp = op
      ;; todo: get current
      :current   (current params)
      :file      (cd params)
      :contents! (contents! params))
    {(wf/rand-sid) [:log (str "unknown message " (d/pretty msg))]}
    )
  )


;; value to send to client
(defn cmd->client [v]
  (locking *out* (println "cmd->client:" (d/pretty v)))

  ;; currently file always
  [:file v])




(defn context-fn [& {:keys [*local in-chan> out-chan< send-transit!]}]
  {
    ;; loop that waits for steps from the in> channel
    :wait-for-commands  {
                      :fn (x/global-shandler
                            (x/infinite-expand-rf (fn
                                                    ([] in-chan>)
                                                    ([cmd]
                                                     (cmd->steps cmd)
                                                     )
                                                    ([in-chan out-chan]))))
                      :infinite true
                      :expands? true
                      }

;; debug
    :log  {:fn (fn[a]
                 (locking *out* (println "DBG:" a))
                 (identity a))}

    :log*  {:fn (fn[a]
                 (locking *out* (println "DBG:" a))
                 (identity a))
            :collect? true
            }


    :full-state {:fn (fn[_]
                  @*local)
            }

;; state step handler
    :state {:fn (fn[selector]
                  (get-in @*local selector))
            }


    :state! {:fn (fn [[selector data]]
                  (d/update-value *local selector data))}



;; read current file
    :read-current {:fn (fn[_]
                         (let [current (get-in @*local [:current])
                               {path :path} current]

                           (:current (d/update-value *local [:current]
                                           (merge current {:contents (safe-slurp path)})))))
                   }

;;
    :write-current (wf/step-handler (fn [_]
                                   (let [current (get-in @*local [:current])]
                                     (do
                                       (spit (:path current) (:contents current))
                                       (:current current)))))

;;
    :client> {:fn (x/global-shandler
                    (x/channel-collect-rf
                      (fn
                        ([]    out-chan<)
                        ([v]

                           (locking *out* (println "SRV: out" v (cmd->client v)))

                           (send-transit! (cmd->client v))
                         v)
                        ([a b]))))
              :collect? true
              }

    }
  )




(defn steps-fn [& {:keys [initial-command]}]
  {
    ::main-loop [:wait-for-commands initial-command]

    })



;; workflow constructor

(defn wwf [in-chan> out-chan< *local
            params]

  ;; todo: add state

  (wfc/params-wf (merge params ;; pass the in/out channel in case they'll be needed in context or steps constructors
                        {
                          :in-chan> in-chan>
                          :out-chan< out-chan<
                          :*local *local
                          }
                        )
                 context-fn
                 steps-fn))


;; state


(defn default-state []
  {
    :current {:path "/Users/ndrw/m/woof/test/data/config.edn"}
  })






;; api for webserivce
(defn wf! []
  ;; pass here the default path

  (let [*local (atom (default-state))
        in-chan> (async/chan)
        out-chan< (async/chan)

        receive-fn (fn [payload]
                     (let [msg (read-transit-str payload)]
                       ;; (locking *out*  (println "SERVER RECEIVE:\n" (pr-str msg) "\n"))
                       (go
                         ;; call cmd-> steps inside wf
                         (async/put! in-chan> msg))) ;; (cmd->steps ) ;;
                     )
        close-fn (fn [status] ; :server-close/:client-close/:normal/:going-away/:protocol-error/:unsupported/:unknown

                   (locking *out* (println "STOP WS WF:" status))

                   (async/close! in-chan>)
                   (async/close! out-chan<)
                   )
        ]

    ;(endpoint-fn (partial wwf in-chan> out-chan<) receive-fn close-fn)
    {
      :wf (partial wwf in-chan> out-chan< *local)

      :params {
                :receive-fn receive-fn
                :close-fn close-fn
                }
    }
    )
  )




;; some questions:

;; why to pass the required params to a wf constructor fn directly instead of map

;; syncing expand steps -
;; expand1 -> steps -> more steps

;; so expand1 is ready when all sub-sequent steps are processed? or when the first batch is ready



;; tester




(defn wf-tester []
  (let [
         in (async/chan)
         out (async/chan)
         end-chan (async/chan)

         *local (atom (default-state))
         initial-command [:file "/Users/ndrw/m/woof/test/data/config2.edn"]

         ;; opts
         init-fn (fn [wf-chan xtor]

                    (go
                      (when-let [v (async/<! end-chan)]
                        (locking *out* (println "stopping wf"))
                        (wf/end! xtor)
                        ))

                    ;; simulate receiving from socket
                    (go-loop []
                             (if-let [v (async/<! out)]
                               (do
                                 (locking *out* (println "RECEIVED" v))
                                 (recur))
                               (do
                                 (locking *out* (println "STOPPED WF"))

                                 (async/close! in)
                                 (async/close! out)
                                 )
                               ))
                    :ok
                    )

          opts {
                 :before-process init-fn
                 :op-handlers-map {
                                    :done  (fn [data]
                                             (locking *out* (println "DONE!\n" (d/pretty data))))
                                    :error (fn [data]
                                             (locking *out* (println "ERROR!\n" (d/pretty data))))
                                    }
                 }

          ; wf constructor
          wwf (partial wwf in out *local)
          ; wf params

          ; simulate putting data on the wire
          socket-send (fn [v]
                        (go
                          (async/put! out v)))

          socket-send-transit (fn [v]
                                (go ; or use (write-transit-str v)
                                  (async/put! out v)))

          params {
                   :send! socket-send
                   :send-transit! socket-send-transit

                   :initial-command initial-command
                   }

          ]

      (wfc/wf-async-process! (wwf params) opts)


      ;; ::client [:client> "Yo"]

      ;; frontend sends new steps into workflow
      (go
        (async/<! (u/timeout 100))

        ;(async/put! in [:file "/Users/ndrw/m/woof/test/data/config1.edn"])

        (async/put! in [:current])

        ;(async/<! (u/timeout 100))
        ;(async/put! in [:contents! "new content"])
        )


      ;; simulate closing ws
      (go
        (async/<! (u/timeout 1000))
        (async/put! end-chan :done)
        )

      )
    )



;; (wf-tester)
