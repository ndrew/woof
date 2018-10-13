(ns woof.example.edn-editor.fs
  "example of the workflow connected to a websocket"
  (:require
    [clojure.core.async :as async :refer [go go-loop]]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.wfc :as wfc]
    [woof.utils :as u :refer [inline--fn1]]

    [woof.xform :as x]

    [woof.server.utils :refer [read-transit-str write-transit-str]]
    )
)



;; edn file editing example

;; base functions

;;   (slurp "/path") - read file from /path
;;   (spit "/path" value)  - write value into file at /path


;; a) state-less handlers

;; use a map for representing a file info and contents
;;   {:path "/path" :contents "" }  and maybe some additional stuff like meta or whatever


;; [:path-to-file "/path"] -> {:path "/path"}
;; [:read-file {:path "/path"}] -> {:path "/path"} + {:contents "..."}
;; [:write-file {:path "/p" :contents "..."}]


;; b) stateful handlers

;; [:state [selector]] - reads v from state mao atom
;; [:state! [[selector] v]] - writes v to state map


;; [:read-current <steps-after-loaded>] ;

;; [:set-current f]
;; [:write-current "text"/nil]

;; context map


(defn context-fn [& {:keys [send-transit! in-chan> out-chan< *local]}]
  ;; todo: migrate :client< to be a xform global-shandler


  {

    ;; loop that waits for steps from the in> channel
    :wait-for-commands  {
                      :fn (x/global-shandler
                            (x/infinite-expand-rf (fn
                                                    ([] in-chan>) ;; return channel

                                                    ([steps]
                                                     (locking *out* (println "new commands: " (d/pretty steps)))
                                                     steps)

                                                    ([in-chan out-chan]

                                                     (locking *out* (println "closing :wait-for-commands" in-chan out-chan))
                                                     ;; when this is called

                                                     )
                                                    )))
                      :infinite true
                      :expands? true
                      }

    :log  {:fn (fn[a]                                     ;; debug
                 (locking *out* (println "DBG:" a))
                 (identity a))}

    ;;

    :read-file (wf/step-handler (fn [path]
                                  {
                                    :path path
                                    :contents (slurp path)
                                   }
                                  ))

    :write-file (wf/step-handler (fn [f]
                                  (do
                                    (spit (:path f) (:contents f))
                                    f
                                    )
                                  ))

  ;;

    :state {:fn (fn[selector]
                  (get-in @*local selector))
            }

    :state! {:fn (fn [[selector data]]
                  (d/update-value *local selector data)
                  {
                    (wf/rand-sid) [:state selector]
                    }
                  )
             :expands? true
             }


    ;; expand returns refs so additioal sync is needed  ;;  + reusing existing step handlers

    :set-current {:fn (fn[f]
                        {(wf/rand-sid) [:state! [[:current] f]]}
                        )
                  :expands? true
                  }


    ;; instand read
    :read-current {:fn (fn[_]
                         (let [current (get-in @*local [:current])
                               {path :path} current]
                           (d/update-value *local [:current] (merge current {:contents (slurp path)}))
                           )
                         )


                   }


    ;;
    :sync {:fn identity
           :collect? true
           }


    ;;

    :v  {:fn (fn[a] (identity a))}                        ;; identity function
    :&v {:fn (fn[a]                                       ;; reference
               {(wf/rand-sid) [:v a]})
         :expands? true}
    :v* {:fn identity :collect? true}                     ;; collect

    :zip {:fn (fn [vs]                                    ;; zipper
                (partition (count vs) (apply interleave vs)))
          :collect? true}

;    :debug (wf/step-handler (fn[x]                        ;; older steps
;                              (println "DBG: " (d/pretty x))
;                              x))

    ;; debug step handler that returns debug info to client
    :debug {:fn (fn[x]
                  (send-transit! [:debug x])
                  x)}


    :client> {:fn (x/global-shandler
                    (x/channel-collect-rf
                      (fn
                        ([]
                         out-chan<)
                        ([v]
                         (locking *out* [:to-client v])

                         (send-transit! [:to-client v])
                         v)
                        ([a b]
                         ;(locking *out* (println "end!" a b))
                         )
                        )
                      ))
              :collect? true
              }



    }
  )




(defn steps-fn [& {:keys [initial-steps]}]
  {
    ::main-loop [:wait-for-commands initial-steps]

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




;; api for webserivce
(defn prepare-wf []
  ;; pass here the default path

  (let [*local (atom {})
        in-chan> (async/chan)
        out-chan< (async/chan)

        receive-fn (fn [payload]
                     (let [msg (read-transit-str payload)]
                       ; (locking *out*  (println "SERVER RECEIVE:\n" (pr-str msg) "\n"))
                       (go
                         (async/put! in-chan> msg)))
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
      :receive-fn receive-fn
      :close-fn close-fn
    }
    )
  )




;; some questions:

;; why to pass the required params to a wf constructor fn directly instead of map

;; syncing expand steps -
;; expand1 -> steps -> more steps

;; so expand1 is ready when all sub-sequent steps are processed? or when the first batch is ready


(let [selector (d/selector [:hello])
      v (atom {:hello "world"})
      ]

  (get-in @v [:current])
  ; selector
  ;(d/update-value v selector "woof")

  )


;; tester

(let [in (async/chan)
      out (async/chan)

      *local (atom {
                     :current {:path "Users/ndrw/m/woof/example.edn"}}) ;; {:current {:path "Users/ndrw/m/woof/example.edn"}}
      ]


  (let [end-chan (async/chan)

        ;; opts
        init-fn (fn [wf-chan xtor]

                  (go
                    (when-let [v (async/<! end-chan)]
                      (locking *out* (println "stopping wf"))
                      (wf/end! xtor)
                      )
                    )

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
                             )))

        opts {
               :before-process init-fn
               :op-handlers-map {
                                  :done (fn [data] (locking *out* (println "DONE!\n" (d/pretty data))))
                                  :error (fn [data] (locking *out* (println "ERROR!\n" (d/pretty data))))
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

                 :initial-steps {
                                  ;; these are sync by value
                                      ;::test-file [:write-file {
                                      ;                                :path "/Users/ndrw/m/woof/example.edn"
                                      ;                                :contents "Hello Woof!"
                                      ;                                }]
                                      ;::set-current-1 [:set-current ::test-file]

                                  ::init-read [:read-current ::init-path]

                                  ;; instand set
                                  ::init-path [:state! [(d/selector [:current]) {:path "/Users/ndrw/m/woof/example.edn"}]]


                                      }
                 }

        ]

  (wfc/wf-async-process! (wwf params) opts)


  ;; ::client [:client> "Yo"]

  ;; frontend sends new steps into workflow
  (go
    (async/<! (u/timeout 100))

    (async/put! in
                {
                  ;; set via expand - needs syncronization
                  ::init-path1 [:set-current {:path "/Users/ndrw/m/woof/example1.edn"}]

                  ::s [:v ::init-path1]

                  })

    ;(async/put! in { ::1 [:state [:current]] })

    ;(async/<! (u/timeout 100))

    ;(async/put! in {::2 [:state! [(d/selector [:current :path]) "/Users/ndrw/m/woof/example1.edn"]]})
    )


  ;; simulate closing ws
  (go
    (async/<! (u/timeout 1000))
    (async/put! end-chan :done)
    )

  )
)
