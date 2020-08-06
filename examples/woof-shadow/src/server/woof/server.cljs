(ns woof.server
  (:require                                                 ;["express" :as express]
    ["http" :as http]
    ["fs" :as fs]
    [clojure.core.async :as async]

    [woof.base :as base]

    [woof.utils :as u]
    [clojure.string :as string]

    [blog.core :as blog]
    ))


(enable-console-print!)


(defonce *STATE (atom {}))


(defn server-ctx [params]
  (let [*state (base/&state params)]
    {
     :start-server {:fn (fn [cfg]
                          ;(js/console.log ":start-server — ")
                          (let [handler (get cfg :handler (fn [req res]
                                                            (.end res "NO cfg.handler was passed into start-server")))


                                port (get cfg :port 3000)
                                server (http/createServer handler)]

                               (.listen server port
                                        (fn [err]
                                          (if err
                                              (do
                                                (js/console.error "server start failed")
                                                (js/console.error err))

                                              (js/console.info (str ":start-server — http server running on port:" port)))
                                          ))



                               (swap! *state assoc :shutdown (fn []
                                                               ;(js/console.log ":start-server — stopping http server")

                                                               (.close server
                                                                         (fn [err]
                                                                           (js/console.log ":shutdown — stopping http server"
                                                                                           err)
                                                                           ))
                                                               )
                                      )

                               ;;server
                               :started
                               ))
                    }
     }
    )
  )



(defn workflow [*STATE init-fns ctx-fns steps-fns opt-fns]
  ;; use base/wf!
  (let [wf-impl (base/parametrized-wf!
                  (base/combine-init-fns init-fns)
                  identity                                  ; wf-params-fn
                  identity                                  ; opt-params-fn
                  (base/combine-fns opt-fns :merge-results base/merge-opts-maps)
                  (base/combine-fns ctx-fns)
                  (base/combine-fns steps-fns))

        api-map {
                 :stop-and-notify (fn [msg]

                                    )
                 ;:some-api-method (fn [msg]
                 ;                   (prn "boo"))
                 }

        on-stop (fn []
                  ;(js/console.log "on-stop:")
                  ;(js/console.log (base/pretty! @*STATE))

                  ;; how to pass callback here?
                  (when-let [wf-ended-fn (:wf-ended-fn @*STATE)]
                            ;(js/console.log "on-stop: triggering :wf-ended-fn")
                            (wf-ended-fn)
                          )

                  )
        ]
    ;; for stateful wfs we need to provide start/end
    (base/stateful-wf *STATE wf-impl on-stop api-map)
    ))



(defn init-blog-wf []
  (let [*CHAN-STORAGE (atom {})
        CHAN-FACTORY (base/chan-factory *CHAN-STORAGE)
        EVT-LOOP (base/make-chan CHAN-FACTORY (base/rand-sid "server-loop"))

        ]

    (workflow *STATE
              [; blog-init-fn
               (base/build-init-state-fn *STATE)]

              [blog/common-ctx-fn
               blog/debug-ctx-fn
               server-ctx
               ]
              [
               (fn [params]
                 {
                  ;; prepare server config
                  ::_port-k     [:id :port]
                  ::port-v      [:id 3000]
                  ::_handler-k  [:id :handler]
                  ::url-to-open [:id "http://localhost:8081/blog/posts/1"]

                  ::#HANDLER#   [:id (fn [req res]
                                       ;; for now handle this as part of this wf
                                       (.end res (str (base/rand-sid)))
                                       )]

                  ;; start server
                  ::server-cfg  [:kv* [::_port-k ::port-v
                                       ::_handler-k ::#HANDLER#
                                       ]]


                  ::start!      [:start-server ::server-cfg]


                  ;; ::println [:prn-1 ::start!]

                  ;; open browser page, after server had been started
                  ;::url-to-open.. [:wait-others [::url-to-open
                  ;                               ::start!]]
                  ;::open-browser  [:open-browser ::url-to-open..]

                  }
                 )
               (fn [params]                                 ;; evt loop to keep wf running for
                 {
                  ::evt-loop [:infinite-expander EVT-LOOP]
                  })
               ]
              [(fn [params] {:op-handlers-map {
                                               :done  (fn [result]
                                                        (js/console.log "\nWORKFLOW RESULTS:\n"
                                                                        (base/pretty! (base/inline-results result)
                                                                        "\n"
                                                                        ))
                                                        result
                                                        )
                                               :error (fn [err] (prn err) err)}})
               (base/build-opt-state-fn *STATE)
               (base/build-opts-chan-factory-fn CHAN-FACTORY)
               ;; shutdown wf if running
               (base/build-opt-on-done (fn [_ _]
                                         (if-let [shutdown-fn (:shutdown @*STATE)]
                                                 (shutdown-fn)
                                                 (js/console.log "no :shutdown fn"))))]
              )
    )
  )




(def *blog-wf (atom {:start-wf! (fn [])
                     :stop-wf!  (fn [])
                     }))

;; start
(defn start-blog-wf []
  ((:start-wf! @*blog-wf)))

(defn stop-blog-wf []
  ((:stop-wf! @*blog-wf)))




(defn request-workflow [req res]
  (let [
        opts {
              :op-handlers-map {
                                :done  (fn [result]
                                         (println "REQUEST IS READY:")
                                         (.end res (base/pretty! result))
                                         result)

                                :error (fn [result]
                                         (.end res (pr-str result)))
                                }
              }
        ctx-map {
                 :print {:fn (fn [v]
                               (prn v)
                               v
                               )}
                 }

        steps-map {
                   (base/rand-sid) [:print "Hello Woof Shadow-CLJS"]
                   }

        wf (base/wf! :ctx ctx-map
                     :steps steps-map
                     :opts opts)]

    ;; run async
    (base/run-wf! wf)
    )
  )


(defn request-handler [req res]
  (let [response-str (str "azazaz: " (base/rand-sid))]
    (.end res response-str)))

; a place to hang onto the server so we can stop/start it
(defonce server-ref
  (volatile! nil))

;;
;;
;;

(defn main [& args]
  ;; todo: parse args some day
  (reset! *blog-wf (init-blog-wf))
  (start-blog-wf))

;;
;; code reload
;;

(defn start
  "Hook to start. Also used as a hook for hot code reload."
  []

  ;(js/console.log "woof.server/start")

  (reset! *blog-wf (init-blog-wf))
  (start-blog-wf)
  )


(defn stop
  "Hot code reload hook to shut down resources so hot code reload can work"
  [done]

  ; (print "woof.server/stop: .")

  (swap! (:state @*blog-wf) assoc
         :wf-ended-fn (fn []

                        (done)
                        ))
  (stop-blog-wf)
  )



;;
;; (js/console.log "__filename" js/__filename)

;;(js/console.log "\n\n\n\nAAAAAAAAAAAAAAA")