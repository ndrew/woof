(ns woof.server.scraper.core
  "woof scraper server"
  (:require
    [clojure.core.async :as async :refer [go go-loop]]

    [taoensso.timbre :as timbre
     :refer [log trace debug info warn error fatal report
             logf tracef debugf infof warnf errorf fatalf reportf
             spy get-env]]


    [compojure.core :as compojure]
    [compojure.route :as route]
    [compojure.handler :refer [site]]

    [ring.util.response :as response]

    [clojure.core.async :as async :refer [go go-loop]]


    [org.httpkit.server :as httpkit]

    [woof.base :as base]
    [woof.data :as d]
    [woof.utils :as u]
    [woof.server.transport :as tr]
    [clojure.java.io :as io]))


;; browser workflow backend

;; scraping server is needed for storing scraped data on the filesystem.
;; currently saving will be done via websockets


(defn error-handling-opts [params]
  {
   ;; log errors
   :op-handlers-map {
                     :error   (fn [err] (error err))
                     ;:process (fn [result] (info ::process "\n" (d/pretty result)))
                     ;:done    (fn [result])
                     }
   })

;;
;; init loop:
;;
(defn build-init-evt-loop-fn [evt-loop-chan]
  (fn [_]
    {
     ::evt-loop-chan evt-loop-chan ;; keep the evt loop chan
     }))

(defn &evt-loop [params]
  (if-let [evt-loop (get params ::evt-loop-chan)]
    evt-loop
    (u/throw! "no ::evt-loop-chan provided in params. Ensure that init-evt-loop_ had been called" )
    )
  )


(defn evt-loop-ctx-fn [params]
  {
   :evt-loop {
              :fn       (fn [in-chan] in-chan)
              :infinite true
              :expands? true
              }
   }
  )



;;
;; ws
;;

(defn ids-msg [*state]
  [:ids (into #{} (keys (:listings @*state)))])


(defn ws-request-fn [params request]

  ;; fixme: figwheel doesn't pass a http kit request here, so no ws

  (httpkit/with-channel
    request ch

    (let [ws-id (base/rand-sid "ws-")
          msg-out (base/make-chan (base/&chan-factory params) ws-id)
          evt-loop (&evt-loop params)
          *state (base/&state params)
          ]

      ;;(info request)
      ;;(info ch)



      ;; re-route out msg from wf onto a websocket
      (go-loop []
        (when-let [v (async/<! msg-out)]
          (httpkit/send! ch (tr/write-transit-str v))
          (recur)))

      ;; which ws impl figwheel is using
      (httpkit/on-receive ch (fn [msg]
                               (let [msg (tr/read-transit-str msg)]
                                 (go
                                   (async/put! evt-loop
                                               {(base/rand-sid "msg-") [:ws-msg {:ws-id ws-id
                                                                                 :msg   msg}]})
                                   )

                                 )))

      (httpkit/on-close ch (fn [status]
                             (trace ::ws-closed)

                             ;; will this work for second wf?
                             (async/close! msg-out)
                             ))


      ;; send stored ids as initial message to client
      (httpkit/send! ch (tr/write-transit-str {:ws-id ws-id
                                               :msg   (ids-msg *state)
                                               }))

      )

    ))

(defn ws-init-fn [params]
  ; provide initial config here, ignore the params
  {
   ::server {
            :route (compojure/routes
                     ;(compojure/GET "/" [] (response/resource-response "public/preview.html"))
                     (route/resources "/" {:root "public"})
                     ;;
                     (compojure/GET "/scraper-ws" [:as request]
                       ;; (info "/scraper-ws" request)
                       ;; (info "params" params)

                       (ws-request-fn params  request)
                       )
                     )
            :port (get params :port 8081)
            }
   }
  )

(defn &server [params]
  (::server params))

(defn ws-ctx-fn [params]
  (let [chan-factory (base/&chan-factory params)
        *state (base/&state params)]
    {
     ;; start server with the specified configuration
     :start-server {
                    :fn (fn [server]
                          (info "[SRV] :start-server")

                          (let [{route :route
                                 port  :port} server]
                            ;; port
                            (let [shutdown-server (httpkit/run-server (site route) {:port port})]
                              (swap! *state assoc :shutdown shutdown-server))

                            ::started
                            )
                          )
                    }

     ;; wf message handling

     :test         {:fn (fn [v]
                          (prn v)
                          v)}

     :send-ids {
                :fn (fn [server]

                      #_(async/put! out-chan
                                  [:ids (keys (:listings @*state))]
                                  )

                      )
                }

     :ws-msg       {
                    :fn       (fn [ws-msg]
                                (let [{ws-id :ws-id
                                       msg   :msg} ws-msg

                                      out-chan (base/get-chan chan-factory ws-id)
                                      ]


                                  (let [[t body] msg]
                                    (cond
                                      (= :listings t) (do
                                                        (swap! *state update-in [:listings] #(merge % body))

                                                        (spit "/Users/ndrw/m/woof/woof-playground/listings.edn"
                                                              (pr-str (:listings @*state)))

                                                        (async/put! out-chan {:ws-id ws-id
                                                                              :msg (ids-msg *state)})
                                                        {
                                                         (base/rand-sid) [:test (d/pretty (:listings @*state))]
                                                         }
                                                        )

                                      :else
                                      {
                                       (base/rand-sid) [:test (str "uknown message" (d/pretty ws-msg))]
                                       }
                                      )

                                    )



                                  )
                                )
                    :expands? true
                    }

     }
    )
  )


;;
;; wf implementation
;;
(defn scraper-wf! [cfg & {:keys [on-stop] :or {on-stop (fn [stop-chan] (info ::wf-stopped))}}]
;; build-... vs
  (let [; wf dependencies
        *STATE (atom {
                      :listings {}
                      })
        *CHAN-STORAGE (atom {})

        CHAN-FACTORY (base/chan-factory *CHAN-STORAGE)
        EVT-LOOP (base/make-chan CHAN-FACTORY (base/rand-sid "server-loop"))


        ;; ugly way to pass params to into api-map
        *params (atom {})

        init-fns [(fn [_] cfg)
                  (build-init-evt-loop-fn EVT-LOOP)
                  (base/build-init-chan-factory-fn CHAN-FACTORY)
                  (base/build-init-state-fn *STATE)

                  (fn [params]
                    ;; (info "peek params" params)
                    (reset! *params params)
                    {})
                  ws-init-fn

                  ]

        opt-fns [
                 (base/build-opt-state-fn *STATE)
                 (base/build-opts-chan-factory-fn CHAN-FACTORY)
                 error-handling-opts
                 (base/build-opt-on-done (fn [_ _]
                                             (info ::shutdown-server)
                                             ((:shutdown @*STATE))
                                             ))
                 ]
        ctx-fns [
                 evt-loop-ctx-fn
                 ws-ctx-fn
                 ]
        steps-fn [
                  ;; although we can combine steps, but let's use single step for clarity
                  (fn [params] ;; evt-loop-steps-fn + ws-steps-fn
                      {
                       ::evt-loop [:evt-loop (&evt-loop params)]
                       ::start! [:start-server (&server params)]
                       })
                  ]
        ;; todo: migrate from base/parametrized-wf!
        wf (base/parametrized-wf!
             (base/combine-init-fns init-fns)
             identity                                       ; wf-params-fn
             identity                                       ; opt-params-fn
             (base/combine-fns opt-fns :merge-results base/merge-opts-maps)
             (base/combine-fns ctx-fns)
             (base/combine-fns steps-fn))



        ]

    (base/stateful-wf
      *STATE wf
      :on-stop on-stop
      ;; todo: what is nicer way to pass params to api?
      :api {
            ;; handle ws-request
            :handle-ws  (fn [request]
                          (ws-request-fn
                            @*params
                            request)
                          )

            :handle-get (fn []
                          "Test BBBB"
                          )


            :send-msg!  (fn [msg]
                          (go
                            (async/put! EVT-LOOP {(base/rand-sid) [:test msg]})
                            )
                          )
            }
      )

    )
  )

