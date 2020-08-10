(ns woof.server.scraper.core
  "woof scraper server"
  (:require
    [clojure.core.async :as async :refer [go go-loop]]
    [clojure.string :as str]

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
    [woof.server.state :as state]

    [woof.server.ws :as WS]

    [clojure.java.io :as io]

    [woof.wfs.evt-loop :as evt-loop]
    ))


;; browser workflow backend

;; scraping server is needed for storing scraped data on the filesystem.
;; currently saving will be done via websockets



;;
;; ws communication
;;

;; state modifiers

(defn ids-msg [*state]
  [:ids (into #{} (keys (:listings @*state)))])

;; accessors

(defn &broadcast-mult [params]
  (if-let [mult (get params ::mult)]
    mult
    (u/throw! "no ::mult provided in params. Ensure that `ws-broadcast-init-fn` had been called in :init" ))
  )

(defn &broadcast-chan [params]
  (if-let [mult-chan (get params ::mult-chan)]
    mult-chan
    (u/throw! "no ::mult-chan provided in params. Ensure that `ws-broadcast-init-fn` had been called in :init" ))

  )


(defn ws-request-fn [params request]

  (let [evt-loop (evt-loop/&evt-loop params)

        broadcast-mult (&broadcast-mult params)

        ;; re-routes ws msg to a workflow
        receive-msg-fn (fn [ws-id broadcast-chan msg]
                         (info ::WS ws-id msg)
                         (go
                           (async/put! evt-loop
                                       {(base/rand-sid "msg-") [:ws-msg {:ws-id ws-id
                                                                         :msg   msg}]})
                           )

                         )

        ;; prepares msg to be send to a ws
        send-msg-fn (fn [ws-id msg]
                      ;; handle case if msg is null - for initial data send
                      (when msg
                        {:ws-id ws-id
                         :msg msg}
                        )
                      )


        ]
    (WS/broadcast-ws-request-fn params
                                  broadcast-mult
                                  receive-msg-fn
                                  send-msg-fn
                                  )

    )

  #_(httpkit/with-channel
    request ch

    (let [;; generate id for current ws session
          ws-id (base/rand-sid "ws-")
          msg-out (base/make-chan (base/&chan-factory params) ws-id)
          evt-loop (evt-loop/&evt-loop params)
          ;; *state (base/&state params)

          ;; do not send initially data
          initial-data nil ;(ids-msg *state)
          ]


      ;; why track these changes
      #_(swap! *state assoc-in [:sessions ws-id] {:ws-id ws-id
                                                :created (u/now)})


      ;; re-route out msg from wf onto a websocket
      (go-loop []
        (when-let [v (async/<! msg-out)]
          (httpkit/send! ch (tr/write-transit-str v))
          (recur)))

      (httpkit/on-receive ch (fn [msg]
                               (let [msg (tr/read-transit-str msg)]
                                 (go
                                   (async/put! evt-loop
                                               {(base/rand-sid "msg-") [:ws-msg {:ws-id ws-id
                                                                                 :msg   msg}]})
                                   )

                                 )))

      (httpkit/on-close ch (fn [status]
                             (info ::ws-closed)

                             ;; will this work for second wf?
                             (async/close! msg-out)
                             ))


      (if initial-data
        (httpkit/send! ch
                       (tr/write-transit-str {:ws-id ws-id
                                              :msg   initial-data
                                              }))
        )

      )

    ))


(defn ws-broadcast-init-fn [params]
  (let [CHAN-FACTORY (base/&chan-factory params)

        WS-MSG-CHAN (base/make-chan CHAN-FACTORY (base/rand-sid "scrape-ws-"))
        WS-MSG-MULT (async/mult WS-MSG-CHAN)]
    {
     ::mult WS-MSG-MULT
     ::mult-chan WS-MSG-CHAN
     }
    )
  )

(defn ws-broadcast-ctx [params]
  (let [broadcast-chan (&broadcast-chan params)]
    {
     :ws-broadcast {:fn (fn [new-v]
                          (async/put! broadcast-chan new-v)
                          new-v)
                    }
     }
    )

  )


(defn ws-init-fn [params]
  (let [PORT (get params :port 8081)
        ;; for now, use the global scrapping session atom
        *SCRAPING-SESSION state/*scraping-session

        ]
    {
     ;; define routes for scraping workflow
     ::server {
               :route (compojure/routes
                        ;(compojure/GET "/" [] (response/resource-response "public/preview.html"))
                        (route/resources "/" {:root "public"})

                        (compojure/GET "/scraping-session" []
                          (d/pretty! @*SCRAPING-SESSION))

                        (compojure/GET "/clear-scraping-session" []
                          (reset! *SCRAPING-SESSION {})
                          (d/pretty! @*SCRAPING-SESSION))

                        (compojure/GET "/test" []
                          (let [EVT-LOOP (evt-loop/&evt-loop params)
                                msg (u/now)]
                            (go
                              (async/put! EVT-LOOP {(base/rand-sid)
                                                    [:ws-broadcast msg]
                                                    })
                              )

                            (pr-str msg)
                            ))

                        (compojure/GET "/scraper-ws" [:as request]
                          ;; (info "/scraper-ws" request)
                          ;; (info "params" params)

                          (ws-request-fn params  request)
                          )
                        )
               :port PORT
               }
     }
    )
  )

(defn &server [params]
  (::server params))



(defn handle-ws-msg [chan-factory *state ws-msg]
  (let [{ws-id :ws-id
         msg   :msg} ws-msg

        out-chan (base/get-chan chan-factory ws-id)
        ]

    (info :wf/got-msg ws-msg)

    (let [[t body] msg]
      (cond
        (= :scraping/session t) (let [{host :host
                                       url :url} body
                                      ]
                                  (let [session (state/get-scraping-session host)]

                                    ;; (info ::session session)

                                    ;; put just message onto a channel
                                    (async/put! out-chan [:scraping/session
                                                          {
                                                           :ws-id ws-id
                                                           :summary (get session :summary {})
                                                           }
                                                          ]))

                                  {}
                                  )

        (= :scraping/data t) (let [{host :host
                                    url :url
                                    data :data
                                    summary :summary
                                    } body]
                               (swap! state/*scraping-session update-in [host]
                                          (fn [v]
                                            (let [old-data (get v :data [])
                                                  old-summary (get v :summary {})]

                                              (assoc v
                                                  :data (conj old-data {:url url
                                                                        :data data})

                                                  :summary (merge old-summary summary)
                                              ))
                                            )
                                          )
                               {}
                               )

        (= :listings t) (do
                          (swap! *state update-in [:listings] #(merge % body))

                          (spit "/Users/ndrw/m/woof/woof-playground/listings.edn"
                                (pr-str (:listings @*state)))

                          (async/put! out-chan {:ws-id ws-id
                                                :msg   (ids-msg *state)})
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


     ;;
     :ws-msg       {
                    :fn       (fn [ws-msg]
                                (handle-ws-msg chan-factory *state ws-msg)
                                )
                    :expands? true
                    }

     ;; wf message handling

     :test         {:fn (fn [v]
                          (prn v)
                          v)}

     ;:send-ids {
     ;          :fn (fn [server]

     #_(async/put! out-chan
                   [:ids (keys (:listings @*state))]
                   )

     ;                )
     ;          }

     }
    )
  )


(defn pretty-steps [steps]
  (let [re-curr-ns #":woof.server.scraper.core/"]
    (reduce (fn [a [k [step-id v]]]
              (str a
                   (str/replace (pr-str k) re-curr-ns "::")
                   "\t"
                   "[" (pr-str step-id) "\t" (pr-str v) "]"
                   "\n")
              ) "\n" steps )
    )
  )


;;
;; wf implementation
;;
(defn scraper-wf! [cfg & {:keys [on-stop] :or {on-stop (fn [stop-chan] (info ::wf-stopped))}}]
;; build-... vs
  (let [
        ;; here we'll have state hidden in this closure.
        *STATE (atom {
                      :listings {}
                      :sessions {}
                      })

        *CHAN-STORAGE (atom {})
        CHAN-FACTORY (base/chan-factory *CHAN-STORAGE)

        EVT-LOOP (base/make-chan CHAN-FACTORY (base/rand-sid "server-loop"))

        wf (base/wf! :init [(fn [_] cfg)
                            (evt-loop/build-evt-loop-init-fn EVT-LOOP)
                            (base/build-init-chan-factory-fn CHAN-FACTORY)
                            (base/build-init-state-fn *STATE)

                            ws-broadcast-init-fn
                            ws-init-fn ;; needs to be last
                            ]
                     :ctx [
                           evt-loop/evt-loop-ctx-fn
                           ws-ctx-fn
                           ws-broadcast-ctx
                           ]
                     :steps [
                             ;; although we can combine steps, but let's use single step for clarity
                             (fn [params] ;; evt-loop-steps-fn + ws-steps-fn
                               {
                                ::evt-loop [:evt-loop (evt-loop/&evt-loop params)]
                                ::start!   [:start-server (&server params)]
                                })
                             ]
                     :opts [
                            (base/build-opt-state-fn *STATE)
                            (base/build-opts-chan-factory-fn CHAN-FACTORY)
                            (fn [params]
                              {
                               ;; log errors
                               :op-handlers-map {
                                                 :error   (fn [err] (error err))
                                                 ;:process (fn [result] (info ::process "\n" (d/pretty result)))
                                                 ;:done    (fn [result])
                                                 }
                               })
                            (base/build-opt-on-done (fn [_ _]
                                                      (info ::shutdown-server)
                                                      ;; force instant server shutdown
                                                      ((:shutdown @*STATE) :timeout 0)))
                            ]

                     :wf-impl (base/capturing-workflow-fn
                                :context-map-fn (fn [ctx-map]
                                                  ;; (log-ctx ctx-map)
                                                  ctx-map)
                                :steps-fn (fn [steps]

                                            (info ::steps
                                                  (pretty-steps steps))

                                            steps)
                                :params (fn [params]
                                          ;;(log-init-params params)
                                          params
                                          )
                                )
                     )
        ]
    (base/stateful-wf
      *STATE wf
      :on-stop on-stop
      :api {
            :send-msg!  (fn [msg]
                          (go
                            (async/put! EVT-LOOP {(base/rand-sid) [:test msg]})
                            )
                          )
            }
      )

    )
  )


;; ----------------- reloadable wf here --------------------



;(reset! state/*scraping-session {})

(when state/AUTO-RUN-WF?
  (base/auto-run-wf! state/*server-wf #(scraper-wf! state/ws-cfg)))

;; ---------------------------------------------------------
