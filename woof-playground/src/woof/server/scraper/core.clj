(ns woof.server.scraper.core
  "woof scraper server"
  (:require
    [clojure.core.async :as async :refer [go go-loop]]
    [clojure.java.io :as io]
    [clojure.string :as str]

    [taoensso.timbre :as timbre :refer [log trace debug info warn error fatal report logf tracef debugf infof warnf errorf fatalf reportf spy get-env]]

    [compojure.core :as compojure]
    [compojure.route :as route]
    [compojure.handler :refer [site]]

    [org.httpkit.server :as httpkit]

    [ring.util.response :as response]
    [ring.middleware.cors :refer [wrap-cors]]

    [woof.base :as base]
    [woof.data :as d]
    [woof.wfs.evt-loop :as evt-loop]
    [woof.utils :as u]

    [woof.server.state :as state]
    [woof.server.transport :as tr]
    [woof.server.ws :as WS]
    ))


;; browser workflow backend

;; scraping server is needed for storing scraped data on the filesystem.
;; currently saving will be done via websockets

;;
;; in-mem scraping session
;;

(defn scraping-session-init [params]
  { ::scraping-session (atom {}) })

(defn &scraping-session [params]
  (base/& params ::scraping-session "provide ::scraping-session in init fn!"))



;;
;; ws communication
;;


(declare _scraping-msg-fn) ;; scraping ws backend communication protocol

(defn &broadcast-mult [params]
  (base/& params ::mult "no ::mult provided in params. Ensure that `ws-broadcast-init-fn` had been called in :init"))
(defn &broadcast-chan [params]
  (base/& params ::mult-chan "no ::mult-chan provided in params. Ensure that `ws-broadcast-init-fn` had been called in :init"))

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
  (let [broadcast-chan (&broadcast-chan params)
        chan-factory (base/&chan-factory params)

        *state (base/&state params)
        *scraping-session (&scraping-session params)

        ;; (fn [ws-msg])
        ws-handler (partial _scraping-msg-fn
                            chan-factory *state *scraping-session)
        ]
    {
     :ws-broadcast {:fn (fn [new-v]
                          (async/put! broadcast-chan new-v)
                          new-v)
                    }
     ;;
     :ws-msg       {
                    :fn       ws-handler
                    :expands? true
                    }


     ;; todo: remove this
     :test         {:fn (fn [v]
                          (prn v)
                          v)}
     }
    )

  )


; ws impl
(defn _scraping-msg-fn [chan-factory *state *scraping-session ws-msg]

  (let [{ws-id :ws-id
         msg   :msg} ws-msg

        out-chan (base/get-chan chan-factory ws-id)
        ]


    ;; todo: modify state
    (info "[WS]\t" ws-msg)

    (let [[t body] msg]
      (cond

        ;; client scraper wants to get current scraping session or start a new one
        (= :scraping-client/connect t)
        (let [{host :host
               url  :url} body]
          (let [session (state/get-scraping-session *scraping-session host)
                summary (get session :summary {})]

            (info "[WS] sending :summary\n" summary)

            ;; put just message with summary onto a channel
            (async/put! out-chan [:scraping/summary {:ws-id   ws-id
                                                     :summary summary}]))

          {}
          )

        ;; updates the data in scraping session
        (= :scraping/data t) (let [{host :host
                                    url :url
                                    data :data
                                    summary :summary
                                    } body
                                   ]


                               (swap! *scraping-session update-in [host]
                                      (fn [v]
                                        (let [old-data (get v :data [])
                                              old-summary (get v :summary {})
                                              processed-ids (into #{}  (keys old-summary))

                                              filtered-data (filter (fn [a]
                                                                      (not (get processed-ids (:id a))))
                                                                    data)
                                              ]

                                          ;; (info "GOT DATA " (pr-str filtered-data))

                                          (if (seq filtered-data)
                                            (assoc v
                                              :data (conj old-data {:url url
                                                                    :data filtered-data})

                                              :summary (merge old-summary summary)
                                              )
                                            (info "skipping updating scraping data")
                                            )
                                          )
                                        )
                                      )
                               {}
                               )

        (= :scraping-client/broadcast t)
        (do
          {
           ;; todo: how to send broadcast message
           (base/rand-sid)

           [:ws-broadcast [:broadcast (str (System/currentTimeMillis) "\thello from the broadcast service!")]] ; [:test (d/pretty @*scraping-session)]

           }
          )

        :else
        (do
          (warn "[WS]\t unknown message" (d/pretty ws-msg))
          {
           (base/rand-sid) [:test (str "uknown message" (d/pretty ws-msg))]
           }
          )

        )
      )
    )
  )

;;
;; server
;;

(declare server-ws-fn)

(defn &server [params]
  (base/& params ::server "no ::server provided in params. Ensure that `server-init-fn` had been called in :init"))

(defn server-init-fn
  "provides ::server with compojure routes for "
  [params]
  (let [PORT (get params :port 8081)
        ;; for now, use the global scrapping session atom
        *SCRAPING-SESSION (&scraping-session params)]
    {
     ;; define routes for scraping workflow
     ::server {
               :routes (wrap-cors
                         (compojure/routes
                           ;(compojure/GET "/" [] (response/resource-response "public/preview.html"))
                           (route/resources "/" {:root "public"})

                           (compojure/GET "/scraping-session" []
                             (d/pretty! @*SCRAPING-SESSION))

                           (compojure/GET "/clear-scraping-session" []
                             (reset! *SCRAPING-SESSION {})
                             (d/pretty! @*SCRAPING-SESSION))

                           (compojure/GET "/save-scraping-session" []
                             (spit "/Users/ndrw/m/woof/woof-playground/scraping-session.edn"
                                   (d/pretty! @*SCRAPING-SESSION))

                             (d/pretty! @*SCRAPING-SESSION)
                             )


                           ;; tests sending ws broadcast message
                           (compojure/GET "/test" []
                             (let [EVT-LOOP (evt-loop/&evt-loop params)
                                   msg [:broadcast (u/now)]]
                               (go
                                 (async/put! EVT-LOOP {(base/rand-sid)
                                                       [:ws-broadcast msg]
                                                       })
                                 )

                               (pr-str msg)
                               ))

                           ;; ws handler
                           (compojure/GET "/scraper-ws" [:as request]
                             (info "[SRV] /scraper-ws" request "params" params)
                             (server-ws-fn params request)
                             )
                           )
                         ;; todo: add other CORS domains
                         :access-control-allow-origin [#"http://localhost:9500"]
                         :access-control-allow-methods [:get :put :post :delete]
                         )
               :port PORT
               }
     }
    )
  )

(defn server-ctx-fn [params]
  (let [chan-factory (base/&chan-factory params)
        *state (base/&state params)]
    {
     ;; start server with the specified configuration
     :start-server {
                    :fn (fn [server]
                          (info "[SRV] :start-server")

                          (let [{routes :routes
                                 port  :port} server]
                            ;; port
                            (let [shutdown-server (httpkit/run-server (site routes) {:port port})]
                              (swap! *state assoc :shutdown shutdown-server))

                            ::started
                            )
                          )
                    }

     }
    )
  )

;; server impl
(defn server-ws-fn
  "ring ws handler for scraping workflow"
  [params request]

  (let [evt-loop (evt-loop/&evt-loop params)
        broadcast-mult (&broadcast-mult params)

        ;; re-routes ws msg to a workflow
        receive-msg-fn (fn [ws-id broadcast-chan msg]
                         (info "[WF] got msg \t" ws-id "\t" msg)

                         (go
                           (async/put! evt-loop
                                       {(base/rand-sid "msg-") [:ws-msg {:ws-id ws-id
                                                                         :msg   msg}]})))

        ;; prepares msg to be send to a ws
        send-msg-fn (fn [ws-id msg]
                      ;; handle case if msg is null - for initial data send
                      (when msg
                        {:ws-id ws-id
                         :msg msg}))


        ]
    (WS/broadcast-ws-request-fn params broadcast-mult
                                receive-msg-fn
                                send-msg-fn)
    )
  )


;; utils
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
(defn scraper-wf! [cfg & {:keys [on-stop] :or {on-stop (fn [stop-chan]
                                                         (info ::wf-stopped))}}]

  (info "[WF] initializing...\t" (System/currentTimeMillis))
  ;; build-... vs
  (let [

        *CHAN-STORAGE (atom {})
        CHAN-FACTORY (base/chan-factory *CHAN-STORAGE)

        EVT-LOOP (base/make-chan CHAN-FACTORY (base/rand-sid "server-loop"))

        STOP-CHAN (async/chan) ;; do not use chan factory - as it will be closed
        ;; _ (info "[STOP-CHAN]\t" STOP-CHAN)

        ;; fixme: this state is not actually used anywhere
        ;; here we'll have state hidden in this closure.
        ;; todo: 2-stage state update - first change local state, then merge it with global
        ;; problem, that we may lose data if the workflow will be reloaded
        *STATE (atom {
                      ;; old
                      :listings {}

                      :STOP-CHAN STOP-CHAN
                      })

        INIT [(fn [_]
                (info "[WF] starting...\t" (System/currentTimeMillis))
                cfg)                                 ;; pass configuration map into wf fn
              (evt-loop/build-evt-loop-init-fn EVT-LOOP)
              (base/build-init-chan-factory-fn CHAN-FACTORY)

              ;; internal state
              (base/build-init-state-fn *STATE)
              ;; scraping session state
              scraping-session-init

              ws-broadcast-init-fn
              server-init-fn          ;; needs to be last
              ]

        CTX [
             evt-loop/evt-loop-ctx-fn
             server-ctx-fn
             ws-broadcast-ctx
             ]

        STEPS [
               ;; although we can combine steps, but let's use single step for clarity
               (fn [params]                   ;; evt-loop-steps-fn + ws-steps-fn
                 {
                  ::evt-loop [:evt-loop (evt-loop/&evt-loop params)]
                  ::start!   [:start-server (&server params)]
                  })
               ]
        OPTS [
              (base/build-opt-state-fn *STATE)
              (base/build-opts-chan-factory-fn CHAN-FACTORY)
              (fn [params]
                {
                 ;; log errors
                 :op-handlers-map {
                                   :error (fn [err] (error err))
                                   ;:process (fn [result] (info ::process "\n" (d/pretty result)))
                                   :done    (fn [result]
                                              (let [*session (&scraping-session params)]
                                                (info "[WF].DONE\t[SCRAPING SESION]\t" @*session "\t" STOP-CHAN)

                                                ;; (info "	[WF].DONE	[WAIT TEST] start")
                                                ;; (Thread/sleep 1000)
                                                ;; (info "	[WF].DONE	[WAIT TEST] stop")

                                                )
                                              result
                                              )
                                   }
                 })


              (base/build-opt-on-done (fn [_ result]
                                        ;; (info "\t[WF].DONE\t[SRV] :shutdown-server")
                                        ;; force instant server shutdown
                                        (if-let [shutdown-fn (:shutdown @*STATE)]
                                          (shutdown-fn :timeout 0)
                                          (warn "can't shutdown server, as no :shutdown in state."))

                                        result
                                        ))

              (base/build-opt-on-done (fn [params result]
                                        (async/put! STOP-CHAN :after-process)

                                        result
                                        ))


              ]

        WF (base/wf! :init  INIT
                     :ctx   CTX
                     :steps STEPS
                     :opts  OPTS
                     )
        ]

    (base/stateful-wf
      *STATE WF
      ;; <?>: should workflow expose api?
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




;; try with simpler workflow



(info "\n\n[RELOAD] \t" (System/currentTimeMillis) "\n") ;; why this happens after :start-server

;(reset! state/*server-wf nil)


(when state/AUTO-RUN-WF?
  (base/auto-run-wf! state/*server-wf
                (fn [prev-state]
                  (info "[RELOAD] re-runing wf\n" prev-state)

                  (scraper-wf! state/ws-cfg)
                  )
                    ;; TODO: provide :on-stop
                     ))

;; ---------------------------------------------------------
