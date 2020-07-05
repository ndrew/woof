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

    [clojure.java.io :as io]
    ))


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
  (httpkit/with-channel
    request ch

    (let [ws-id (base/rand-sid "ws-")
          msg-out (base/make-chan (base/&chan-factory params) ws-id)
          evt-loop (&evt-loop params)
          *state (base/&state params)

          initial-data (ids-msg *state)
          ]

      (state/new-scraping-session ws-id)
      (swap! *state assoc-in [:sessions ws-id] {:ws-id ws-id
                                                :created (u/now)})

      ;(info (d/pretty! request))
      ;;(info (d/pretty! @*state))

      ;; new-scraping-session

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
                             (trace ::ws-closed)

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

(defn ws-init-fn [params]
  {
   ::server {
            :route (compojure/routes
                     ;(compojure/GET "/" [] (response/resource-response "public/preview.html"))
                     (route/resources "/" {:root "public"})
                     ;;
                     (compojure/GET "/scraping-session" []
                       (d/pretty! @state/*scraping-session))

                     (compojure/GET "/test" []

                       (let [EVT-LOOP (&evt-loop params)
                             msg (u/now)]
                         (go
                           (async/put! EVT-LOOP {(base/rand-sid) [:test msg]})
                           )

                         (pr-str msg)
                         ))

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


     ;;
     :ws-msg       {
                    :fn       (fn [ws-msg]
                                (let [{ws-id :ws-id
                                       msg   :msg} ws-msg

                                      out-chan (base/get-chan chan-factory ws-id)
                                      ]

                                  (let [[t body] msg]
                                    (cond
                                      (= :scraping/session t) (do
                                                                (state/new-scraping-session body)

                                                                {}
                                                                )

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
        ;; state
        *STATE (atom {
                      :listings {}
                      :sessions {}
                      })

        *CHAN-STORAGE (atom {})
        CHAN-FACTORY (base/chan-factory *CHAN-STORAGE)

        EVT-LOOP (base/make-chan CHAN-FACTORY (base/rand-sid "server-loop"))

        wf (base/wf! :init [(fn [_] cfg)
                            (build-init-evt-loop-fn EVT-LOOP)
                            (base/build-init-chan-factory-fn CHAN-FACTORY)
                            (base/build-init-state-fn *STATE)

                            ws-init-fn ;; needs to be last
                            ]
                     :ctx [
                           evt-loop-ctx-fn
                           ws-ctx-fn
                           ]
                     :steps [
                             ;; although we can combine steps, but let's use single step for clarity
                             (fn [params] ;; evt-loop-steps-fn + ws-steps-fn
                               {
                                ::evt-loop [:evt-loop (&evt-loop params)]
                                ::start!   [:start-server (&server params)]
                                })
                             ]
                     :opts [
                            (base/build-opt-state-fn *STATE)
                            (base/build-opts-chan-factory-fn CHAN-FACTORY)
                            error-handling-opts
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



(when state/AUTO-RUN-WF?
  (base/auto-run-wf! state/*server-wf #(scraper-wf! state/ws-cfg)))

;; ---------------------------------------------------------
