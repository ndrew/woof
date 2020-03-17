(ns woof.server.scraper
  "woof scraper server"
  (:require
    [clojure.core.async :as async :refer [go go-loop]]

    [taoensso.timbre :as timbre
     :refer [log  trace  debug  info  warn  error  fatal  report
             logf tracef debugf infof warnf errorf fatalf reportf
             spy get-env]]


    [compojure.core :as compojure]
    [compojure.route :as route]
    [compojure.handler :refer [site]]

    [woof.base :as base]
    [woof.data :as d]
    [woof.utils :as u]

    [org.httpkit.server :as httpkit]

    [woof.server.transport :as tr]


    [woof.common.core :as common]
    ))

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
     ;; keep the evt loop chan
     ::evt-loop-chan evt-loop-chan
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

(defn ws-request-fn [params request]
  ;; per each request
  (if-not (:websocket? request)
    (do
      (warn ::no-ws-support "server running not in http-kit?")
      {:status 200 :body "websocket connecting..."})

    (httpkit/with-channel
      request ch

      (let [ws-id (base/rand-sid "ws-")
            msg-out (base/make-chan (common/&chan-factory params) ws-id)
            evt-loop (&evt-loop params)
            ]

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
                                                                                   :msg msg}]})
                                     )

                                   )))

        (httpkit/on-close ch (fn [status]
                               (trace ::ws-closed)

                               ;; will this work for second wf?
                               (async/close! msg-out)
                               ))

        ;; send initial message to client

        (httpkit/send! ch (tr/write-transit-str {:ws-id ws-id
                                                 :msg :hello}))


        )

      )))

(defn ws-init-fn [params]
  ; provide initial config here, ignore the params
  {
   ::server {
            :route (compojure/routes
                     ;(compojure/GET "/" [] (response/resource-response "public/preview.html"))
                     ;(route/resources "/" {:root "public"})
                     (compojure/GET "/ws" [:as req]
                       (ws-request-fn params req)
                       )
                     )
            :port 8081
            }
   }
  )
(defn &server [params]
  (::server params))

(defn ws-ctx-fn [params]

  (let [chan-factory (common/&chan-factory params)
        *state (common/&state params)]

    {

     ;; start server with the specified configuration
     :start-server {
                    :fn (fn [server]
                          (let [{route :route
                                 port  :port} server]
                            ;; port
                            (let [shutdown-server (httpkit/run-server (site route) {:port port})]
                              (swap! *state assoc :shutdown shutdown-server))

                            ::started
                            )

                          )
                    }

     ;; step handler for stopping the wf from the wf
     ;; TODO: is this needed???
     :stop-server  {
                    :fn (fn [_]
                          (if-let [xtor (common/state-get-xtor *state)]
                            (do
                              ((:shutdown @*state))

                              ;; close channels?
                              (base/end! xtor)
                              ::stopped
                              )
                            (do
                              (error ::no-wf-running)
                              ::no-wf-running
                              )
                            )
                          )
                    }

     ;; wf message handling

     :test         {:fn (fn [v]
                          (prn v)
                          v)}

     :ws-msg       {
                    :fn       (fn [ws-msg]
                                (let [{ws-id :ws-id
                                       msg   :msg} ws-msg

                                      out-chan (base/get-chan chan-factory ws-id)
                                      ]

                                  (async/put! out-chan ws-msg)

                                  {
                                   (base/rand-sid) [:test (d/pretty ws-msg)]
                                   }
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
(defn scraper-wf! []
;; build-... vs
  (let [; wf dependencies
        *STATE (atom {})
        *CHAN-STORAGE (atom {})

        CHAN-FACTORY (base/chan-factory *CHAN-STORAGE)
        EVT-LOOP (base/make-chan CHAN-FACTORY (base/rand-sid "server-loop"))


        init-fns [
                  ws-init-fn
                  (build-init-evt-loop-fn EVT-LOOP)
                  (common/build-init-chan-factory-fn CHAN-FACTORY)
                  (common/build-init-state-fn *STATE)
                  ]
        opt-fns [
                 (common/build-opt-state-fn *STATE)
                 (common/build-chan-factory-opts CHAN-FACTORY)
                 error-handling-opts
                 (common/build-opt-on-done (fn [_]
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
        wf (base/parametrized-wf!
             (base/combine-init-fns init-fns)
             identity                                       ; wf-params-fn
             identity                                       ; opt-params-fn
             (base/combine-fns opt-fns :merge-results base/merge-opts-maps)
             (base/combine-fns ctx-fns)
             (base/combine-fns steps-fn))]

    (common/stateful-wf
      *STATE wf
      (fn []
        (info ::wf-stopped))
      {
       :send-msg! (fn [msg]
                    (go
                      (async/put! EVT-LOOP {(base/rand-sid) [:test msg]})
                      )
                    )

       :stop-server! (fn []
                       (async/put! EVT-LOOP {(base/rand-sid) [:stop-server :nop]}))
       }
      )

    )
  )


;; tre