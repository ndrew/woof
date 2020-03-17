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
    ))


;;
;; channel factory
;;
(defn chan-factory-init-fn_ [cf params]
  {
   ::cf cf
   })
(defn &chan-factory [params]
  (if-let [cf (get params ::cf)]
    cf
    (u/throw! "no ::cf provided in params. Ensure that chan-factory-init-fn had been called" )
    )
  )
(defn &chan [params chan-sid]
  (base/make-chan (&chan-factory params) chan-sid))
(defn chan-factory-opts [params]
  (let [close! (fn [result]
                 (base/close-chans! (&chan-factory params))
                 result)]
    {
     :op-handlers-map {
                       :done close!
                       :error close!
                       }
     }
    )
  )

;;
;; init loop
;;
(defn init-evt-loop_ [evt-loop-chan params]
  ;; this should prepare all stuff needed for ctx and steps
  {
   ;; keep the evt loop chan
   ::evt-loop-chan evt-loop-chan
   }
  )
(defn &evt-loop [params]
  (if-let [evt-loop (get params ::evt-loop-chan)]
    evt-loop
    (u/throw! "no ::evt-loop-chan provided in params. Ensure that init-evt-loop_ had been called" )
    )
  )
(defn evt-loop-ctx-fn [params]
  {
   :test     {:fn (fn [v]
                    (prn v)
                    v)}

   :evt-loop {
              :fn       (fn [in-chan] in-chan)
              :infinite true
              :expands? true
              }
   }
  )
(defn evt-loop-steps-fn [params]
  {
   ::hello [:test "yooooo!"]
   ::evt-loop [:evt-loop (::evt-loop-chan params)]
   }
  )


;;
;; store xtor
;;
(defn store-xtor-opts [params]
  {
   :before-process  (fn [wf-chan xtor]
                      ;
                      (swap! (:state params) assoc :xtor xtor)
                      :ok
                      )
   ;; convenience
   :op-handlers-map {
                     :process (fn [result]
                                ; (info ::fs-results "\n" (d/pretty result))

                                )

                     :error   (fn [err]
                                ;(swap! *ui-state assoc-in [:status] :error)
                                (error err)
                                )

                     :done    (fn [result]
                                ;; this will be called only if the cc request
                                ;; will be stopped
                                )
                     }
   })


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
            msg-out (base/make-chan (&chan-factory params) ws-id)
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
   :server {
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
(defn ws-ctx-fn [params]

  (let [cf (&chan-factory params)
        state (:state params)
        ]
    {
     :start-server {
                    :fn (fn [server]
                          (let [{route :route
                                 port :port} server]
                            ;; port
                            (httpkit/run-server (site route) {:port port})

                            ::started
                            )

                          )
                    }


     :stop-server {
                   :fn (fn [_]
                         ;; todo: do we need it here?
                         (if-let [xtor (get-in @state [:xtor])]
                           (do
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

     :ws-msg {
              :fn (fn [ws-msg]
                    (let [{ws-id :ws-id
                           msg :msg} ws-msg

                          out-chan (base/get-chan cf ws-id)
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
(defn ws-steps-fn [params]
  ;; get routes
  {
   ::start! [:start-server (:server params)]

   }
  )


;;
;; wf implementation
;;
(defn scraper-wf! []

  (let [*STATE (atom {})
        *CHAN-STORAGE (atom {})

        CHAN-FACTORY (base/chan-factory *CHAN-STORAGE)
        EVT-LOOP (base/make-chan CHAN-FACTORY (base/rand-sid "server-loop"))

        wf (base/parametrized-wf!
             (base/combine-init-fns [
                                     ws-init-fn
                                     (fn[_] { :state *STATE })
                                     (partial init-evt-loop_ EVT-LOOP)
                                     (partial chan-factory-init-fn_ CHAN-FACTORY)
                                     ])
             identity ; wf-params-fn
             identity ; opt-params-fn
             (base/combine-fns
               [
                store-xtor-opts
                chan-factory-opts
                ] :merge-results base/merge-opts-maps)
             (base/combine-fns
               [
                evt-loop-ctx-fn
                ws-ctx-fn
                ])
             (base/combine-fns
               [
                evt-loop-steps-fn
                ws-steps-fn
                ])
             )]

    {

     :wf wf
     :start-wf! (fn []
                  (base/run-wf! wf identity))
     :stop-wf! (fn []
                 (if-let [xtor (get-in @*STATE [:xtor])]
                   (do
                     ;; close channels?
                     (base/end! xtor)
                     ::stopped
                     )
                   (error ::no-wf-running)
                   ))
     ;; other api

     :state *STATE

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


;; tre