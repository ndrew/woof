(ns woof.server.fs
  "woof embedded file server"
  (:require
    [compojure.core :as compojure]
    [compojure.route :as route]
    [compojure.handler :refer [site]]

    [org.httpkit.server :as httpkit]

    [ring.util.response :as response]
    [ring.middleware.reload :as reload]


    [woof.server.transport :as tr]

    [clojure.core.async :as async :refer [go go-loop]]

    [woof.data :as d]

    [woof.utils :as u]

    ; logging
    [clojure.java.io :as io]

    [taoensso.timbre :as timbre :refer [info error]]
    [woof.base :as base]))

(defn- fs-init-fn [params]
  ; provide initial config here, ignore the params
  {
   :route (compojure/routes
            (compojure/GET "/" []
                           (response/resource-response "public/preview.html"))
            (route/resources "/" {:root "public"}))
   :port 8081

   }
  )


(defn- fs-ctx-fn [& {:keys [port state] :as params}]

  {
   ;; this will add new actions to a wf
   :fs-loop {
                  :fn       (fn [loop]
                              ;; (u/wiretap-chan in-chan (partial println "UI LOOP:"))
                              loop)

                  :infinite true
                  :expands? true
                  }

   :start-server {
                  :fn (fn [route]
                        ;; port
                        (httpkit/run-server (site route) {:port port})

                        ::started
                        )
                  }

   :stop-server {
                 :fn (fn [_]
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

   :test         {
                  :fn (fn [a]
                        (info a)
                        a)
                  }
   }
  )

(defn- fs-steps-fn [& {:keys [route loop]}]
  (info ::init-steps)
  {
   ::loop [:fs-loop loop]

   ::start! [:start-server route]

   ::hello [:test "hello"]

   }
  )


(defn fs-xtor-opts [params]
  {:before-process (fn [wf-chan xtor]
               ;
                     (swap! (:state params) assoc :xtor xtor)
                     :ok
                     )})


(defn fs-process-opts [& {:keys [cc-loop]}]
  {:op-handlers-map
   {
    :process (fn [result]
               (info ::fs-results "\n" (d/pretty result))

               )

    :error   (fn [err]
               ;(swap! *ui-state assoc-in [:status] :error)
               (error err)
               )

    :done    (fn [result]
               ;; this will be called only if the cc request
               ;; will be stopped
               )
    }}
  )



#_(defn start-cc-wf! []
  (base/run-wf! (get-in @STATE [:cc :wf]) identity))


#_(defn end-cc-wf! []
  (if-let [xtor (get-in @STATE [:cc :xtor])]
    (do
      ;; close channels?
      (base/end! xtor)
      )
    (error ::no-wf-running)
    ))



(defn fs-wf!
    "creates and starts the cc workflow"
    []

    (timbre/info ::fs-wf!)






    ;; assume that currently there is a single cc workflow, so take in chan from global state
    #_(let [cc-in-channel (get-chan :msg-in)]

      {
       :start-wf!        start-cc-wf!
       :end-wf!          end-cc-wf!

       ;; todo: how to handle separate websockets working with single wf?

       :response-handler (fn [request]
                           (if-not (:websocket? request)
                             (do
                               (warn ::no-ws-support "server running not in http-kit?")
                               {:status 200 :body "websocket connecting..."})

                             (httpkit/with-channel
                               request ch

                               (warn ::init-ws request)

                               (close-chan :msg-out)
                               (new-chan :msg-out)

                               ;; re-route out msg from wf onto a websocket
                               (go-loop []
                                 (when-let [v (async/<! (get-chan :msg-out))]
                                   (httpkit/send! ch (tr/write-transit-str v))
                                   (recur)))


                               (httpkit/on-receive ch (fn [msg]
                                                        (let [command (tr/read-transit-str msg)]
                                                          ;; todo: handle incorrect transit
                                                          (trace :woof.cc/got-ws-msg command)
                                                          ;; for now, just relay the message to cc workflow
                                                          (go
                                                            (async/put! cc-in-channel command)))))

                               (httpkit/on-close ch (fn [status]
                                                      (trace :woof.cc/ws-closed)

                                                      ;; will this work for second wf?
                                                      (close-chan :msg-out)))

                               ;; send initial message to client

                               (httpkit/send! ch (tr/write-transit-str {:available-commands #{:response :test}}))

                               )))
       }
      )

  (let [wf-params-fn identity
        opt-params-fn identity

        STATE (atom {})
        loop (async/chan)


        wf (base/parametrized-wf!
             (base/combine-init-fns [(fn[_]
                                       {
                                        :loop loop
                                        :state STATE
                                        }
                                       ) fs-init-fn])
             wf-params-fn
             opt-params-fn
             (base/combine-fns
               [(base/arg-fn fs-process-opts) fs-xtor-opts]
               :merge-results base/merge-opts-maps)
             (base/combine-fns [(base/arg-fn fs-ctx-fn)])
             (base/combine-fns [(base/arg-fn fs-steps-fn)])
             )]

    ;{
    ; :init-fn init-fn
    ; :wf-fn   wf-fn
    ; :opts-fn opts-fn
    ; }

    ;;@STATE
    {
     :wf wf
     :state STATE
     :start-wf! (fn []
             (base/run-wf! wf))

     :stop-server! (fn []
              (go
                (async/put! loop {(base/rand-sid) [:stop-server :nop]}))
              ;; how to actually c
              )
     }
    )

)




;;
;; do we need a separate server for preview — to avoid updating links
;;

;(httpkit/run-server (site preview) {:port 8081})
