(ns woof.server.cc
  "woof command center"
  (:require
    [clojure.core.async :as async :refer [go go-loop]]

    [taoensso.timbre :as timbre
     :refer [log  trace  debug  info  warn  error  fatal  report
             logf tracef debugf infof warnf errorf fatalf reportf
             spy get-env]]

    [woof.data :as d]
    [woof.base :as base]

    [org.httpkit.server :as httpkit]
    [woof.server.transport :as tr]

    [woof.server.fs :as fs-server]
    ))


;; todo: how to handle session

;; store CC stuff as global atom
(defn initial-state []
  {:channels {}
   :cc {:wf nil
        :xtor nil}
   })

(defonce STATE (atom (initial-state)))


;; helpers for accessing state
;; TODO: migrate to channel factory

(defn get-chan [id]
  (get-in @STATE [:channels id]))


(defn new-chan [id]
  (let [c (async/chan)]
    (swap! STATE  update-in [:channels] assoc id c)
    c))


(defn close-chan [id]
  (when-let [c (get-in @STATE [:channels id])]
    (async/close! c)
    (swap! STATE update-in [:channels] dissoc id)))


;; CC wf

;; TODO: migrate to new way of launching workflows -
(defn init-and-run-cc-wf!
  [& {:keys [init-fn
             opts-fn ctx-fn steps-fn
             run-fn]
      :or   {init-fn  (fn [] {})
             ctx-fn   (fn [params] {}) steps-fn (fn [params] {}) opts-fn  (fn [params] {})
             run-fn   (fn [])}}]
  (let [wf-params-fn identity
        opt-params-fn identity

        wf (base/parametrized-wf!
             init-fn
             wf-params-fn
             opt-params-fn
             opts-fn
             ctx-fn
             steps-fn)]

    (swap! STATE update-in [:cc]
           merge {
            :wf wf
            :xtor nil
            })

    (run-fn)))


(defn- cc-loop-init-fn [params]
  (info ::init-loop params)
  {
   ;; provide a channel for event loop chan
   :msg-in (new-chan :msg-in)

   ;; provide an function to return the channel for current session
   :out-fn (fn [v]
             (async/put! (get-chan :msg-out) v))

   })


(defn- cc-loop-ctx-fn [& {:keys [out-fn] :as params}]
  (info ::init-ctx params)

  {
   ;; this will add new actions to a wf
   :request-loop {
                  :fn       (fn [in-chan]
                              ;; (u/wiretap-chan in-chan (partial println "UI LOOP:"))
                              (info ::request-event-loop ::started)

                              in-chan)

                  :infinite true
                  :expands? true
                  }

   :response     {
                  ;; pass response as a function
                  :fn out-fn
                  }

   :test         {
                  :fn (fn [a]
                        (info a)
                        a)
                  }

   ;;

   :run-wf {
            :fn (fn [params]
                  (info :run-wf params)

                  (let [WF (fs-server/fs-wf!)]
                    ((:start-wf! WF))
                    )

                  )
            }

   }
  )

(defn- cc-loop-steps-fn [& {:keys [msg-in msg-out]}]
  (info ::init-steps)
  {
   ::cc [:request-loop msg-in]

   ::hello [:test "hello"]

   }
  )


(defn cc-xtor-opts [params]
  {:before-process (fn [wf-chan xtor]
                     (swap! STATE update-in [:cc]
                            merge {:xtor  xtor})
                     :ok
                     )})


(defn cc-process-opts [& {:keys [cc-loop]}]
  {:op-handlers-map
   {
    :process (fn [result]
               (info ::results "\n" (d/pretty result)))

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



(defn start-cc-wf! []
  (base/run-wf! (get-in @STATE [:cc :wf]) identity))


(defn end-cc-wf! []
  (if-let [xtor (get-in @STATE [:cc :xtor])]
    (do
      ;; close channels?
      (base/end! xtor)
      )
    (error ::no-wf-running)
    ))




(defn cc-wf!
  "creates and starts the cc workflow"
  []

  (info ::cc-wf!)

  (init-and-run-cc-wf!
    :init-fn (base/combine-init-fns [cc-loop-init-fn])
    :ctx-fn (base/combine-fns [(base/arg-fn cc-loop-ctx-fn)])
    :steps-fn (base/combine-fns [(base/arg-fn cc-loop-steps-fn)])
    ; args
    :opts-fn (base/combine-fns
               [(base/arg-fn cc-process-opts)
                cc-xtor-opts]
               :merge-results base/merge-opts-maps)

    :run-fn (fn []
              (base/run-wf! (get-in @STATE [:cc :wf]) identity)))


  ;; assume that currently there is a single cc workflow, so take in chan from global state
  (let [cc-in-channel (get-chan :msg-in)]

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

                                                        (info :woof.cc/got-ws-msg command)
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
  )


