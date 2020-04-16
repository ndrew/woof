(ns woof.server
      (:require ;["express" :as express]
                ["http" :as http]
                [clojure.core.async :as async]

                [woof.base :as woof]

                [woof.utils :as u]
                [clojure.string :as string]
                ))



(defn request-workflow [req res]
  (let [
        opts {
              :op-handlers-map {
                                :done  (fn [result]
                                         (println "REQUEST IS READY:")
                                         (.end res (woof/pretty! result))
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
                   (woof/rand-sid)  [:print "Hello Woof Shadow-CLJS"]
                   }

        wf (woof/wf! :ctx ctx-map
                     :steps steps-map
                     :opts opts)]

    ;; run async
    (woof/run-wf! wf)
    )
  )


(defn request-handler [req res]
      (let [response-str (str "azazaz: " (woof/rand-sid) )]
           (.end res response-str)))


; a place to hang onto the server so we can stop/start it
(defonce server-ref
         (volatile! nil))

(defn main [& args]
      (js/console.log "starting server")
      (let [server (http/createServer
                     ;#(request-handler %1 %2)
                     #(request-workflow %1 %2)
                     )]

           (.listen server 3000
                    (fn [err]
                        (if err
                          (js/console.error "server start failed")
                          (js/console.info "http server running"))
                        ))

           (vreset! server-ref server)))

(defn start
      "Hook to start. Also used as a hook for hot code reload."
      []
      (js/console.warn "start called")
      (main))

(defn stop
      "Hot code reload hook to shut down resources so hot code reload can work"
      [done]

      (js/console.warn "stop called" @server-ref)
      (when-some [srv @server-ref]
                 (.close srv
                         (fn [err]
                             (js/console.log "stop completed" err)
                             (done)
                             ))
                 ))

(js/console.log "__filename" js/__filename)
