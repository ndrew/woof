(ns blog.preview
  "example of the the dynamic preview"
  (:require

    [compojure.core :as compojure]
    [compojure.route :as route]
    [compojure.handler :refer [site]]

    [org.httpkit.server :as httpkit]

    [ring.util.response :as response]
    [ring.middleware.reload :as reload]

    [woof.server.transport :as tr]

    [clojure.core.async :as async :refer [go go-loop]]

    [clojure.data :as cd]

    [woof.base :as base]
    [woof.data :as d]
    [woof.cc :as cc]
    [woof.utils :as u]

    ; [woof.wf :as wf]
    ; [woof.wf-data :as wdata]
    ; [woof.test-data :as test-data]

    ; [woof.server.utils :refer [read-transit-str write-transit-str]]
    ; [woof.example.files :as files-wf]

    [woof.server.log :refer [init-logging!]]


    ; logging
    [taoensso.timbre :as timbre :refer [info error]]


    ;; ws

    [org.httpkit.server :as httpkit]

    [hiccup.core :as hiccup]

    [ring.util.response :as response]
    [ring.middleware.reload :as reload]

    [compojure.core :as compojure]


    ;;
    [woof.wf :as wf])
  )

;; todo:

;; web-server wf
;; routes
;; start/stop

;; hiccup wf

;; file system wf

;; state wf


;; use cases

; 1) template editor
;   web-server wf
;;


(defonce *DATA (atom "Hello Woof!"))



(defn pretty-diff [expected actual]
  (let [[things-only-in-a things-only-in-b things-in-both] (cd/diff expected actual)]
    (apply str
           (concat
             (map #(str "+" (pr-str %) "\n") things-only-in-a)
             (map #(str "-" (pr-str %) "\n") things-only-in-b)
             (map #(str (pr-str %) "\n") things-in-both)
             )
           )
    )
  )



;; for testing request
(defn fake-request [routes uri method & params]
  (let [localhost "127.0.0.1"]
    (routes {:server-port    80
             :server-name    localhost
             :remote-addr    localhost
             :uri            uri
             :scheme         :http
             :headers        (or params {})
             :request-method method})))


(defn map-ctx
  ":kv [[k v]]
   :map [kv..]"
  []
  {
   ;; build map by combining kv
   :kv  {
         :fn (fn [[k v]]
               {k v})
         }

   :map {
         :fn       (fn [maps]
                     (apply merge maps))
         :collect? true
         }

   }
  )
(defn edn-ctx
  ":EDN - returns data that had been passed to it"
  []
  {
   ;; just return the edn data
   :EDN  {
          :fn identity
          }
   }
  )



(defn web-server-wf []
  (let [wf-params-fn identity
        opt-params-fn identity

        ;; chans
        loop (async/chan)
        reload-chan (async/chan)
        reload-mult (async/mult reload-chan)

        ;; state
        *WS-STATE (atom {
                         ;; save wf xtor for stopping wf
                         :xtor nil


                         ;; store channels, so they can be closed
                         :loop loop
                         :reload-chan reload-chan
                         :reload-mult reload-mult
                         })

        ;;

        keep-xtor-ref (fn [params]
                        {
                         :before-process  (fn [wf-chan xtor]
                                            (swap! *WS-STATE assoc :xtor xtor)

                                            :ok)

                         :after-process (fn [exec-chann]

                                          ; (.println *err* (str "\nAFTER PROCESS:\n"))

                                          exec-chann
                                          )

                         :op-handlers-map {

                                           :error (fn [result]
                                                    (.println *err* (str "\n" (d/pretty result) "\n"))
                                                    )
                                           :done  (fn [result]

                                                    (prn (fake-request (::preview-routes result) "/" :get))

                                                    (.println *err* (str "\n" (d/pretty result) "\n"))
                                                    )
                                           }

                         })

        ;; API
        ;; start/stop
        ;; single server?
        ;; options?
        ;; routes
        ;; compojure or ring
        ;; https://practicalli.github.io/clojure-webapps/images/clojure-ring-adaptor-middleware-route--handler-overview.png

        ;; Compojure route definitions are just functions which accept request maps and return response maps:

        ; (myapp {:uri "/" :request-method :post})
        ;; (make-route method path handler) - Returns a function that will only call the handler if the method and path match the request.
        ;; The routes function join multiple routes together.

        loop-ctx-fn (fn [params]
                      {:loop {
                              :fn       (fn [loop-chan]
                                          ;; (u/wiretap-chan in-chan (partial println "UI LOOP:"))
                                          loop-chan)

                              :infinite true
                              :expands? true
                              }
                       }
                      )

        common-ctx-fn (fn [params]
                        (merge
                          (edn-ctx)
                          (map-ctx)
                          ))

        hiccup-ctx-fn (fn [params]
                        {
                         :hiccup->html    {
                                           :fn (fn [data]
                                                 (hiccup/html data))
                                           }
                         }
                        )

        routes-ctx-fn (fn [params]
                        {

                         ;; serve static
                         :static-route    {:fn (fn [[path root]]
                                                 (route/resources path {:root root})
                                                 )}

                         ;; for now handle only
                         :ws-handler      {:fn (fn [reload-mult]
                                                 (fn [request]
                                                   (if-not (:websocket? request)
                                                     (do
                                                       ;(warn ::no-ws-support "server running not in http-kit?")
                                                       {:status 200 :body "websocket connecting..."})

                                                     (let [r-ch (async/chan)]

                                                       ;(prn reload-mult)

                                                       (async/tap reload-mult r-ch)
                                                       ;; reload-mult



                                                       (httpkit/with-channel
                                                         request ch

                                                         ;; how to handle this for multiple ws
                                                         (go []
                                                             (when-let [v (async/<! r-ch)]
                                                               (prn "got reload!")
                                                               (httpkit/send! ch v)
                                                               ;(recur)
                                                               ))


                                                         (httpkit/on-receive ch (fn [msg]

                                                                                  (prn "msg")

                                                                                  #_(let [command (tr/read-transit-str msg)]
                                                                                      ;; todo: handle incorrect transit

                                                                                      (info :woof.cc/got-ws-msg command)
                                                                                      ;; for now, just relay the message to cc workflow
                                                                                      (go
                                                                                        (async/put! cc-in-channel command))
                                                                                      )
                                                                                  ))

                                                         (httpkit/on-close ch (fn [status]
                                                                                (async/untap reload-mult r-ch)
                                                                                ;(trace :woof.cc/ws-closed)

                                                                                ;; will this work for second wf?
                                                                                ;(close-chan :msg-out)

                                                                                ))

                                                         ;; send initial message to client
                                                         (httpkit/send! ch "init-ws")
                                                         )
                                                       )
                                                     ))
                                                 )}


                         ;; wraps content to a full html page with reload ws
                         :preview-page      {:fn (fn [content]
                                                 [:html

                                                  [:script {:src "/woof/preview-reload.js"}]

                                                  [:body
                                                   [:pre (str (u/rand-sid))]

                                                   content
                                                   ;;
                                                   ]]
                                                 )}

                         :html-handler    {
                                           :fn (fn [body]
                                                 {:status  200
                                                  :body    body
                                                  :headers {}}
                                                 )
                                           }

                         :wrap-to-handler {
                                           :fn (fn [result]
                                                 (fn [req]
                                                   result)
                                                 )
                                           }

                         :handler-kv      {
                                           :fn (fn [handler]
                                                 {:handler handler})
                                           }

                         :route           {
                                           :fn (fn [[method path handler]]
                                                 (compojure/make-route method path handler))
                                           }

                         :route-map       {
                                           ;; for now pass map
                                           :fn (fn [cfg]

                                                 (let [method (get cfg :method :get)
                                                       path (get cfg :path "/")
                                                       handler (get cfg :handler (fn [req]
                                                                                   {:status  200
                                                                                    :body    "<h1>Please provide handler!</h1>"
                                                                                    :headers {}}
                                                                                   ))
                                                       ]
                                                   (compojure/make-route method path handler)
                                                   )
                                                 )
                                           }

                         :routes          {
                                           :fn       (fn [route-list]
                                                       (apply compojure/routes route-list))
                                           :collect? true
                                           }

                         }
                        )

        ws-ctx-fn (fn [params]
                    {

                     :start-server {
                                    :fn (fn [handler]
                                          ;; port
                                          (let [shutdown-server (httpkit/run-server handler {:port 8001})]
                                            (swap! *WS-STATE assoc :shutdown shutdown-server)
                                            )

                                          ::started
                                          )
                                    }

                     :stop-server  {
                                    :fn (fn [_]
                                          (if-let [xtor (get-in @*WS-STATE [:xtor])]
                                            (do

                                              (prn "shutting down wf!")
                                              ((:shutdown @*WS-STATE))

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

                     }

                    )


        preview-steps-fn (fn [params]
                          {
                           ;; stay in infinite loop
                           ::loop            [:loop (:loop-chan params)]

                           ;; start web server (for now on port 8001)
                           ::server          [:start-server ::preview-routes] ;; todo: pass port

                           ;; prepare routes
                           ::preview-routes  [:routes
                                              [::index-route ::ws-route ::serve-static]] ;; todo: pass ws-route

                           ;; index / route
                           ::index-route     [:route-map ::index-opts]


                           ;; preview specific stuff

                           ;; preview page
                           ::preview-content [:EDN
                                              [:div "hello!"]]

                           ::preview-page    [:preview-page ::preview-content]

                           ;; hiccup to compojure handler
                           ::preview-html    [:hiccup->html ::preview-page]

                           ::GET             [:kv [:method :get]]
                           ::root-path       [:kv [:path "/"]]

                           ::index-fn        [:wrap-to-handler ::preview-html]
                           ::index-handler   [:handler-kv ::index-fn]
                           ; prepare a map with method/path/handler
                           ::index-opts      [:map [::GET ::root-path ::index-handler]]


                           ;; serve static resources under /woof
                           ::serve-static    [:static-route ["/woof" "public"]]

                           ;; ws for reload
                           ;; pass mult of the channel
                           ::ws-fn           [:ws-handler (:reload-mult params)]

                           ::ws-route        [:route-map ::ws-opts]

                           ::ws-handler      [:handler-kv ::ws-fn]
                           ::ws-path         [:kv [:path "/ws"]]
                           ::ws-opts         [:map [::GET ::ws-path ::ws-handler]]

                           }
                          )

        init-fns [(fn [params]
                    {
                     :loop-chan loop
                     ;;
                     :reload-chan reload-chan
                     :reload-mult reload-mult
                     }
                    )]
        opt-fns [keep-xtor-ref]
        ctx-fns [common-ctx-fn
                 loop-ctx-fn
                 hiccup-ctx-fn
                 routes-ctx-fn
                 ws-ctx-fn]
        steps-fns [preview-steps-fn]
        ]

    (let [wf (base/parametrized-wf!
               (base/combine-init-fns init-fns)
               wf-params-fn
               opt-params-fn

               (base/combine-fns opt-fns :merge-results base/merge-opts-maps)
               (base/combine-fns ctx-fns)
               (base/combine-fns steps-fns))]

      {
       :wf        wf
       :state     *WS-STATE
       :start-wf! (fn []
                    (base/run-wf! wf identity))

       :stop-wf!  (fn []
                    (if-let [xtor (get-in @*WS-STATE [:xtor])]
                      (do
                        (base/end! xtor)

                        (u/close-channels! [loop reload-chan])

                        ; (remove-watch *DATA :watcher)
                        ; (u/close-channels! (vals (:result @STATE)))
                        ::stopped
                        )
                      (do
                        (error ::no-wf-running)
                        ::no-wf-running)
                      ))
       }

      )
    )
  )


#_(defonce zzz (atom nil))

;; start dev stuff and close it after 10 seconds
#_(let [z (web-server-wf)]
  (reset! zzz z)

  ((:start-wf! z))

  (let [reload-chan (:reload-chan @(:state @zzz))]
    (go
      (u/timeout 2000)
      (async/put! reload-chan "reload")
      (u/timeout 3000)
      (async/put! reload-chan "reload")
      (u/timeout 3000)
      (async/put! reload-chan "reload")
      (u/timeout 3000)
      (async/put! reload-chan "reload")
      (u/timeout 3000)
      (async/put! reload-chan "reload")
      (u/timeout 3000)
      (async/put! reload-chan "reload")
      )
    )

  (async/thread
    (Thread/sleep 40000)

    (prn "closing")

    (async/put! (:loop @(:state z))
                {(wf/rand-sid) [:stop-server nil]})

    ;((:stop-wf! z))
    )
  )













(defn data-wf!
  "wf that works checks for updates of the data atom"
  [*DATA]

  (info ::YO!!!!)

  (let [wf-params-fn identity
        opt-params-fn identity

        ;;
        ;; shared stuff for workflow
        ;;
        STATE (atom {
                     :result {}
                     })


        loop (async/chan)

        init-fns [(fn [_]
                    (info ::init-fn)
                    {
                     :loop  loop
                     :state STATE
                     :*DATA *DATA
                     }
                    )

                  (fn [_]
                    {
                     :template (fn [data]
                                 [:div
                                  [:pre (pr-str data)]
                                  ]
                                 )}
                    )
                  ]



        opt-fns [
                 ;; save intermediary and final results
                 (fn [params]
                   (info ::opts params)
                   {
                    ; use time based updated on channel
                    :execute         (fn [executor] (base/time-chunk-execute 300 executor))

                    :op-handlers-map {
                                      :process (fn [result]
                                                 (let [state (:state params)]

                                                   #_(info ::process "\n"
                                                           (pretty-diff result (:result state)))

                                                   (swap! state assoc :result result))
                                                 )

                                      :error   (fn [err]
                                                 (error err))

                                      :done    (fn [result]
                                                 ;; will be called on stop

                                                 #_(info ::result "\n"
                                                         (pr-str result))

                                                 (.println *err* (str "\n" (d/pretty result) "\n"))

                                                 (swap! (:state params) assoc :result result)
                                                 )
                                      }
                    }
                   )

                 ;; save constructor, in order to be able to stop wf
                 (fn [params]
                   (info ::save-xtor)
                   {
                    :before-process (fn [wf-chan xtor]
                                      (swap! (:state params) assoc :xtor xtor)

                                      :ok
                                      )})
                 ]

        ctx-fns [
                 (base/arg-fn
                   (fn [& {:keys [port state] :as params}]
                     (info ::ctx)
                     {
                      ;; this will add new actions to a wf
                      :loop  {
                              :fn       (fn [loop]
                                          ;; (u/wiretap-chan in-chan (partial println "UI LOOP:"))
                                          loop)

                              :infinite true
                              :expands? true
                              }

                      ;; just print
                      :print {:fn (fn [v]
                                    (.println *err* (str "\n" (pr-str v) "\n"))

                                    v)}

                      ;; installs watcher on *DATA and emits changes onto a chan
                      :watch {
                              :fn       (fn [*DATA]
                                          (let [ch (async/chan)]
                                            (add-watch *DATA :watcher
                                                       (fn [key atom old-state new-state]
                                                         (info "-- Atom Changed --")
                                                         ;(prn "key" key)
                                                         ;(prn "atom" atom)
                                                         ;(prn "old-state" old-state)
                                                         ;(prn "new-state" new-state)

                                                         (async/put! ch new-state)
                                                         ))

                                            (async/put! ch @*DATA)
                                            ch
                                            ))
                              :infinite true
                              }
                      }
                     )
                   )
                 (fn [params]
                   (let [tempate-fn (:template params)
                         state (:state params)
                         ]
                     {

                      ;;
                      :render             {
                                           :fn       (fn [data]
                                                       (tempate-fn data))
                                           :infinite true
                                           }

                      :save-render-result {:fn (fn [data]
                                                 (swap! state assoc :preview data))}
                      }
                     )
                   )
                 ]

        steps-fns [(base/arg-fn
                     (fn [& {:keys [loop *DATA]}]
                       ;(info ::init-steps)

                       (let [v-sid (base/rand-sid)
                             v-ref (base/rand-sid "&")
                             v* (base/rand-sid "*")
                             zip (base/rand-sid "zip-")

                             z-sid (base/rand-sid)
                             ]
                         {

                          ;; v-sid [:v "A\nA\nA\n"]
                          ;; v-ref [:&v [v-sid]] ;; get a reference to it
                          ;; v*    [:v* [v-sid]] ;; get a value list
                          ;; z-sid [:v "BBBBBBBB"]

                          ;;::foo [:v1 v-ref]
                          ;;::bar [:v1 v*]
                          ;::p1 [:print ::foo]
                          ;;::p2 [:zip [::foo ::bar]]
                          ;zip   [:zip [v-ref v*]] ;; interleave them
                          ; ::print-zip [:print [v-ref v*]]


                          ;; test that wf is running
                          ;; ::test [:print "hello"]

                          ;; a loop to make the wf infinite
                          ;   ::loop    [:loop loop]

                          ;; install watcher on data
                          ::watcher  [:watch *DATA]
                          ;; print on *DATA changes
                          ;; ::print [:print ::watcher]

                          ::renderer [:render ::watcher]
                          ::save     [:save-render-result ::render]

                          }
                         )
                       )

                     )]

        wf (base/parametrized-wf!
             (base/combine-init-fns init-fns)
             wf-params-fn
             opt-params-fn
             (base/combine-fns opt-fns :merge-results base/merge-opts-maps)
             (base/combine-fns ctx-fns)
             (base/combine-fns steps-fns))]


    ;; what to return for user?
    {
     :wf        wf
     :state     STATE
     :start-wf! (fn []
                  (base/run-wf! wf identity))

     :stop-wf!  (fn []
                  (if-let [xtor (get-in @STATE [:xtor])]
                    (do
                      (base/end! xtor)

                      (remove-watch *DATA :watcher)
                      (u/close-channels! (vals (:result @STATE)))

                      ::stopped
                      )
                    (do
                      (error ::no-wf-running)

                      ::no-wf-running)
                    ))
     }
    )
  )


#_(let [*data (atom {:hello "woof"})
        wf (data-wf! *data)]
    ; start wf
    ((:start-wf! wf))

    (async/thread
      (Thread/sleep 100)

      (swap! *data assoc :test 1234)

      (Thread/sleep 100)

      (dotimes [n 3]
        (swap! *data assoc n (str n))
        )

      (Thread/sleep 500)

      ;(info @*data)

      ((:stop-wf! wf))

      (reset! *DATA @*data)

      )
    )
