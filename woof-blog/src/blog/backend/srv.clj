(ns blog.backend.srv
  (:require
    [clojure.java.browse :as browse]

    [compojure.core :as compojure]
    [compojure.route :as route]
    [compojure.handler :refer [site]]

    [org.httpkit.server :as httpkit]

    [taoensso.timbre :as timbre :refer [info warn] ]

    [woof.base :as base]
))


(defn server-ctx-fn [params]
  (let [*state (base/&state params)]
    {
     ;; start server with the specified configuration
     :start-server    {
                       :fn (fn [server]
                             (let [{handler :handler
                                    port    :port} server]
                               (let [shutdown-server (httpkit/run-server handler {:port port})]
                                 (swap! *state assoc ::server-shutdown shutdown-server))

                               ::started
                               )
                             )
                       }
     ;; builds route that serves static
     :static-route    {:fn (fn [[path root]]
                             (route/files path {:root root}))}

     ;; collects routes into route-list
     :routes          {
                       :fn       (fn [route-list]
                                   (apply compojure/routes route-list))
                       :collect? true
                       }

     ;; generic rout handler
     :route           {
                       :fn (fn [[method path handler]]
                             ; (prn :route path)
                             (compojure/make-route method path handler))
                       }


     :wrap-to-handler {
                       :fn (fn [result]
                             {:handler (fn [req]
                                         ;; (prn "to hiccup")
                                         result
                                         )}
                             )
                       }

     :route-map       {
                       :fn (fn [cfg]
                             (let [method (get cfg :method :get)
                                   path (get cfg :path "/")
                                   handler (get cfg :handler (fn [req]
                                                               {:status  200
                                                                :body    "<h1>Please provide handler!</h1>"
                                                                :headers {}}
                                                               ))]
                               (compojure/make-route method path handler)))
                       }

     :open-browser {
                    :fn (fn [url]
                          (browse/browse-url url))
                    }

     }
    )
  )

(defn server-impl []
  {:ctx server-ctx-fn
   :opts (base/build-opt-on-done ;; ;; custom 'shutdown' handler, for stopping server
                                  (fn [params _]
                                    (let [*STATE (base/&state params)]
                                      (if-let [shutdown-fn (::server-shutdown @*STATE)]
                                        (shutdown-fn)
                                        (warn "no ::server-shutdown fn"))
                                      )))
   }
  )