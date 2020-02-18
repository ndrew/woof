(ns woof.alpha.server
  "woof alpha backend"
  (:require

    [compojure.core :as compojure]
    [compojure.route :as route]
    [compojure.handler :refer [site]]

    [ring.util.response :as response]

    [clojure.core.async :as async :refer [go go-loop]]

    ;; woof
    [woof.alpha.wf.ping :as ping]
    )
  (:gen-class))



(compojure/defroutes
  app
  ;; serve the application
  (compojure/GET "/" [] (response/resource-response "public/index.html"))
  ;; and it's resources
  (route/resources "/" {:root "public"})

  ;;
  (compojure/GET "/ping" []
    ;; this one triggers a wf, waits for its execution and then returns a result
    (let [wf-result @(ping/ping-wf-sync)]
      ;; wf-result will contain ns-specific keywords, we have to map this to response properly
      (ping/prepare-response wf-result)
      )

    )
  )
