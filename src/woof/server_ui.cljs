(ns woof.server-ui
  (:require
    [cljs.core.async :as async]
    [rum.core :as rum]

    [woof.app-data :as app-model]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.ws :as ws]

    [woof.utils :as u]
    [woof.ui :as ui]
    [woof.wf-ui :as wf-ui]

    [clojure.data :as cd]))



(rum/defcs <server-ui> < rum/reactive
                         (rum/local nil  ::socket)
  [local model server]
  (let [actions [ ["init ws" (fn[]
                                  (ws/start server)
                                (reset! (::socket local) (ws/get-socket server)))]

                  ["client-server wf"
                   (fn[]
                     (app-model/start! model
                                       (fn [model]
                                         (let [opts {
                                                      :channel (app-model/get-xctor-chan model)
                                                      :op-handler (partial ws/server-wf-handler model)
                                                      }
                                               xctor (app-model/get-xctor model)
                                               worker (wf/->AsyncWFProcessor xctor opts)]

                                           (wf/process-results! worker)
                                           )
                                         ))


                     )
                   ]
                  ]
        socket @(::socket local)
        ]

    [:div
       (ui/menubar "Server:"
                   (if socket
                     (into actions [["client ping"
                                     (fn[]
                                       ;(ws/send! server [:debug (str "Ping " (u/now))])

                                       (ws/send! server [:server-time ""])

                                       ;(.send socket (ws/write-transit [:client-ping "Hello"]))
                                       )]])
                     actions
                     )
                   )]))
