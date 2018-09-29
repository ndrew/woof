(ns woof.example.files-ui
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.ws :as ws]

    [woof.utils :as u]

    [woof.ui :as ui]
    [woof.wf-ui :as wf-ui]
    [woof.ui.results :as r]


    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; woof commander

;; real world example of client server communication



;; ws communication


(defn response-fn
  "forms steps from the server message"
  [msg]
  (.warn js/console (str "SERVER: " (d/pretty msg)))

  ;; currently assume that server sends [k v] message

  (let [[op v] msg]
    (condp = op
      :cwd (let [cwd-sid (wf/rand-sid)]            ;; got cwd
             {
               cwd-sid [:cwd! v]
               (wf/rand-sid) [:get-files cwd-sid]
               })
      :dir {                                         ;; got list of files
             (wf/rand-sid) [:files! v]
           }
      ;; else
      {
        (wf/rand-sid) [:log msg]
        }
      )
    )
  )


(defn- default-state[]
  {
    :cwd nil
    :files []


    ;; communication

    :comm {
            :client []
            :server []
            }

    }
  )


;; internal state
(defonce *state (atom (default-state)))

(def cursor (partial rum/cursor-in *state))


;; track calls to ws and from ws for visualization

(def *comm (cursor [:comm]))

(defn server-comm! [e]
  (swap! *comm update-in [:server] conj e)
  (swap! *comm update-in [:client] conj [])
)

(defn client-comm! [e]
  (swap! *comm update-in [:client] conj e)
  (swap! *comm update-in [:server] conj []))


;; init and return parameters needed for
;;  * ws communication
;;  * ui
;;  * wf internal state
(defn prepare-params! [endpoint-url]



  (let [server-in (async/chan)
        server-out (async/chan)
        endpoint (ws/ws-server endpoint-url
                                  :on-message (fn [msg]
                                    ; (.warn js/console (d/pretty msg))


                                    (when-let [steps (response-fn msg)]
                                      ;;
                                      (server-comm! [msg steps])

                                      (go
                                        (async/put! server-out steps)))))]

  (reset! *state (default-state))
  {
    ;; ws endpoint
    :endpoint endpoint

    ;; endpoint in/out channels
    :server-in server-in
    :server-out server-out

    ;; ui
    :ui-chan (async/chan)

    ;; state
    :local-state *state

   })
  )


;; context handlers

(defn gen-ui-loop-handler []
  {:fn (fn [in-chan]
         (u/wiretap-chan in-chan (partial println "FILES: ui loop:")))
   :infinite true
   :expands? true
   }
  )

;;
;; context function



(defn context-map-fn [& {:keys [ui-chan server-in server-out endpoint ]}] ;;
  {
    ;; utils

    :log {:fn (fn[v]
                (println v)
                v)}


    ;; ui
    :ui-loop (gen-ui-loop-handler)


    ;; saves cwd to local state
    :cwd! {:fn (fn[cwd]
                 (reset! (cursor [:cwd]) cwd))}

    ;; stores list of files in local state
    :files! {:fn (fn[files]
                   (reset! (cursor [:files]) files))}


    ;; ws

    ; waits for server responses from channel passed as parameter
    :server< (wf/receive-steps-handler
               :step-fn
               (fn [steps]
                 (println (d/pretty ["adding new steps from server:" steps]))
                 steps))



    ;; sends steps to server to set the cwd
    :set-cwd (wf/send-value-handler*              ;; todo: find better name
               server-in
               :v-fn (fn[v]
                       (let [sid (wf/rand-sid)
                             sid1 (wf/rand-sid)
                             steps {
                                     sid [:cd v]
                                     sid1 [:cwd-response sid]
                                     (wf/rand-sid) [:client> sid1]
                                     }]

                         (client-comm! [[:set-cwd v] steps])
                         steps
                         )
                       )
               :out-fn (fn [z]
                         (ws/send! endpoint z)))




    ;; sends steps to server worflow to file names for director
    :get-files (wf/send-value-handler*                ;; todo: find better name
                 server-in
                 :v-fn (fn[v]


                         (let [sid (wf/rand-sid)
                               sid1 (wf/rand-sid)

                               steps {
                                       sid [:dir v]
                                       sid1 [:dir-response sid]
                                       (wf/rand-sid) [:client> sid1]
                                       }
                               ]

                             (client-comm! [[:get-files v] steps])
                             steps
                           )

                         )
                 :out-fn (fn [z]


                           (ws/send! endpoint z))

                 )
})



;;
;; steps

(defn steps-fn [& {:keys [ui-chan server-in server-out]}]
    {
      ::ui  [:ui-loop ui-chan]
      ::server-loop [:server< server-out]
      }
)





(defn actions-fn [& {:keys [ui-chan server-in server-out endpoint]}]
  (let [start-wf! (fn [] ;; may return channel
                    (ws/start endpoint))

        stop-wf! (fn []
                    (ws/close! endpoint)

                    (async/close! ui-chan)
                    (async/close! server-in)
                    (async/close! server-out)
                    )]
    {
      :start! start-wf!
      :stop!  stop-wf!
      :reset! (fn []           ;; todo: fix restart
                (stop-wf!)
                (start-wf!)
                )

      :actions []             ;; todo: add more actions
      }
    )
  )



;;
;; UI
;;


(rum/defcs <file> < rum/reactive
  [local f]
  [:.file f]
)


(rum/defcs <folder> < rum/reactive
  [local f select-fn]

  [:.folder [:a {:href "#"
                 :on-click (fn[e]
                             (select-fn f)
                             (.preventDefault e)
                             )
                 } f]]

)



(rum/defc <action> < rum/reactive[v]

  (let [[action params] v]
    [:div
     (d/pretty action)
       (if (string? params)
         (d/pretty params)
         (ui/menu-item "..." (fn [] (println (d/pretty params)))))

     ]

    )
  )

(rum/defc <log> < rum/reactive
  [[client server]]

  (let [server? (= client [])]
    (if server?
      [:div.hbox
       [:div (ui/menu-item "..." (fn [] (println (d/pretty (last server)))))]
       [:div "⇦"]
       (<action> (first server))
       ;[:div (d/pretty (first server))]
      ]
      [:div.hbox
       ; [:div (d/pretty (first client))]
       (<action> (first client))
       [:div "⇨"]
       [:div (ui/menu-item "..." (fn [] (println (d/pretty (last client)))))]
      ]
      )
    )
)

(rum/defcs <mc> < rum/reactive
  (rum/local "" ::pattern)
  [local *STATE *s ui-chan]

  (let [cursor (partial rum/cursor-in *STATE)
        {status :status
         } @*STATE

        ]

    [:div.mc
     [:h5 "file manager example"]

     (wf-ui/<wf-menu-ui> "woof commander:"
                         @(cursor [:status])
                         @(cursor [:wf :status-actions]))


     (let [{cwd :cwd
            files :files} @*s]


       [:div ;; .hbox

        ;[:pre (d/pretty @*STATE)]

        (let [comm @*comm
              client-comm (:client comm)
              server-comm  (:server comm)


              ]

          (into
          [:div.communication
           [:div.hbox
            [:header "Server"]
            [:header "-"]
            [:header "Client"]
            ]
           ]
          (map <log> (map vector client-comm server-comm)))

          )


        [:div.files
         [:header cwd
          [:a {:href "#"
               :style {:margin-left "1rem"}
               :on-click (fn[e]
                           (go
                             (async/>! ui-chan { (wf/rand-sid) [:set-cwd [cwd ".."]] }))
                           (.preventDefault e)
                           )
               } "back"]
          ]

         (into [:div]
               (map (fn[f]
                      (if (clojure.string/starts-with? f "/")
                        (<folder> f (fn [f]
                                      (go
                                        (async/>! ui-chan { (wf/rand-sid) [:set-cwd [cwd (subs f 1)]] }))))

                        (<file> f)
                        )
                      ) (sort files)))]

        ]
       )
     ]
    )
  )



(defn ui-fn [& {:keys [ui-chan server-in server-out endpoint local-state]}]

  (fn [*STATE]
    [:div
      (<mc> *STATE local-state ui-chan)
     ])

)
