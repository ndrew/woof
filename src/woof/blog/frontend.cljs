(ns woof.blog.frontend
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




(defn response-fn
  "forms steps from the server message"
  [msg]
  (.warn js/console (str "SERVER: " (d/pretty msg)))

  ;; currently assume that server sends [k v] message

  (let [[op v] msg]
    (condp = op
      ;; else
      :cfg (let [sid (wf/rand-sid)]
               {
                 sid [:cfg! v]
                 })
      {
        (wf/rand-sid) [:log msg]
        }
      )
    )
  )


(defn- default-state[]
  {
    :current :cfg ;; :posts

    :cfg {}


    }
  )



;; internal state
(defonce *state (atom (default-state)))

(def cursor (partial rum/cursor-in *state))


;; init and return parameters needed for
;;  * ws communication
;;  * ui
;;  * wf internal state
(defn prepare-params! [endpoint-url]



  (let [server-in (async/chan)
        server-out (async/chan)
        endpoint (ws/ws-server endpoint-url
                                  :on-message (fn [msg]
                                    (when-let [steps (response-fn msg)]
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


(defn receive-steps-handler
  "creates a step handler config [:your-handler-name <in-channel>] that will wait for the steps from <in-channel>.
  Parametrizeble by optional :step-fn param "
  [& {:keys [step-fn]
      :or {step-fn identity}}]
  {
    :fn (fn [in>]
          (let [chan> (async/chan)]
            (go-loop []
                     (when-let [new-steps (async/<! in>)]

                       (if-not (map? new-steps)
                         (u/throw! (str "invalid expand map passed to :in " (d/pretty new-steps))))

                       ; (locking *out* (println "server: IN" (d/pretty new-steps)))

                       (async/put! chan> (step-fn new-steps))
                       ;; todo: use :expand-key instead of having intermediary steps
                       ;; #_(async/put! chan> (with-meta {sid v} {:expand-key sid}))
                       (recur)
                       )
                     )
            chan>))
    :infinite true
    :expands? true
    }
  )



(defn context-map-fn [& {:keys [ui-chan server-in server-out endpoint local-state]}] ;;
  {
    ;; utils

    :log {:fn (fn[v]
                (println v)
                v)}


    ;; ui
    :ui-loop (gen-ui-loop-handler)


    ;; ws

    ; waits for server responses from channel passed as parameter
    :server< (receive-steps-handler
               :step-fn
               (fn [steps]
                 (println (d/pretty ["adding new steps from server:" steps]))
                 steps))

    ;;
    :cfg! {:fn (fn[v]
                 (swap! local-state assoc :cfg v)
                 v
                 )}


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

      :actions [
                 ;; edit blog params
                 ["config" (fn[])]

                 ;; show posts
                 ["posts" (fn[])]

                 ["generate all" (fn[])]

                 ]             ;; todo: add more actions
      }
    )
  )



;;
;; UI
;;


;; how to make this as a component

(rum/defcs <blog> < rum/reactive
  [local *STATE *s ui-chan]

  (let [cursor (partial rum/cursor-in *STATE)
        {status :status
         } @*STATE]

    [:div.blog
     [:h5 "blog example"]
     (wf-ui/<wf-menu-ui> "woof commander:"
                         @(cursor [:status])
                         @(cursor [:wf :status-actions]))


       (let [{cfg :cfg} @*s]
         [:pre
          (d/pretty cfg)

         ]
      )
     ]
  )
)


(defn ui-fn [& {:keys [ui-chan server-in server-out endpoint local-state]}]

  (fn [*STATE]
    [:div
      (<blog> *STATE local-state ui-chan)
     ])

)
