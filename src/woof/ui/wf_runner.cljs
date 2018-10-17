(ns woof.ui.wf-runner
  (:require
    [cljs.core.async :as async]
    [clojure.data :as cd]

    [rum.core :as rum]

    [woof.data :as d]

    [woof.wf :as wf]
    [woof.wfc :as wfc]

    [woof.wf-data :as wdata]
    [woof.wf-ui :as wf-ui]

    [woof.ws :as webservice]


    [woof.ui :as ui]

    [woof.ui.context :as ctx-ui]
    [woof.ui.steps :as steps-ui]
    [woof.ui.results :as r]

    [woof.utils :as u]

    [woof.test-data :as test-data]

    ;; examples
    ;;
    [woof.example.ui-loop :as ui-loop]

    [woof.example.popup :as popup]

    [woof.example.files-ui :as files-ws]

    [woof.example.ws :as ws] ;; todo: rename

    [woof.example.ouroboros :as ouroboros]
    [woof.example.infinite :as infinite]
    [woof.example.big-wf :as big-wf]


    [woof.example.edn-editor.config-editor :as cfg-wf]

    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; UI playground

;; there should be a way to have some kind of customizable UI for workflows:

;; so far
;;
;; + ui workflow consists of:
;;
;;   * workflow arguments
;;       will be passed as & {:key} args to further 'constructors'
;;
;;     * steps-fn - provides initial steps
;;        usually there will be infinite expand action - for ui loop
;;
;;     * context  - provides context map
;;        usually more specific context will be merged to a more generic context map
;;
;;     * actions
;;        provides specific start and stop function for current workflow
;;        provides list of available actions for wf per status (not-started, working, error, .. )
;;
;;     * ui
;;        provides a custom rum component for wf or generic one
;;





;;


;; test
(defn config-ws-1 []
  (let [in (async/chan)
        out (async/chan)

          ;; substitute with cursor
        *local (atom {:current {:path "Users/ndrw/m/woof/example.edn"}})

          ]


    (let [end-chan (async/chan)
          ;; opts
          init-fn (fn [wf-chan xtor]

                    (go
                      (when-let [v (async/<! end-chan)]
                        (locking *out* (println "stopping wf"))
                        (wf/end! xtor)
                        )
                      )

                    ;; simulate receiving from socket
                    (go-loop []
                             (if-let [v (async/<! out)]
                               (do
                                 ;; ws
                                 (locking *out* (println "RECEIVED" v))
                                 (recur))
                               (do
                                 (locking *out* (println "STOPPED WF"))

                                 (async/close! in)
                                 (async/close! out)
                                 )
                               ))
                    :ok
                    )

          opts {
                 :before-process init-fn
                 :op-handlers-map {
                                    :done (fn [data]
                                            (locking *out* (println "DONE!\n" (d/pretty data))))
                                    :error (fn [data]
                                             (locking *out* (println "ERROR!\n" (d/pretty data))))
                                    }
                 }

          ; wf constructor
          wwf (partial cfg-wf/wwf in out *local)
          ; wf params

          ; simulate putting data on the wire
          socket-send (fn [v]
                        (go
                          (async/put! out v)))

          socket-send-transit (fn [v]
                                (go ; or use (write-transit-str v)
                                  (async/put! out v)))

          params {
                   :send! socket-send
                   :send-transit! socket-send-transit

                   :initial-steps {
                                    ;; these are sync by value
                                        ;::test-file [:write-file {
                                        ;                                :path "/Users/ndrw/m/woof/example1.edn"
                                        ;                                :contents "Aloha Woof!"
                                        ;                                }]
                                        ;::set-current-1 [:set-current ::test-file]

                                    ;;::init-read [:read-current ::init-path]

                                    ;; instant set
                                    ;;::init-path [:state! [(d/selector [:current])
                                     ;;                     {:path "/Users/ndrw/m/woof/example.edn"}]]

                                      ::log [:log "I'm alive!"]

                                        }
                   }

          ]

    (wfc/wf-async-process! (wwf params) opts)))
  )



(defn ui-opts [endpoint-url receive-fn close-fn]
  (let [*endpoint (atom nil)

        socket-send         (fn [v]
                              (println "sending" v @*endpoint)
                              ;(js/setTimeout (fn[]
                                (webservice/send! @*endpoint
                                        {(wf/rand-sid) [:debug v]})
                              ;                 ) 4000)

                              )
        socket-send-transit (fn [v]
                              ;(httpkit/send! socket-chan (write-transit-str v))
                              )


        ;; add the helper funcs
        params {
                 :send! socket-send
                 :send-transit! socket-send-transit
                 }

        init-fn (fn[wf-chan xtor]

                  (let [endpoint (webservice/ws-server endpoint-url
                                                       :on-message (fn [msg]
                                                                     (receive-fn msg))

                                                       ;; how to init both on-close and socket-send

                                                       :on-close (fn [] ;; arguments
                                                                   (wf/end! xtor)
                                                                   (close-fn :status!)
                                                                   )
                                                       )]
                    (reset! *endpoint endpoint)

                    (let [chan (webservice/start endpoint)]
                      ;; return the signaling channel, so the wf will wait for the ws to be initialized
                      chan)
                    )
                  )
        ]
    {
      :params params
      :opts {
              :before-process init-fn
              }
      })
  )





;;
;; main state atom for workflow runner

(declare default-ui-fn)
(declare init-runner-wf!)
(declare init-ui!)
(declare runner-processing-opts)

;; wf constructor ui

;; creating wf via wf constructor
(defn config-ws [*STATE]

  ;;

  (let [;; call wf constructor
        {
          wwf :wf                 ;; workflow function
          receive-fn :receive-fn    ;;
          close-fn :close-fn        ;;
          actions :actions
          ;; todo: return default params?
          } (cfg-wf/wf! *STATE) ;; for now pass the whole state

        {
            params :params
            opts :opts
            } (ui-opts "/api/config" receive-fn close-fn)

        ]

    ;; uncomment for auto-start
    ;; (wfc/wf-async-process! (wwf params) opts)



    ;; prepare wf for ui

    (let [ui-fn (partial default-ui-fn "client-server edn editor")

          args (apply concat params) ;; todo: get from WF

          WF (wwf params)
          xtor (wfc/wf-xtor WF)

          opts (merge opts
                      (runner-processing-opts *STATE))

          steps (wfc/get-steps WF)
          context-map (wfc/get-context-map WF)


          cursor (partial rum/cursor-in *STATE)
          start-fn (fn []

                     (reset! (cursor [:status]) :woof.app/running)

                     ; (reset! (cursor [:status]) :woof.app/running)
                     (wf/process-results! (wf/->ResultProcessor xtor
                                                                opts

                                                                ))

                     )
          stop-fn  (fn [] (wf/end! xtor))
          ]

      (init-ui! *STATE
                {
                     :steps steps
                     :context-map context-map
                     :args args

                  :status-actions {
                                       :woof.app/not-started [
                                                               ["start" start-fn]
                                                               ]

                                       :woof.app/done        [
                                                               ["finish" (fn[]

                                                                          ;;(reset-fn)
                                                                          ; (init!)
                                                                          )]
                                                              ; todo: restart workflow?
                                                              ["restart" (fn[]
                                                                           ;; todo: we need a new xtor for these
                                                                           )]
                                                              ]

                                       :woof.app/running     (into actions
                                                                   [[]
                                                                    ["stop" stop-fn]
                                                                    ]
                                                                   )
                                       :woof.app/error       [
                                                                ["start" start-fn]
                                                               ; ["restart" reset-fn]
                                                               ]
                                       ; :woof.app/stopped     "error"
                                       }


                     :start! start-fn
                     :stop! stop-fn

                     } (apply ui-fn args))

      )

    )
  )




;; basic uis

(defn ouroboros-wf [*STATE]
  (init-runner-wf! *STATE
    (ouroboros/prepare-params!)
    ouroboros/context-map-fn
    ouroboros/steps-fn
    ouroboros/actions-fn
    (partial default-ui-fn "infinite clock")
    :auto-start true
  )
)

(defn infinite-wf [*STATE]
  (init-runner-wf! *STATE
    (infinite/prepare-params!)
    infinite/context-map-fn
    infinite/steps-fn
    infinite/actions-fn
    (partial default-ui-fn "infinite workflow")
    :auto-start true
  )
)

(defn expand-wf [*STATE]
  (init-runner-wf! *STATE
    (big-wf/prepare-params!)
    big-wf/context-map-fn
    big-wf/steps-fn
    big-wf/actions-fn
    (partial default-ui-fn "expand")
    ; :auto-start true
  )
)



;; complex uis

(defn ui-loop-wf [*STATE]
  (init-runner-wf! *STATE
    {:ui-chan (async/chan)}
    ui-loop/context-map-fn
    ui-loop/steps-fn
    ui-loop/actions-fn
    (partial default-ui-fn "Example of using workflow with infinite expand handler as ui-loop."))
  )


(defn popup-wf [*STATE]
  (init-runner-wf! *STATE
    (popup/prepare-params)
    popup/context-map-fn
    popup/steps-fn
    popup/actions-fn
    popup/ui-fn)
  )



(defn ws-wf [*STATE]
  (init-runner-wf! *STATE
    (ws/prepare-params! "/api/test") ;; "/api/websocket"
    ws/context-map-fn
    ws/steps-fn
    ws/actions-fn
    (partial default-ui-fn "websocket RPC")
    :auto-start true
  )
)


(defn files-ws-wf [*STATE]
  (init-runner-wf! *STATE
    (files-ws/prepare-params! "/api/files")
    files-ws/context-map-fn
    files-ws/steps-fn
    files-ws/actions-fn
    files-ws/ui-fn
    :auto-start true
  )
)








(defonce *UI-STATE (atom
    {


      :basic-worflows [
                        ["config" config-ws]


                        ["ouroboros" ouroboros-wf]
                        ["infinite" infinite-wf]
                        ["expand" expand-wf]
                        ]
      :complex-workflows [


                            ["UI loop" ui-loop-wf]
                            []
                            ["file browser" files-ws-wf]
                            []
                            ["popup"   popup-wf ]
                            []
                            ["rpc via webservice" ws-wf]
                            []
                            ]


      ;; status of current workflow
      :status :woof.app/not-started

      ;; workflow specific map
      :wf nil

      ;; stores wf arguments (just in case)
      :wf-args []

      ;; log
      :history []

      ;; current resulting map
      :result {}

      ;; current steps
      :steps {}

      ;; current ui
      :rum-ui nil

      }))


(defn init!                      ;; actually it's reset
  "init ui state"
  []
  (swap! *UI-STATE merge
         {
           :wf nil

           :status :woof.app/not-started
           :history []

           :result {}
           :steps {}

           :wf-args []
           })
  )



(defn init-ui! [*STATE ui-wf ui-fn]
  (swap! *STATE merge
           {
             :wf ui-wf

             :status :woof.app/not-started
             :history []

             :rum-ui ui-fn
             })

  )


(defn- runner-processing-opts [*STATE]
  (let [cursor (partial rum/cursor-in *STATE)]
    {
      :op-handlers-map {

                         :wf-update (fn[data]
                                      (swap! (cursor [:steps]) merge (first data))
                                      (swap! (cursor [:result]) merge (second data)))

                         :process (fn[data]
                                    (swap! (cursor [:history]) conj
                                           (wdata/inline-results data))
                                    (swap! (cursor [:result]) merge data))

                         :done (fn [data]
                                 (swap! (cursor [:history]) conj
                                        (wdata/inline-results data))
                                 (reset! (cursor [:status]) :woof.app/done))

                         :error (fn [data]
                                  (.error js/console "ERROR" data)
                                  (reset! (cursor [:status]) :woof.app/error))

                         }
      ;; :timeout 1000
      }))









(defn- runner-actions
  "prepares ui wf start/stop functions and available wf actions groupped by status"
  [*STATE actions-map xtor]

  (let [processing-opts (runner-processing-opts *STATE) ;; todo: specify more processing opts

         cursor (partial rum/cursor-in *STATE)

         ;; todo: are this ok?
         {
           wf-start-fn :start!
           wf-stop-fn :stop!
           wf-reset-fn :reset!

           actions :actions
         } actions-map

         ;; if :start! function is present:
         ;; * does wf specific initialization (can be deferred if channel is returned)
         ;; * starts wf
         start-fn (fn[]
                   (let [f (fn []
                             (let [proc (wf/->ResultProcessor xtor processing-opts)]
                               (wf/process-results! proc))

                 ;; todo: replace these wai opts
                             (reset! (cursor [:status]) :woof.app/running))]

                   (let [v (if wf-start-fn (wf-start-fn))]
                     (if (u/channel? v)
                       (go
                          (if-let [nu-v (async/<! v)]
                            (f)))
                       (f)))))

        ;; ends wf, does wf specific clean-up
        stop-fn (fn[]
                      (wf/end! xtor)

                      (if wf-stop-fn
                        (wf-stop-fn)))


        ;; todo: why reset-fn?
        reset-fn (fn []
                    (if wf-reset-fn
                       (wf-reset-fn))
                   )

        ;;
        stop-action ["stop" stop-fn]

        status-actions  {
                        :woof.app/not-started [["start" start-fn]]

                        :woof.app/done        [["finish" (fn[]
                                                           (reset-fn)
                                                           (init!)
                                                           )]
                                               ; todo: restart workflow?
                                                ["restart" (fn[]
                                                            ;; todo: we need a new xtor for these
                                                            )]
                                               ]

                        :woof.app/running     (into actions
                                                    [[] ["stop" stop-fn]])
                        :woof.app/error       [["start" start-fn]
                                               ["restart" reset-fn]]
                          ; :woof.app/stopped     "error"
                        }

         ]

    {
      :status-actions status-actions

      :start! start-fn
      :stop! stop-fn
      }
    )
  )




(defn init-runner-wf!
  "initializes ui with specific ui workflow
   *STATE     - map in atom, ui state, for rum updates
   params     - workflow map of wf parameters

   context-fn - (fn [& {:keys [...]}]) -> context-map
   steps-fn   - (fn [& {:keys [...]}]) -> initial steps map

   actions-fn - (fn [& {:keys [...]}]) -> {
      :start! (fn[]) -> channel/nil ;  if channel — wait for it before starting wf
      :stop! (fn[]) -> ; clean-up
      :reset! (fn[]) -> ; some global clean-up
      :actions <menu-actions> ; available actions during wf in format as for menus
   }

   ui-fn      - (fn [& {:keys [...]}])
  "
  [*STATE
   params

   context-fn
   steps-fn
   actions-fn

   ui-fn
   & {:keys [auto-start]}
   ]

  (let [args (apply concat params) ;; maybe add here some other params

        ;; todo: migrate to use WoofWorkflow
        context-map (apply context-fn args)
        steps (apply steps-fn args)

        xtor (wf/build-executor (wf/make-context context-map) steps)

        wf-actions (runner-actions *STATE (apply actions-fn args) xtor)]


    #_(swap! *STATE merge
           {
             :wf (merge
                   {
                     :steps steps
                     :context-map context-map
                     }
                   wf-actions)

             :status :woof.app/not-started
             :history []

             :rum-ui (apply ui-fn args)
             :wf-args args
             })

    (init-ui! *STATE (merge
                   {
                     :steps steps
                     :context-map context-map
                     :args args
                     }
                   wf-actions)
              (apply ui-fn args))

    (if auto-start
      ((:start! wf-actions)))

    )
  )





;;
;; UI


;; default wf runner ui
(rum/defcs <wf-ui> < rum/reactive
  [local header *STATE]

  (let [cursor (partial rum/cursor-in *STATE)

        {status :status
         wf :wf
         full-history :history} @*STATE]

    [:div.wfui
     [:h5 header]

     ;; pre-wf stuff: steps

     [:div.hbox
      (steps-ui/<steps> (cursor [:wf :steps]) @(cursor [:wf :context-map]))
      (ctx-ui/<context> (cursor [:wf :context-map]))
      ]

     ;; wf actions menu

     (wf-ui/<wf-menu-ui> "wf:" status @(cursor [:wf :status-actions]))

     ;; results

     (let [history (reverse @(cursor [:history]))]
       [:.log

        ;; results
        [:div
         [:h2 "result:"]
         [:pre (d/pretty (first history))]
         [:h4 "last added"]
         [:pre
          (let [[cur prev] (first (partition 2 history))
                [added _ _] (cd/diff cur prev)
                ]
            (d/pretty added)

            )
          ]
         ]

        [:hr]
        ;; todo: migrate old ui
        (r/<wf-results-ui> "result"
                           @(cursor [:result])
                           @(cursor [:steps])
                           (atom {:pre   {} ;; *editors
                                  :post {}}))


        ])
     ]
    )
  )


(defn default-ui-fn [header]
  (fn [*STATE]
    (<wf-ui> header *STATE)))



(rum/defc <wf-list> < rum/reactive [*STATE]
  (let [inject-state (fn[item]
                        (if-let [[h action] item]
                          (if action
                            [h (partial action *STATE)]
                            item)))

         { basic-items :basic-worflows
           complex-items :complex-workflows} @*STATE]


    [:div
     [:div (ui/menubar "Simple workflows:" (map inject-state basic-items))]
     [:hr]
     [:div (ui/menubar "Complex workflows:" (map inject-state complex-items))]]
    )
  )


(rum/defc <wf-runner-ui> < rum/reactive [*STATE]

  (let [{wf :wf
         ui  :rum-ui} @*STATE

        <ui> (if-not wf
               <wf-list>
               (if ui ui <wf-ui>))
        ]

    (<ui> *STATE)
))









(comment

  [woof.blog.frontend :as blog]




(defn blog-wf [*STATE]
  (init-runner-wf! *STATE
    (blog/prepare-params! "/api/blog")
    blog/context-map-fn
    blog/steps-fn
    blog/actions-fn
    blog/ui-fn
    :auto-start true
  )
)

 ["blog" blog-wf]

)
