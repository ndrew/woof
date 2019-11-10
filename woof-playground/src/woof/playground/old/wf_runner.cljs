(ns woof.playground.old.wf-runner
  (:require
    [cljs.core.async :as async]
    [clojure.data :as cd]

    [rum.core :as rum]

    [woof.data :as d]

    [woof.base :as base]

    [woof.wf :as wf]
    [woof.wf-data :as wdata]
    [woof.playground.old.wf-ui :as wf-ui]

    [woof.ui :as ui]

    [woof.playground.old.context :as ctx-ui]
    [woof.playground.old.steps :as steps-ui]
    [woof.playground.old.results :as r]

    [woof.utils :as u]

    ;; wf examples
    ;; ui:
    [woof.playground.old.example.ui-loop :as ui-loop]
    [woof.playground.old.example.popup :as popup]
    ;; server:
  ;    [woof.playground.old.example.files-ui :as files-ws]
  ;    [woof.playground.old.example.ws :as ws]
    ;; basic wfs:
    [woof.playground.old.example.ouroboros :as ouroboros]
    [woof.playground.old.example.infinite :as infinite]
    [woof.playground.old.example.big-wf :as big-wf]


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
;; main state atom for workflow runner

(declare default-ui-fn)
(declare init-runner-wf!)

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



;; ui wfs

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



#_(defn ws-wf [*STATE]
  (init-runner-wf! *STATE
                   (ws/prepare-params! "/api/websocket")
                   ws/context-map-fn
                   ws/steps-fn
                   ws/actions-fn
                   (partial default-ui-fn "websocket RPC")
                   )
  )


#_(defn files-ws-wf [*STATE]
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
                                       ["ouroboros" ouroboros-wf]
                                       ["infinite" infinite-wf]
                                       ["expand" expand-wf]
                                       ]
                      :complex-workflows [
                                          ["UI loop" ui-loop-wf]
                                          []
                                          ;["file browser" files-ws-wf]
                                          []
                                          ["popup"   popup-wf ]
                                          []
                                          ;["rpc via webservice" ws-wf]
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




(defn init!
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

                             (reset! (cursor [:status]) :woof.app/running))]

                     (let [v (if wf-start-fn (wf-start-fn))]
                       (if (u/channel? v)
                         (go
                           (if-let [nu-v (async/<! v)]
                             (f)))
                         (f)))))

        ;; ends wf, does wf specific clean-up
        stop-fn (fn[]

                  (base/end! xtor)

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
                                                             (reset-fn)
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

        context-map (apply context-fn args)
        steps (apply steps-fn args)

        xtor (wf/build-executor (wf/make-context context-map) steps)

        wf-actions (runner-actions *STATE (apply actions-fn args) xtor)]


    (swap! *STATE merge
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
