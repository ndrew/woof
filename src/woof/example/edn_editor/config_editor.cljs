(ns woof.example.edn-editor.config-editor
  (:require
    [cljs.core.async :as async]
    [woof.data :as d]
    [woof.wf :as wf]
    [woof.wfc :as wfc]
    [woof.xform :as x]
    [woof.utils :as u])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))

;;;;;;;;;;;;;;;;;
;;
;; context
;;
;;   todo: add steps

(defn context-fn [& {:keys [*local send! in-chan>]}]

  {

    :log  {:fn (fn[a]                                     ;; debug
                 (locking *out* (println "DBG:" a))
                 (identity a))}

    :send! {:fn (fn[a]
                  (send! a)
                  )}

    :ui-loop  {
                          :fn (x/global-shandler
                                (x/infinite-expand-rf (fn
                                                        ([] in-chan>) ;; return channel

                                                        ([steps]
                                                         (locking *out* (println "new commands: " (d/pretty steps)))
                                                         steps)

                                                        ([in-chan out-chan]

                                                         (locking *out* (println "closing :wait-for-commands" in-chan out-chan))
                                                         ;; when this is called

                                                         )
                                                        )))
                          :infinite true
                          :expands? true
                          }


    }
  )

;;;;;;;;;;;;;;;;;
;;
;; steps
;;

(defn steps-fn [& {:keys [initial-steps]}]
  {
    ;; infinite loop
    ::main-loop [:ui-loop initial-steps]

    ;;
    ::hello [:log "hello!"]

    ;; test sending value to server
    ::send [:send! "Yo"]
    })



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; workflow constructor


(defn wwf [in-chan> out-chan< *local  ;; <- these should be partially applied ;; <?> why these are not in params?
           params]

  (let [defaults {
                   :in-chan> in-chan>
                   :out-chan< out-chan<
                   :*local *local
                   }
        all-params (merge params ;; pass the in/out channel in case they'll be needed in context or steps constructors
                          defaults)]

    (wfc/params-wf all-params context-fn steps-fn)))




;; constructor


(defn wf! [*local]

  ;; inits necessary channels/resources for wf

  (let [in-chan> (async/chan)
        out-chan< (async/chan)

        receive-fn (fn [msg]
                     (println "SERVER RECEIVE:\n" (pr-str msg) "\n")

                     ;;
                     ;; =>  uncomment for adding actual steps
                     ;;
                       #_(go
                         (async/put! in-chan> msg))

                     )
        close-fn (fn [status]
                   (async/close! in-chan>)
                   (async/close! out-chan<)
                   )
        ]

    {
      :wf (partial wwf in-chan> out-chan< *local)
      :receive-fn receive-fn
      :close-fn close-fn

      :actions [] ;; todo: add some actions
    }
    )
  )


