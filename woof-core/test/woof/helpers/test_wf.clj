(ns woof.helpers.test-wf
  (:require
    [clojure.core.async :as async :refer [go go-loop]]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.base :as base]

    [woof.utils :as u]
))



;; run the wf
(defn async-run-wf!
  "executes the workflow and notifies done-chan when done or error.

     done-chan - channel that will receive [:done/:error <data>]
     ctx-map   - context map
     steps     - wf steps
  ; options pairs
   :prefix - for logging

  "
  [done-chan ctx-map steps & {:keys [prefix]}]
  (let [prefix (if prefix prefix "")
        xtor (wf/build-executor (wf/make-context ctx-map) steps)]
    ;; todo: use processor
    (let [exec-chann (wf/execute! xtor)]
      (go-loop []
        (let [r (async/<! exec-chann)
              [status data] r]

          (condp = status
            :init (recur)

            :error (do
                     (async/>! done-chan [:error data] )
                     (locking *out* (println prefix
                                             "\nERROR:\n"
                                             (d/pretty [:error data]))))

            :process (do
                       (recur))

            :done (do
                    (async/>! done-chan [:done data])
                    (locking *out* (println prefix
                                            "\nDONE:\n"
                                            (d/pretty [:done data])
                                            "\nRESULTS:\n"
                                            (d/pretty (base/inline-results data))
                                            )))

            (do ; other events like :wf-update
              (recur)))))

      xtor
      )
    )
  )


(defn thread-wf! [params context-fn steps-fn]
  (let [*wf (atom nil)
        thread-blocker (async/chan)
        args (apply concat params)]
    {
      :start (fn []
               (async/thread
                 (locking *out* (println (:prefix params) "STARTED THREAD"))
                 (let [
                       context-map (apply context-fn args)
                       steps (apply steps-fn args)]

                   (let [xtor (apply async-run-wf!
                                 (concat [thread-blocker
                                          context-map
                                          steps] args))]
                     (reset! *wf xtor)
                     (async/<!! thread-blocker)
                     ))))
      :stop (fn[]
              (wf/end! @*wf)
              (async/close! thread-blocker)
              (u/close-channels! args)
              )


      }
    )
  )

