(ns woof.prototype.prototype1
  (:require
    ; fs
    [clojure-watch.core :refer [start-watch]]
    [me.raynes.fs :as fs]

    [clojure.core.async :as async :refer [go go-loop]]

    [woof.base :as base]
    [woof.data :as d]
    [woof.utils :as u]

    [woof.common :as common]

    [clojure.core.async :as async]

    ))


;; clojure wf, prototype 1
;;   file watcher wf (for file)



(defn print-results-opts-fn [params]
  {
   :after-process   (fn [exec-chann]
                      (.println *err* (str "\nAFTER PROCESS:\n"))

                      exec-chann)

   :op-handlers-map {
                     :done  (fn [result] (.println *err* (str "RESULT:\n" (d/pretty result) "\n")))
                     :error (fn [result] (.println *err* (str "ERROR:\n" (d/pretty result) "\n")))
                     }

   })


;; parameterizable function for watching changes to a file
(defn build-file-watcher [store-close-fn ; (fn[]) - callback that will stop watcher
                          on-change-fn   ; (fn[chan event path])
                          file-path]
  (let [f (fs/absolute file-path)
        abs-path (.getPath f)
        watch-dir (.getPath (fs/parent f))]

    (let [chan (async/chan)
          watch-config {
                        :path        watch-dir
                        :event-types [:create :modify :delete]
                        :bootstrap   (fn [path]
                                       (println "Starting to watch " path))
                        :callback    (fn [event filename]
                                       ;; is it efficient way of doing this
                                       (when (= abs-path filename)
                                         (on-change-fn chan event filename))
                                       ;(println event filename)
                                       )
                        :options     {:recursive false}}]

      (let [close-watcher (start-watch [watch-config])]
        (store-close-fn close-watcher))

      chan
      )
    )
  )


(defn common-ctx-fn [_]
  {
   ;; print v
   :print      {:fn (fn [v]
                      (.println *err* (str "\n" (pr-str v) "\n"))
                      v)}

   ;; read text contents of the file
   :read-text! {:fn (fn [file-map]
                      ;; todo: encoding
                      (let [path (:path file-map)]
                        (slurp path))
                      )}
   :EDN  {
          :fn identity
          }

   })


;; IN: path (of a file to watch)
;;
;; init:
;; ctx:
;;
(defn watcher-wf [ctx-fn steps-fn]
  (let [*state (atom {:fs-watchers []})

        init-fns  [(fn [_] { :state *state })]
        ctx-fns   [common-ctx-fn ctx-fn]
        steps-fns [steps-fn]

        on-stop (fn []
                  (prn "stopping filesystem watchers")
                  (let [stop-fns (get-in @*state [:fs-watchers])]
                    (doseq [stop-fn stop-fns]
                      (println stop-fn)
                      (stop-fn)
                      )
                    )
                  )

        opt-fns [(common/build-opt-keep-xtor *state)
                 (common/build-opt-on-done on-stop)
                 print-results-opts-fn]


        wf (base/parametrized-wf!
             (base/combine-init-fns init-fns)
             identity ;wf-params-fn
             identity ;opt-params-fn

             (base/combine-fns opt-fns :merge-results base/merge-opts-maps)
             (base/combine-fns ctx-fns)
             (base/combine-fns steps-fns))

        ]

    (common/stateful-wf *state wf on-stop)
    )
  )




(defn infinite-ctx-fn [params]
  (let [*state (:state params)]
  ;; infinite step handler, return changes over the channel
  {
   :watch! {:fn       (partial build-file-watcher
                               (fn [close-watcher] (swap! *state update-in [:fs-watchers] conj close-watcher))
                               (fn [chan event filename]
                                 (async/put! chan
                                             {:path         filename
                                              :triggered-by event}
                                             ))
                               )
            :infinite true
            }

   }
    )
  )

;;

;; watcher WF with infinite step as watcher
#_(let [wf (watcher-wf infinite-ctx-fn
                       {
                        ::changes   [:watch! "/Users/ndrw/m/woof-new/woof-playground/watch.txt"]
                        ::content   [:read-text! ::changes]
                        ::debug-out [:print ::content]

                        })]
  ((:start-wf! wf))

  (async/thread
    (Thread/sleep 20000)

    (println "closing")
    ((:stop-wf! wf)))
  )



;;
;; infinite expand/collect version
;;   history of change
;;   last change can be converted to a standard infinite step handler
;;
;; ? merging two sources?
(defn infinite-expand-ctx-fn [params]
  (let [*state (:state params)]
    {

     :last-change {:fn (fn [all-changes]
                         (last all-changes))
                   :collect? true
                   }

     :changes-count {:fn (fn [all-changes]
                         (let [c (count all-changes)]
                           (prn c)
                           c
                           )
                         )}

     :watch!* {:fn (partial build-file-watcher
                                           (fn [close-watcher] (swap! *state update-in [:fs-watchers] conj close-watcher))
                                           (fn [chan event filename]
                                             (async/put! chan
                                                         {(base/rand-sid) [:EDN {:path         filename
                                                                                 :triggered-by event}]}
                                                         ))
                                           )
                              :infinite true
                              :expands? true
                              }


     }
    )
  )


#_(let [wf (watcher-wf infinite-expand-ctx-fn
                     (fn [_]
                       {
                        ::changes*    [:watch!* "/Users/ndrw/m/woof-new/woof-playground/watch.txt"]

                        ::last-change [:last-change ::changes*]
                        ::content     [:read-text! ::last-change]
                        ::hello       [:print ::content]

                        ::count      [:changes-count ::changes*]
                        }
                       ))]
  ((:start-wf! wf))

  (async/thread
    (Thread/sleep 20000)

    (println "closing")
    ((:stop-wf! wf)))
  )


;; how to combine initial read step with changes step

;; same example, but for add-watcher

;; this example but as ws




