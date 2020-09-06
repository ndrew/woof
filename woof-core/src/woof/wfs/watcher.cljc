(ns woof.wfs.watcher
  (:require
    [woof.base :as base]
    [woof.utils :as u]
    #?(:clj [clojure.core.async :as async :refer [go go-loop]])
    #?(:cljs [cljs.core.async :as async]))

  #?(:cljs
     (:require-macros
       [cljs.core.async.macros :refer [go go-loop]]))
  )

;; atom watcher via init


(defn _watcher-chan-init [watcher-id ch DATA* params]
  ;; todo: throw exception if watcher-is is qualified keyword? as it will break [:watch WATCHER-ID]

  (async/put! ch @DATA*)
  (add-watch DATA*
             watcher-id
             (fn [key atom old-state new-state]

               (if (not= old-state new-state)
                 (async/put! ch new-state))
               ))
  {
   ::watchers (merge {
                      watcher-id [ch DATA*]
                      } (get params ::watchers {}))
   }
  )

(defn watcher-ctx [params]
  {:watch {:fn       (fn [watcher-id]
                       ;; store channel and data*
                       (get-in params [::watchers watcher-id 0]))
           :infinite true
           }}
  )


(defn _watcher-cf-init [WATCHER-ID *state params]
  (_watcher-chan-init WATCHER-ID
                      (base/make-chan (base/&chan-factory params) (base/rand-sid))
                      *state params))


(def watcher-opts (base/build-opt-on-done
                    (fn [params result]
                      (doseq [[watcher-id [_ *DATA]] (get params ::watchers)]
                        (remove-watch *DATA watcher-id))

                      result
                      )))