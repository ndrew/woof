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


(defn do-watcher-chan-init [watcher-id ch DATA* params]
  ;; todo: throw exception if watcher-is is qualified keyword? as it will break [:watch WATCHER-ID]

  (async/put! ch @DATA*)
  (add-watch DATA*
             watcher-id
             (fn [key atom old-state new-state]

               ;; #?(:cljs (.log js/console "upd:" watcher-id key (= old-state new-state)))

               (if (not= old-state new-state)
                 (async/put! ch new-state)
                 )
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
  (do-watcher-chan-init WATCHER-ID
                        (base/make-chan (base/&chan-factory params) (base/rand-sid))
                        *state params))


(defn _watcher-cf-init-cb [WATCHER-ID *state cb params]
  (let [
        cf (base/&chan-factory params)
        ; old impl
        ; ch (base/make-chan cf (base/rand-sid))

        ;; as watcher can receive lots of updates - use non-default chan implementation
        ch (base/own-chan cf (base/sid) (async/chan (async/sliding-buffer 72)))
        ]

    ;; why this is not properly working with mult
    (let [nu-state' @*state
          nu-state (if (nil? nu-state') (reset! *state {}) nu-state')]

      (cb *state nu-state)
      (async/put! ch nu-state))

    (add-watch *state
               WATCHER-ID
               (fn [key atom old-state new-state]

                 ;; #?(:cljs (.log js/console "WATCHER UPD:" WATCHER-ID key (= old-state new-state)))

                 (when (not= old-state new-state)
                   (cb *state new-state)
                   (async/put! ch new-state)
                   )
                 ))
    {
     ::watchers (merge {
                        WATCHER-ID [ch *state]
                        } (get params ::watchers {}))
     }
    )
  )



(def watcher-opts (base/build-opt-on-done
                    (fn [params result]
                      (doseq [[watcher-id [_ *DATA]] (get params ::watchers)]
                        (remove-watch *DATA watcher-id))

                      result
                      )))