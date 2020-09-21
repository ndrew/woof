(ns woof.wfs.alpha
  (:require
    [woof.base :as base]
    [woof.utils :as u]
    #?(:clj [clojure.core.async :as async :refer [go go-loop]])
    #?(:cljs [cljs.core.async :as async]))

  #?(:cljs
     (:require-macros
       [cljs.core.async.macros :refer [go go-loop]]))
  )

;;
;; place for incubation of certain useful workflow and generic s-handler helpers


;;
;; linearization using worker channel loops

;; currently we assume that step handler does not returns channel


(defn _seq-worker-init
  "partial initialization fn for defining sequential worker loop with id=worker-chan-id"
  [worker-chan-id params]
  (let [chan-factory (base/&chan-factory params)
        in-chan (base/make-chan chan-factory (base/rand-sid))]

    ;; process handler one by one

    (go-loop []
      (when-let [[handler out-chan] (async/<! in-chan)]
        (let [ready-chan (handler)
              val (async/<! ready-chan)]

          ;; (.log js/console "got val. " (meta handler) val)
          ;; (async/<! (u/timeout 5000))
          (async/put! out-chan val)
          (recur)
          )
        ))

    ;; return worker-chan-id in params
    {worker-chan-id in-chan}
    ))





(defn _seq-worker-expander
  ""
  [worker-chan-id shandler-fn params els]
  (let [make-chan (fn []
                    (base/make-chan (base/&chan-factory params) (base/rand-sid)))
        worker-chan (if-let [v (get params worker-chan-id)]
                      v
                      (u/throw! (str "no worker channel '" worker-chan-id "' provided in the params")))

        ]

    (reduce (fn [a e]
              (let [outbound-chan (make-chan)

                    _handler (fn []
                               (let [res (shandler-fn e)]
                                 (if (u/channel? res)
                                   res
                                   (let [c (make-chan)]
                                     (go
                                       (if res
                                         (async/put! c res)
                                         (async/put! c :nil)))
                                     c
                                     )
                                   )
                                 ))

                    handler-fn (with-meta _handler {:v e})
                    ]
                (async/put! worker-chan [handler-fn outbound-chan])

                (assoc a (base/rand-sid) [:v outbound-chan])
                ))
            (array-map) els)
    )
  )


(defn _seq-worker-handler
  ""
  [worker-chan-id shandler-fn params v]
  (let [
        make-chan (fn [] (base/make-chan (base/&chan-factory params) (base/rand-sid)))
        worker-chan (if-let [v (get params worker-chan-id)]
                      v
                      (u/throw! (str "no worker channel '" worker-chan-id "' provided in the params")))

        _handler-fn (fn []
                      (let [c (make-chan)]
                        (go
                          (if-let [r (shandler-fn v)]
                            (async/put! c r)
                            ))
                        c))

        handler-fn (with-meta _handler-fn {:v v})
        ]

    (let [outbound-chan (make-chan)]
      (async/put! worker-chan [handler-fn outbound-chan])

      outbound-chan
      )
    )
  )



