(ns woof.wf.simple.wf
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.wfc :as wfc]

    [woof.utils :as u]
    [woof.ui :as ui]

    [woof.wf-ui :as wf-ui]

    [woof.ui.results :as r]

    [markdown.core :refer [md->html]])


  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


(defn context-fn [& r] ;; & {:keys []}
  (println "->CTX: " r)
  {
    :log  {:fn (fn[a]
                 (locking *out* (println "DBG:" a))
                 (identity a))}

    :log*  {:fn (fn[a]
                  (locking *out* (println "DBG:" a))
                  (identity a))
            :collect? true
            }

    })


(defn steps-fn  [& r]  ;; & {:keys []}
  (println "->STEPS: " r)
  {
    ::hello [:log "Hello!"]

    ::1 [:log* ::hello]

    })


(defn wf! [initial-params]
  (println "wf-fn: " initial-params)

  {
    :wf (fn [params]
          (wfc/params-wf params context-fn steps-fn)
          ) ;; (partial wwf in-chan> out-chan< *local) ;; wf constructor
    :params (merge initial-params {})
    }
  )
