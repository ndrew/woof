(ns woof.state.core
  "woof state mgmt"
  (:require
    #?(:cljs [cljs.pprint :as cljs-pprint])
    #?(:cljs [cljs.reader])

    #?(:clj [clojure.pprint :as clj-pprint])))

;; state for workflows

(defonce
  *STATE (atom {
              ;; :wf-id {}
                }))

(defn add-wf [wf-id wf]
  (swap! *STATE assoc wf-id wf)
  )




