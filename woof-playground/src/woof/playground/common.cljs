(ns ^:figwheel-hooks woof.playground.common)


(defn default-init!  ;; ;; todo: re-implement as subscription
  "initializes ui state"
  [*UI-STATE mount-fn]
   (add-watch *UI-STATE :woof-main
              (fn [key atom old-state new-state]
                (mount-fn)))

   (when-not (::initialized @*UI-STATE)
     (swap! *UI-STATE merge
            {::initialized true})))


(defn default-reload! [*UI-STATE]
  (swap! *UI-STATE merge {
                          ::initialized false
                          }))
