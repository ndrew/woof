(ns ^:figwheel-hooks woof.playground.wf.example
  (:require
    [cljs.core.async :as async]

    [woof.utils :as utils]
    [woof.test-data :as td]
    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; stateful workflow example

(defn swf-ctx-fn [params]
  {
   :v      {
            :fn identity
            }
   :expand {:fn       (fn [xs]
                        (into (array-map)
                              (map-indexed (fn [i x]
                                             [(keyword (str *ns* "/" i)) [:v x]]) xs)))
            :expands? true
            }
   :wait   {
            :fn (fn [v]
                  (let [chan (async/chan 1)]
                    (go
                      (async/<! (utils/timeout 3000))
                      (async/put! chan v)
                      )

                    chan)
                  )
            }
   })


(defn swf-steps-fn [params]
  {
   ::test   [:v "this is a test"]
   ::wait   [:wait "for testing ui state"]
   ::result [:v ::wait]

   ::expand-test [:expand ["a" "b" "c"]]
   }
  )

;;

(defn test-expand-wf [n]
  (let [{ctx :context
         steps :steps
         ;; todo: channel factory
         }
        (binding [td/*new-chan!* (fn []
                                   (.log js/console "new chan")
                                   (async/chan)
                                   )]
          (td/get-test-steps-and-context n)
          )
        ]
    {
     :ctx-fns   [(fn [params] ctx)]
     :steps-fns [(fn [params] steps)]
     }
    )
  )

