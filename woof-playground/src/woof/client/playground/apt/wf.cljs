(ns woof.client.playground.apt.wf
  (:require
    [cljs.core.async :as async]
    [clojure.string :as str]
    [clojure.set :as set]

    [rum.core :as rum]

    [woof.base :as base]
    [woof.data :as d]
    [woof.data.core :as data]
    [woof.utils :as utils]

    [woof.browser :as woof-browser]

    [woof.client.dom :as woof-dom]
    [woof.client.playground.ui :as pg-ui]
    [woof.client.playground.ui.wf :as wf-ui]
    [woof.client.ws :as ws]

    [woof.wfs.evt-loop :as evt-loop]

    [clojure.set :as set]
    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


(rum/defcs <WF> < rum/reactive
           [local *wf]

           (let [wf @*wf
                 not-started? (= :not-started (:status wf))]
             [:div.wf-root
              (if not-started?
                [:div "WF is not running."]
                (let [*data (rum/cursor-in *wf [:state ::data])]
                  (try
                    ;; your wf is here
                    [:div
                     (pg-ui/menubar "KV"
                                    [["post" (fn []

                                               (let [k (d/to-primitive (js/prompt "k=" "k"))
                                                     v (d/to-primitive (js/prompt "v=" "v"))
                                                     ]
                                                 (ws/POST "http://localhost:8081/kv/put" (fn [])
                                                          {:k k
                                                           :v v
                                                           }
                                                          )

                                                 )


                                               )]]
                                    )
                     ]
                    (catch js/Error e [:pre (pr-str e)]))))
              ]))


;;;;;;;;;;;;;;;;;;;;

;;
;; WF definition
(defn wf! [*SWF]

  (let [CHAN-FACTORY (base/chan-factory (atom {}))
        *dict (rum/cursor-in *SWF [:state ::data])]
    {

     :title       "apartment dashboard"
     :explanation [:div.explanation
                   [:p "apartment dashbord"]]

     ;; this state will be added to a wf?
     :state {
             ::data {
                     ;; just pile of data
                     }
             }

     :init-fns    [
                   { ::*data *dict }

                   (base/build-init-chan-factory-fn CHAN-FACTORY)
                   (evt-loop/build-evt-loop-init-fn (base/make-chan CHAN-FACTORY (base/rand-sid "evt-")))
                   ]
     ;;
     :ctx-fns     [
                   evt-loop/evt-loop-ctx-fn
                   ;; re-use common browser step handlers
                   woof-browser/common-ctx
                   woof-dom/dom-ctx
                   ]
     ;;
     :steps-fns   [
                   (fn [params] { ::#EVT-LOOP# [:evt-loop (evt-loop/&evt-loop params)]})
                   (partial woof-browser/_add-style-once-steps-fn "http://localhost:9500/css/apt.css")
                   ]

     :opt-fns     [
                   (base/build-opts-chan-factory-fn CHAN-FACTORY)
                   ;; uncomment to display wf results (esp. if there was an error)
                   (base/build-opt-on-done (fn [params result]
                                             (.warn js/console params result)))
                   ]

     :ui-fn       (partial wf-ui/<wf-UI> (partial <WF>))


     ;; dev stuff
     :playground/keys-to-update-on-reload [
                                           :actions
                                           :title
                                           :explanation
                                           :wf-actions
                                           ;; overwrite ui
                                           :ui-fn

                                           ;; update/overwrite workflow
                                           :init-fns :steps-fns :opt-fns
                                           ;; overwrite the state
                                           ;:state
                                           ]
     })
  )
