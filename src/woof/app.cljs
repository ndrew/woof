(ns woof.app
  (:require
    [cljs.core.async :as async]
    [cljs.test :as test :refer-macros [deftest is testing run-tests async]]

    [goog.string :as gstring]
    [goog.string.format]

    [rum.core :as rum]

    [woof.app-data :as app-model]

    [woof.data :as d]
    [woof.graph :as g]
    [woof.wf :as wf]
    [woof.ws :as ws]

    [woof.ui :as ui]
    [woof.wf-ui :as wf-ui]
    [woof.utils :as u]

    [woof.test-data :as test-data]

    ; [woof.wf-tester-ui :as tester-ui]
    [woof.ui.wf-runner :as runner]

    [woof.ui.results :as r]
    )


  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]
    [woof.utils-macros :refer [put!?]]))


;;
;; init

(enable-console-print!)


(defn init-state
  "provides initial state map"
  []
  {
      ;; workflow context
      :context (app-model/default-context)

      ;; workflow definition
      :workflow {
                  :name "TEST WORKFLOW"
                  :steps (array-map) ; (assoc (array-map) ::woof [:identity "test wf!"])
                  }


      ;; step handler to editor mapping
      ;:editors {:pre   {}
      ;          :post {}}

      :xctor nil
      :xctor-chan nil
      }
  )

(defonce *APP-STATE (atom (init-state)))



(def cursor (partial rum/cursor-in *APP-STATE))

(defn first-init-state
  "returns state to be merged to "
  []
  {
    ;; this intentionaly left blank
     }
)








(defn gen-new-wf-f! [N]
  (fn[]
    (let [{
          test-context :context
          test-steps :steps
          } (test-data/get-test-steps-and-context N)]
    (swap! *APP-STATE assoc-in [:context] test-context)
    (swap! *APP-STATE assoc-in [:workflow :steps] test-steps))))










;; todo: add css animation on value update


(defn done-percentage [result actual-steps]

  ;; todo

  (if (map? result)
    (* 100 (/ (reduce (fn [acc [k v]]
                                   (+ acc
                                      (if (u/channel? v)
                                        0
                                        1)
                                      )
                                )
                              0 result
                              )
                      (count actual-steps)))
    (do
      #_(.warn js/console (d/pretty
                          [actual-steps result]))
      0))

)



(rum/defc <wf-progress-ui>   <   { :key-fn (fn [start _] start)} ;; todo: make it timer based
  "shows workflow progress status - time elapsed, done percentage"

  [start-t percent-done]
  [:span (str (gstring/format "%.2f" percent-done) "% " (- (u/now) start-t) "ms.   ")])









;; todo: remove this to op-map

(defonce *backpressure-cache (atom nil) )
(defonce *backpressure-t (atom 0) )


(defn workflow-handler [*result r]

  (let [[status data] r
        done? (= :done status)]



    (when (= :error status)
     (.warn js/console r)

      (swap! *result assoc-in [::wf-status] ::error)
      (swap! *result assoc-in [::result] data))



    #_(when (and
            (= :back-pressure status)
            (nil? @*backpressure-cache))

      ; (println "GOT backpressure")
      (reset! *backpressure-t (u/now))
      (reset! *backpressure-cache @*result)
      )



    ;; backpressure for react rendering

    (let [*resulting-map (if (nil? @*backpressure-cache)
                           (do *result)
                           (let [bp-time (- (u/now) @*backpressure-t)]
                             ;(< (- (u/now) @*backpressure-t) 1000)
                             ;(println bp-time)

                             (if (> bp-time 1000)
                               (do
                                 (reset! *result @*backpressure-cache)
                                 (reset! *backpressure-cache nil)
                                 *result)
                               *backpressure-cache
                               )
                             )
                           )]

      (when (= :expand status)
        (let [[x-id nu-steps] data]
          ;; todo: check if this breaks done-percentage
          (swap! *resulting-map update-in [::steps] merge nu-steps)
          (swap! *resulting-map assoc-in [::result] data)
          ))

      (when (= :process status)
        (swap! *resulting-map assoc-in [::result] data))

      (when (= :wf-update status)
        (swap! *resulting-map assoc-in [::steps] (first data))
        (swap! *resulting-map assoc-in [::result] (second data)))

      )



    (when done?
      (swap! *result assoc-in [::wf-status] ::done)
      (swap! *result assoc-in [::result] data))

    (swap! *result update-in [::history] conj r)

    (not done?)))



(defn- pipe-fn [editor-chan s]
  (let [c (async/chan)]
    (go-loop []
             (let [v (async/<! editor-chan)] ; read from preview chan
               (async/put! c v)
               (recur)))
    c))




;; state changing fns
;; todo: rework the IWFUi
(defprotocol IWFUi
  (get-status [this])

  (set-status [this status])


  (merge-result [this data])

  (get-result* [this])

  (add-post-editor [this k v])

  (add-pre-editor [this k v])
  )



(defn make-ui-state [local]
  ; (::result local)

  (reify IWFUi
    (get-status [this] @(::status local))
    (set-status [this status] (reset! (::status local) status))

    (merge-result [this data]
                  (swap! (::result local) merge data))

    (get-result* [this] (::result local))

    (add-post-editor [this k v]
      (swap! (::editors local) update-in [:post] assoc k v))

    (add-pre-editor [this k v]
      (swap! (::editors local) update-in [:pre] assoc k v))
    )
  )





;; menu items


;;
(defn- ajax-step-mi [model UI-STATE]

  ["ajax wf"
   (fn[]
     ;; ws/make-ajax-handler

     (app-model/merge-context model
                              {:ajax {:fn ws/transit-handler}})

     (app-model/merge-steps model {
                                      ::test-ajax  [:ajax (ws/resolve-url "/ajax")]
                                      })

     )
   ]
  )





(comment
  [:div.graph
   {:dangerouslySetInnerHTML
    {:__html (g/graph-to-svg steps (fn [gviz [k [action param]]]
                                     (if (wf/sid? param)
                                       (str gviz " "
                                            (clojure.string/replace (name param) #"-" "_")
                                            " -> "
                                            (clojure.string/replace (name k) #"-" "_")
                                            ";\n")
                                       gviz
                                       )
                                     ))}}]
  )







;;;


(defn ui-state
  "provides state map atom for the ui updates"
  []

  ;; *APP-STATE

  runner/*UI-STATE
)

(defn init-state!
  "initializes first ui update, if needed"
  []

  (when-not (::initialized @*APP-STATE)
    (swap! *APP-STATE merge (first-init-state) {::initialized true})
    (runner/init!))

  )




;; root ui component
(rum/defcs <app-ui>
  < rum/reactive [local *STATE]

  [:div#app (runner/<wf-runner-ui> *STATE)]
)





(defn watch-ui-state!
  "adds watcher to a ui state map"
  [f]
  (add-watch (ui-state) :ui
             (fn [key atom old-state new-state]
               (f))))


;;
;; mount the application

(let [el (. js/document (getElementById "app"))
      app-fn #(<app-ui> (ui-state))
      mount-app #(rum/mount (app-fn) el)]

  (watch-ui-state! mount-app)
  (init-state!)

  (defn on-js-reload []
    (mount-app)) ;; re-mount app on js reload

)


