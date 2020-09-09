(ns ^:figwheel-hooks woof.playground.core
  (:require
    [cljs.reader]
    ;; core async
    [cljs.core.async :as async]

    [goog.string :as gstring]
    [goog.string.format]

    [rum.core :as rum]

    ;[woof.u :as u]
    [woof.utils :as u]

    [woof.playground.old.app-data :as app-model]
    [woof.playground.old.wf-runner :as runner]

    [woof.test-data :as test-data]
    [woof.data :as d]

    ;[viz.core :as viz]

    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))

;; initial version of woof playground




;; --- exports

(declare <ui>)
(declare init!)
(declare reload!)


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



(defn done-percentage [result actual-steps]
  ;; todo: add css animation on value update

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

      (swap! *result assoc-in [::wf-status] :error)
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
      (swap! *result assoc-in [::wf-status] :done)
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



(comment
  (defn graph-to-svg [steps rfn]
    (let [graphviz-edges (reduce rfn "" steps)]
        (viz/image (str "digraph { " graphviz-edges " }")))
    )


  [:div.graph
   {:dangerouslySetInnerHTML
    {:__html (graph-to-svg steps (fn [gviz [k [action param]]]
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

  [:div

   [:pre
    "app state:\n"
    (d/pretty @*STATE)]

   (runner/<wf-runner-ui> (ui-state))]
  )





(defn watch-ui-state!
  "adds watcher to a ui state map"
  [f]
  (add-watch (ui-state) :ui
             (fn [key atom old-state new-state]
               (f))))





;; todo: re-implement as subscription
(defn init!
      "initializes ui state"
      ([]
       ; todo: is this needed
       ; (prn "INIT")
       )
      ([mount-fn]

       (watch-ui-state! mount-fn)
       (init-state!)

       )
      )


(defn reload! []
      ;(remove-watch *UI-STATE :woof-main)
      (swap! *APP-STATE merge {
                              ::initialized false
                              }))



;; main ui


(def <app> #(<app-ui> *APP-STATE))

