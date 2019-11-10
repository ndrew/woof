(ns ^:figwheel-hooks woof.playground.prototype12
  (:require
    [rum.core :as rum]

    ;; client core
    [woof.playground.v1.ui :as ui]
    [woof.data :as d]
    [woof.utils :as utils]


    [woof.playground.v1.playground :as pg]
    [woof.playground.v1.utils :refer [dstr kstr vstr]]
    [woof.playground.common :as cmn]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



(defn wf-state []
  {
   :status :prepare ; :compile
   :IN     []
   :OUT {
         :EXPORT {
                  ;; export-key {:step-id .., :value ... }
                  }
         }
   })


(defonce *UI-STATE
         (atom
           {
            ::defs  (wf-state)
            ::cfg   (wf-state)
            ::combi (wf-state)
            }))


;; --- exports

(declare <ui>)

(def init! (partial cmn/default-init! *UI-STATE))
(def reload! (partial cmn/default-reload! *UI-STATE))


;;
;; pass *wf as :wf via init params
;; pass exported step as string
;; store ctx/steps via capturing wf
;;

;;

(defn store-xtor [*wf wf-chan xtor]
  (swap! *wf assoc ::xtor xtor)
  :ok)


(defn store-results [*wf result]
  (swap! *wf assoc ::result result))

(defn update-OUT [out result]
  (let [exported (reduce (fn [a [export-k v]]
                           (let [sid (:step-id v)]
                             (assoc a export-k
                                      (if (qualified-keyword? sid)
                                        (assoc v :value (get result sid))
                                        v))
                             )
                           )
                         {} (get out :EXPORT {}))]
    (assoc out :EXPORT exported)
    )
  )

(defn done-handler [*wf result]
  (let [wf @*wf]
    (swap! *wf merge {
                    :OUT (update-OUT (get wf :OUT {}) result)

                    ::result result
                    ::status :done
                    }))
  (println "[wf]:done\n" (d/pretty result)))


(defn err-handler [*wf result]
  (swap! *wf assoc ::status :error)
  (swap! *wf assoc ::error result)
  (println "[wf]:error\n" (d/pretty result)))




(rum/defcs <debug> < rum/reactive (rum/local false ::show?)

  [{show? ::show?} *wf]
  (let [h (fn [] (swap! show? not))]
    (if @show?
      [:pre (ui/btn "..." h) "\n" (dstr (into (sorted-map) @*wf))]
      (ui/btn "..." h)
      )
    )
  )


;; whether to pass wf via params/or as parameter

(defn IN-step [k params]
  (let [IN (get params k)]
    ; if IN is null, should we wait

    (.log js/console "IN-step" k params)
    (if (nil? (:value IN))
      [:v (:step-id IN)]
      [:v (:value IN)]
      )
    )
  )

(defn defs-ctx-fn [params]
  (let [*wf (:wf params)]
    {

     :v            {:fn identity}

     ;; step handler that gets value from init-params params
     ;; :IN           {:fn (fn [k] (:value (get params k)))}

     ;;
     ;; step handler for exporting the step - for now as string, which is pretty ugly

     :export-step! {:fn       (fn [[k step]]
                                (let [step-k (keyword (clojure.string/replace-first step ":" ""))
                                      v {:step-id step-k}]
                                  (swap! *wf assoc-in [:OUT :EXPORT k] v)
                                  [k v]))
                    :collect? true
                    }
     :export-value! {:fn       (fn [[k value]]
                                  (let [v {:value value}]
                                    (swap! *wf assoc-in [:OUT :EXPORT k] v)
                                    [k v]))
                    :collect? true
                    }

     ;;
     :kv-zip*      {
                    :fn       (fn [vs]
                                ;; todo: check for correct number of vs
                                (apply hash-map vs))
                    :collect? true
                    }
     :kv-merge*    {
                    :fn       (fn [vs]
                                (apply merge vs))
                    :collect? true
                    }
     ; todo: check with infinite steps - if the resulting map will be updated
     }
    )
  )

(defn opt-fn [params]
  (let [*wf (:wf params)]
    {
     :before-process  (partial store-xtor *wf)
     :op-handlers-map {
                       :process (partial store-results *wf)
                       :done    (partial done-handler *wf)
                       :error   (partial err-handler *wf)
                       }}
    )
  )



(rum/defc <IN-row> < rum/reactive [*wf]

  [:.row [:.gutter "IN:"]
   (let [IN (get @*wf :IN [])]

     (map (fn [a]
            (let [{k :k,
                   type :type
                   wf-id :wf-id
                   wf-k :wf-k} a]

              [:.flex.in {:style {:border "1px solid crimson" :border-top "none" :padding ".5rem"}}
               [:label (pr-str k)]
               ;[:label (pr-str type)]
               [:label (str "<" (name wf-id) ">" "." (name wf-k))]

               ;(pr-str a)
               ]
              )

            ) IN)
     )
   ;"..."
   #_[:div.flex
      [:div {:style {:border "1px solid crimson" :border-top "none" :padding ".5rem"}}
       [:label "path:"] [:code " /some/path"]]

      [:div {:style {:border "1px solid crimson" :border-top "none" :border-left "none"
                     :padding ".5rem"}}
       [:label "include:"] [:code " *.*"]]

      [:div {:style {:border "1px solid crimson" :border-top "none" :border-left "none"
                     :padding ".5rem"}}
       [:label "encoding:"] [:code " UTF-8"]]
      ]
   ]
  )

(rum/defc <OUT-row> < rum/reactive [*wf]
  [:.row [:.gutter "OUT:"]
   [:div
    [:pre
     (dstr (into (sorted-map) (get-in @*wf [:OUT :EXPORT])))
     "\n"
     ]
    ]
   ;"..."
   #_[:div.flex
      [:div {:style {:border "1px solid crimson" :border-top "none" :padding ".5rem"}}
       [:label "path:"] [:code " /some/path"]]

      [:div {:style {:border "1px solid crimson" :border-top "none" :border-left "none"
                     :padding ".5rem"}}
       [:label "include:"] [:code " *.*"]]

      [:div {:style {:border "1px solid crimson" :border-top "none" :border-left "none"
                     :padding ".5rem"}}
       [:label "encoding:"] [:code " UTF-8"]]
      ]
   ]
  )



(defn get-wf-value [in]
  ; todo: check type
  (let [{wf-id :wf-id
         wf-k  :wf-k } in]
    ;; todo pass the wf
    (get-in @*UI-STATE [wf-id :OUT :EXPORT wf-k])
    )
  )



(defn collect-IN-vals [IN]
  ;; todo: how to pass IN vals in map
  (.warn js/console "collect IN" IN)
  (reduce (fn [a in]
            (let [v (get-wf-value in)]
              ; return in values as k-v here
              (assoc a (:k in) v))
            ) {} IN))


(defn defs-init-fn [params]
  (.log js/console "INIT: defs")
  {
   ;;
   :defs {
          :value {
              :title "default title"
              :t (utils/now)
            }
          }
   }
  )




(defn defs-steps-fn [params]
  (let [*wf (:wf params)]
    {
     ;; get the value from IN
     ;; this is exported step, so we are using certain domain
     :domain/DEFAULTS     (IN-step :defs params)

     ;; use internal domain ns to avoid further collisions

     ;; declare export key & export key
     :defs/export-k           [:v :DEFAULT-CONFIGURATION]
     :defs/export-step-k      [:v (str :domain/DEFAULTS)]

     ;; export the step, and take the results after wf is done
     :defs/export-step! [:export-step! [:defs/export-k :defs/export-step-k]]

     :defs/export-value-k     [:v :TEST-VALUE-EXPORT]
     :defs/export-value       [:v "exported as value"]

     ;; export some value
     :defs/export-value! [:export-value! [:defs/export-value-k :defs/export-value]]

     }
    )

  )


(defn cfg-init-fn [params]
  (.log js/console "INIT: cfg")
  (let [*wf (:wf params)
        IN (get @*wf :IN [])
        in-params (collect-IN-vals IN)]

    ;; we have to tell steps generation function
    ;; whether to use IN value or step
    ;; todo: figure out standard way of specifying thes

    ; (.warn js/console "IN-PARAMS" in-params)
    in-params
    )

  )

(defn cfg-steps-fn [params]
  ;; todo: how to use exported keys here?
  (.log js/console "cfg: steps")
  (let [*wf (:wf params)]
    {
     ::defaults    (IN-step :defaults params)

     ::k1          [:v :title]
     ::v1          [:v "new title"]
     ::zip         [:kv-zip* [::k1 ::v1]]

     :domain/cfg   [:kv-merge* [::defaults ::zip]]

     :cfg/export-k [:v :cfg]
     :cfg/cfg-k    [:v (str :domain/cfg)]

     :cfg/export   [:export-step! [:cfg/export-k :cfg/cfg-k]]
     }
    )

  )


;;
(rum/defc <defs-wf> < rum/reactive [*wf cfg]
  (let [run (fn []
                (pg/ui-wf *wf cfg))
        ]
    [:div.wf
      (ui/menubar "Defaults WF" [["run" run]])
       (<IN-row> *wf)
       (<debug> *wf)
       (<OUT-row> *wf)
     ]
    ))


(rum/defc <cfg-wf> < rum/reactive [*wf cfg]
  (let [run (fn []
              (pg/ui-wf *wf cfg))]

    [:div.wf
     (ui/menubar "Config WF" [
                              ["init" (fn []
                                        (let [IN (:IN @*wf)]
                                          (swap! *wf update-in [:IN] conj {
                                                                           :k :defaults
                                                                           :type :wf-v
                                                                           :wf-id ::defs
                                                                           :wf-k :DEFAULT-CONFIGURATION
                                                                           })
                                          )
                                        )]
                              ["run" run]

                              ])

     (<IN-row> *wf)
     (<debug> *wf)
     (<OUT-row> *wf)
     ]
    )

  )


(rum/defc <combi-wf> < rum/reactive [*wf cfg]

  (let [wf @*wf
        err (::error wf)
        run (fn []
              (pg/ui-wf *wf cfg))]

    [:div.wf
     (if err
       [:pre (pr-str err)]
       )

     [:p
      "handling in parameters for composed wf: as in step is being added during steps-fn, it's not available during init-fn"
      ]
     [:p
      "for now combining is NOT working"
      ]

     (ui/menubar "COMBI WF" [
                              ["init" (fn []
                                        (let [IN (:IN @*wf)]
                                          (swap! *wf update-in [:IN] conj {
                                                                           :k :defaults
                                                                           :type :wf-v
                                                                           :wf-id ::combi ; important
                                                                           :wf-k :DEFAULT-CONFIGURATION
                                                                           })
                                          )
                                        )]
                              ["run" run]
                              ])

     (<IN-row> *wf)
     (<debug> *wf)
     (<OUT-row> *wf)
     ]
    )

  )

(rum/defc <ui> < rum/reactive [*STATE]
  (let [
        *defs (rum/cursor-in *STATE [::defs])
        *cfg (rum/cursor-in *STATE [::cfg])
        *combi (rum/cursor-in *STATE [::combi])
        ]
    [:div.proto10
     [:p "2 wf example: one wf is the default config, other overrides config"]
     (<defs-wf> *defs {
                       :init-fns [defs-init-fn]
                       :ctx-fns [defs-ctx-fn]
                       :steps-fns [defs-steps-fn]
                       :opt-fns [opt-fn]
                       })
     (<cfg-wf> *cfg {
                     :init-fns [cfg-init-fn]
                     :ctx-fns [defs-ctx-fn]
                     :steps-fns [cfg-steps-fn]
                     :opt-fns [opt-fn]
                     })

     (<combi-wf> *combi
                 {
                  :init-fns  [
                              (fn [params]
                                (.log js/console "INIT: export")

                                (let [*wf (:wf params)
                                      IN (get @*wf :IN [])
                                      in-params (collect-IN-vals IN)
                                      EXPORT (get @*wf :EXPORT {})
                                      ]
                                  ;; check whether there are all needed keys with values
                                  (if (= {} in-params)
                                    (utils/throw! "Forgot to press init!"))

                                  ; get exported keys
                                  (.warn js/console IN)

                                  ; (prn EXPORT)
                                  (assoc params :EXPORT EXPORT)
                                  )
                                )

                              cfg-init-fn
                              defs-init-fn
                              ]
                  :ctx-fns   [defs-ctx-fn]
                  :steps-fns [cfg-steps-fn defs-steps-fn]
                  :opt-fns   [
                              (fn [params]
                                (.log js/console "steppps!")
                                {}
                                )

                              opt-fn]

                  }
                 )
     ]))

;; root ui component for prototype
(def <app> #(<ui> *UI-STATE))

;; TODO: ui fn