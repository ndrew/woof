(ns ^:figwheel-hooks woof.playground.prototype10
  (:require
    [cljs.core.async :as async]
    [rum.core :as rum]

    ;; client core
    [woof.base :as base]
    [woof.playground.v1.ui :as ui]
    [woof.data :as d]

    [woof.wfc :as wfc
     :refer [WoofWorkflow
             get-params
             get-context-map
             get-steps]
     ]
    [woof.playground.common :as cmn]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



(defonce *UI-STATE (atom {
                          ::wf {}

                          :wf1 {
                                ::status :prepare
                                ::history []
                                }
                          }))


;; --- exports

(declare <ui>)

(def init! (partial cmn/default-init! *UI-STATE))
(def reload! (partial cmn/default-reload! *UI-STATE))

;;


(defn capturing-WF [*wf nu-params context-map-fn steps-fn wf-params]
  ;; todo: how to use here your ::ctx and ::steps - substitute via methods
  (swap! *wf assoc ::init-params nu-params)
  ;(js-debugger)
  (reify WoofWorkflow
    (get-params [this] nu-params) ;; is this really needed
    (get-context-map [this] (let [ctx-map (context-map-fn nu-params)]
                              (swap! *wf assoc ::ctx ctx-map)
                              ctx-map))
    (get-steps [this] (let [steps (steps-fn nu-params)]
                        (swap! *wf assoc ::steps steps)
                        steps
                        )))
  )


(defn run-wf! [*wf & wfs]
  (let [empty-cfg {
                   :init-fns []
                   :ctx-fns []
                   :steps-fns []
                   :opt-fns []
                   }
        resulting-cfg (reduce (fn [a cfg]
                                (merge-with into a cfg))
                              empty-cfg wfs)
        {init-fns :init-fns
         ctx-fns :ctx-fns
         steps-fns :steps-fns
         opt-fns :opt-fns
         } resulting-cfg
        ]
    (let [
          ; latest fn that returns params
          opt-params-fn (fn [params]
                          ;; maybe store params here
                          ;;(.log js/console "params" params)
                          ;(swap! *wf assoc ::params params)
                          params
                          ) ;; transforms wf params to opt params

          wf (base/parametrized-wf!
               (base/combine-init-fns init-fns)
               ;; transforms initial map to a wf params
               identity
               opt-params-fn

               (base/combine-fns opt-fns :merge-results base/merge-opts-maps)
               (base/combine-fns ctx-fns)
               (base/combine-fns steps-fns)
               (partial capturing-WF *wf)
               )]

      ; store wf
      (swap! *wf merge wf)
      (swap! *wf assoc ::status :running)
      (base/run-wf! wf identity)
      )
    ))


(defn form-wf [*wf-state
               init-fns
               ctx-fns
               steps-fns
               opt-fns]
  ;; can ds have a keyword key?
  (let [
        _init-fns (conj init-fns
                       (fn [params]
                         {:wf *wf-state}
                         )
                       )

        ]

    (let [wf-cfg {
                  :init-fns _init-fns
                  :ctx-fns ctx-fns
                  :steps-fns  steps-fns
                  :opt-fns   opt-fns
                  }]
      (run-wf! *wf-state wf-cfg)
      )

    )
  )

(defn form-ctx-fn [*wf params]
  {
   :v {:fn identity}
   :kv-zip* {
             :fn (fn [vs]
                   ;; todo: check for correct number of vs
                   (apply hash-map vs))
             :collect? true
             }
   :kv-merge* {
              :fn (fn [vs]
                    (apply merge vs))
              :collect? true
              }
   ; todo: check with infinite steps - if the resulting map will be updated
   }
  )

(defn form-steps-fn [*wf params]
  {
   ::k1 [:v :k1]
   ::v1 [:v "v1"]

   ::defaults [:v {:k1 "default" :k2 "k2"}]

   ::zip [:kv-zip* [::k1 ::v1]]

   ::data [:kv-merge* [::defaults ::zip]]
   }
  )

(defn form-opt-fn [*wf params]
  {
   :before-process  (fn [wf-chan xtor]
                      (swap! *wf assoc ::xtor xtor)
                      :ok)
   :op-handlers-map {
                     :process (fn [result]
                                (swap! *wf assoc ::result result))
                     :done    (fn [result]
                                (swap! *wf assoc ::result result)
                                (swap! *wf assoc ::status :done)
                                (println "[wf]:done\n" (d/pretty result)))
                     :error   (fn [result]
                                (swap! *wf assoc ::status :error)
                                (swap! *wf assoc ::error result)
                                (println "[wf]:error\n" (d/pretty result)))
                     }
   }
  )


(rum/defc <header-row> < rum/reactive [*wf]
  [:.row [:.gutter
          ; kinda toggler
          ;; (ui/menubar "" [["▹" (fn [])]])
          ]
   (ui/menubar "WORKFLOW" [
                           ;["run" (fn [])]
                           ])
   ;; run should be in steps
   [:.status  "✰ " [:small "running"]]
   ]
  )


(rum/defc <IN-row> < rum/reactive [*wf]
  [:.row [:.gutter "IN:"]
   "..."
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

(rum/defc <UI-row> < rum/reactive [*wf]
  [:div.row
   [:div.gutter "UI:"]

   (let [wf @*wf
         history (::history wf)
         status (::status wf)
         ;xtor (::xtor wf)
         ]
     [:pre
      (d/pretty status)
      ;(d/pretty xtor)
      (d/pretty history)
      ]
     ;"..."
     )

   ]
  )


(rum/defc <OUT-wf> < rum/reactive [*wf]
  [:.row
   [:.gutter "OUT:"]

   (when-let [result (::result @*wf)]
     ;[:pre (d/pretty result)]

     (let [st {:style {:border "1px solid blue" :border-bottom "none" :border-left "none" :padding ".5rem"}}]
       (into [:.flex]
             (map-indexed (fn [i [k v]]
                    [:div (if (= 0 i) (assoc-in st [:style :border-left ] "1px solid blue") st)
                     [:p (str ":" (name k))]
                     [:code (pr-str v)]
                     ]
                    ) result)
             )
       )

     )

   #_[:div.flex

    [:div {:style {:border "1px solid blue" :border-bottom "none" :padding ".5rem"}}
     [:label "::included"] [:code "[...]"]]

    [:div {:style {:border "1px solid blue" :border-bottom "none" :border-left "none"
                   :padding ".5rem"}}
     [:label "::excluded"] [:code "[...]"]]

    ]
   ]

  )


(rum/defc <export-row> < rum/reactive [*wf]
  [:.row [:.gutter "=>"]

   [:div
    ;[:p.nfo "export result: for cases if there are two many OUT parameter, or they need to be adjusted"]
    [:pre
     "{\n :files [...] \n}"
     ]
    ]]

  )

(defn run-static-wf [*wf]
  (let [init-fns [(fn [params]
                    {:some :data}
                    )]
        ctx-fns [(partial form-ctx-fn *wf)]
        steps-fns [(partial form-steps-fn *wf)]
        opt-fns [(partial form-opt-fn *wf)]
        ]
    (form-wf *wf
             init-fns
             ctx-fns
             steps-fns
             opt-fns)
    )
  )

(rum/defc <static-wf> < rum/reactive [*wf]

  [:div.proto10
   [:div.example {:style {}}

    (ui/menubar "tmp actions: " [["run" (partial run-static-wf *wf)]])

    [:.row
     [:pre
      (try (d/pretty
             ;(keys @*wf)
             (dissoc @*wf ::init-params)
             )
           (catch js/Error e
             "error")
           )

      (let [init-params (::init-params @*wf)]
        (d/pretty (keys init-params))
        )
      ]

     ]


    (<header-row> *wf)
    (<IN-row> *wf)
    (<UI-row> *wf)
    (<OUT-wf> *wf)
    (<export-row> *wf)

    ]
   ]
  )


(rum/defc <form-example> < rum/reactive [*STATE]
  [:div.proto10 {:style {:padding "1rem"}}


  [:p "if no wf ui is provided - show standard progress ui"]

   (let [*wf (rum/cursor-in *STATE [:wf1])]
     (<static-wf> *wf)
     )

   [:hr]

   "compile time vs runtime"
[:.flex
 [:div.example {:style {:width "18rem" :margin-right "1rem"}}
  [:.row [:.gutter [:.status "♕"]]
   (ui/menubar "WORKFLOW w NO INPUTS" [["compile" (fn [])]])
   ]

  [:.row [:.gutter "IN:"]
   [:div
    [:p.nfo "whether WF has IN params:"]

    [:pre
"
IN params:  [
  path <path>
  include <*.*>
  exclude nil
  encoding UTF-8
]\n"

     ]
    ]
   ]
  [:div.row
   [:div.gutter "WF:"]
   [:div
    [:pre
     "init fn:    [UI,IN]\n"
     "context fn: [..+FS]\n"
     "step fn:    [FS]\n"
     "opt fn:     [OUT]\n"

     ]
    [:p.nfo "whether WF has UI wf:"]
    [:pre "UI wf:
a) run button - collect INs and run wf
b) auto-upd UI
c) ui that will confirm \n"]
    ]
   ]

  [:.row
   [:.gutter "OUT:"]
   ;; what will wf return
   [:pre "OUT: [::included ::excluded]"]
   ]

  [:.row [:.gutter "  ->"]

   [:div
    ;[:p.nfo "export result: for cases if there are two many OUT parameter, or they need to be adjusted"]
    [:pre
     ;;"transformations: identity"
     "transformations: (subst)\n
{
  :files   @::included
}"

     ]
    ]]

  ]

 [:div.example {:style {:width "25rem"}}
  [:.row [:.gutter
          ; kinda toggler
          ;; (ui/menubar "" [["▹" (fn [])]])
          ]
   (ui/menubar "WORKFLOW" [
                           ;["run" (fn [])]
                           ])
   ;; run should be in steps
   [:.status  "✰ " [:small "running"]]
   ]

  [:.row [:.gutter "IN:"]
   [:div.flex
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
  [:div.row
   [:div.gutter "UI:"]

   ;; waiting for inputs

   ;; ::collect-params [::input1 ::input2 ...]

   ;; can be triggered via button (so inputs are non infinite)
   ;; or infinite (so UI can be updated)

   ;; results can be returned immediately
   ;; or should be confirmed via button

   [:pre "waiting for inputs...\n"
    "working...\n"
    "done..."
    ]
   ]

  [:.row
   [:.gutter "OUT:"]

   [:div.flex
    [:div {:style {:border "1px solid blue" :border-bottom "none" :padding ".5rem"}}
     [:label "::included"] [:code "[...]"]]

    [:div {:style {:border "1px solid blue" :border-bottom "none" :border-left "none"
                   :padding ".5rem"}}
     [:label "::excluded"] [:code "[...]"]]

    ]
   ]

  [:.row [:.gutter "=>"]

   [:div
    ;[:p.nfo "export result: for cases if there are two many OUT parameter, or they need to be adjusted"]
    [:pre
     "{\n :files [...] \n}"
     ]
    ]]

  [:.row
   [:pre
    "todo: internals
    state mgmt, etc.
    "
    ]
   ]

  ]
 ]

[:p ""]
   (ui/btn "test"
           (fn []
             (let [*wf (rum/cursor-in *STATE [::wf])
                   init-fns []
                   ctx-fns [(partial form-ctx-fn *wf)]
                   steps-fns [(partial form-steps-fn *wf)]
                   opt-fns [(partial form-opt-fn *wf)]
                   ]
               (form-wf *wf
                        init-fns
                        ctx-fns
                        steps-fns
                        opt-fns)
               )

             ))
   (let [{wf ::wf} @*STATE]
     [:pre
      (d/pretty (::status wf))
      (d/pretty (::result wf))
      ]
     )

   ]
  )

(rum/defc <ui> < rum/reactive [*STATE]
  [:div
   [:p "prototype 10. kv substitution and generic wf ui"]
   [:header "kv substition"]
   [:div

    (<form-example> *STATE)

    [:p "how to build map like this? basically it means to implement a custom form"]
    [:pre (pr-str {:k1 "k1" :k2 "k2" :v [1 2 3]})]


    [:pre
"[
  ;; keys
     ::k-k1 [:k :k1]
     ::k-k2 [:k :k2]
     ::k-v  [:k :v]\n
  ;; values
   ::v-k1 [:v 'k1']
   ::v-k2 [:v 'k2']
   ::v-v  [:v [1 2 3]]\n
  ;; zip
   ::data [:kv-zip* [
                  ::k-k1 ::v-k1
                  ::k-k2 ::v-k2
                  ::k-v  ::v-v
                  ]]
   ; or via intermediary maps
   ::kv1 [:kv* [::k-k1 ::v-k1] ]
   ::kv2 [:kv* [::k-k2 ::v-k2] ]
   ::kvv [:kv* [::k-v  ::v-v] ]

   ::data [:kv-merge [::kv1 ::kv2 ::kvv]]
]"

"\n
so values can be indirected via intermediary steps (like ui)
or you can just provide the data (for testing)

you can have a workflow with holes/gaps/steps to be implemented
ex: you should provide ::data step

defaults:

no need to provide all data, as there can be sensible defaults, like

[
  ::defaults [:v {:k1 \"k1\", :k2 \"k2\", :v [1 2 3]}]
  ...
  ::provided-data [...]

  ::data [:kv-merge [::defaults ::provided-data]]
]

validation:

there can be separate step for validation

[
  ::data ...
  ::validate-data [:validate ::data]
]
"

     ]

    ]
   [:hr]
   [:div.flex
    ]
   ]
  )

;; root ui component for prototype
(def <app> #(<ui> *UI-STATE))
