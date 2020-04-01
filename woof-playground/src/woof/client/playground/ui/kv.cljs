(ns ^:figwheel-hooks woof.client.playground.ui.kv
  (:require
    [rum.core :as rum]

    [clojure.data :as cd]
    [clojure.string :as str]

    [woof.base :as base]
    [woof.client.playground.ui :as ui]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.utils :as u]

    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; generic key value UI

(declare <row>)


(defn gen-toggle-action_ [h k *ui]
  [(str (if (str/ends-with? h " ") h
                                   (str h " "))
        (ui/shorten-bool (get @*ui k)))
   (fn [] (swap! *ui update k not))])


(rum/defcs <ui> < rum/reactive
                          (rum/local true ::show?)

                          ;; this atom will be merged to ui config passed further
                          (rum/local nil ::ui)

                          (rum/local (atom {}) ::prev-results)
    [l <k> <v> cfg]

  (when (nil? @(::ui l))
    (.warn js/console "setting ui property")
    (reset! (::ui l)
            ((get cfg :ui-defaults (fn [] {})))
            )
    )

  (let [
        h1 (get cfg :header "")
        ui-cfg @(::ui l)

        show?  @(::show? l)
        toggle-action [(if show? "↑" "↓") (fn [] (swap! (::show? l) not))]
        actions (if show? (concat [toggle-action]
                                  ((get cfg :ui-actions (identity [])) (::ui l)))
                          [toggle-action])

        ]
      [:div.wf-results (ui/menubar h1 actions)

       (if show?
         (let [kvs-fn  (get cfg :kvs (fn [_] []))]

           (map (fn [row] (<row> ui-cfg <k> <v> row))
                (kvs-fn ui-cfg))
           )
         )
       ]
      )
  )


(rum/defc <row> < rum/static
                  {:key-fn (fn [_ _ _ row] (str "row-" (:k row)))}
  [cfg <k> <v> row]

  (let [_attrs-fn (get cfg :row-attrs-fn (identity {}))]
    [:div.wf-results-row (_attrs-fn cfg row)
     (<k> cfg row)
     (<v> cfg row)
     ]
    )
  )


