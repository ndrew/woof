(ns woof.client.playground.wf.multi.wf-ui
  (:require
    [cljs.core.async :as async]

    [clojure.string :as string]
    [rum.core :as rum]

    [woof.client.playground.ui.wf :as wf-ui]
    [woof.client.playground.ui :as ui]


    [woof.base :as wf]
    [woof.utils :as u]
    [woof.u :as base]
    [woof.data :as d]
    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; UI example for specific workflow, WIP

(rum/defcs <f-item> < rum/static
                      (rum/local false ::grow?)
  [local title body]
  ;; how to make this grow properly
  [:div.f-item
                (if @(::grow? local)
                  {:style {:width "100%" :flex-grow 1}
                   :class "f-selected"
                   }
                  {

                   }
                  )
   [:span.f-title {:on-click (fn [e]
                               (swap! (::grow? local) not))} title]
   [:div.f-body body]]
  )


(rum/defc <CFG-results> < rum/static
  [wf CFG-KV]

  [:div {:style {:margin-bottom "1rem"}}
   [:header "CFG - config process"]

   ;; show config keys as ui
   (let [res (:result wf)
         cfg-ks (get (group-by namespace (keys res)) "cfg")

         sub-configs (filter (fn [sid] (string/ends-with? (name sid) "-cfg")) cfg-ks)]

     [:div.f-box
      (map
        #(<f-item> (name %) (d/pretty! (get res %)))
        sub-configs)
      ]

     #_[:pre
      #_(d/pretty!
             (reduce (fn [a b]
                       (assoc a b (get res b))
                       ) {} cfg-ks)
             )]

     )
   ]
  )

(rum/defc <CONTENT-results> < rum/static
  [wf POSTS-KV]

  (let [
        file-names (get POSTS-KV :content/FILE-NAMES [])
        file-contents (get POSTS-KV :content/FILE-CONTENTS [])

        ;; reduce assets into it's owner
        root-files (sort-by (fn [item] (:f item))
                            (filter #(get % :content) file-contents))

        ]
    [:div {:style {:margin-bottom "1rem"}}
     [:header "CONTENT - content mgmt process"]

     [:div.f-box
      (map (fn [item]
             (let [k (:f item)
                   contents (:content item)]
                  (<f-item> k
                            [:div
                             [:header "contents:"]
                             [:div {:style {:margin-bottom ".5rem"}} contents]


                             (let [assets-set (set (:assets item))
                                   assets   (sort-by (fn [item] (:f item))
                                              (filter (fn [asset]
                                                    (assets-set (:f asset))
                                                    ) file-contents))]

                               (if (> (count assets-set) 0)
                                 [:div
                                  [:header "assets:"]
                                  [:ul (map #(vector :li (get % :f)) assets)]]))
                             ]
                            )
                  )

             ) root-files)
      ]



     #_(map (fn [[k v]]
              [:div
               (pr-str k)
               #_[:code
                  (d/pretty! v)
                  ]

               ]
              ) POSTS-KV)

     ]

    )

  )


(rum/defc <RENDER-results> < rum/static
  [wf RENDER-KV]

  ;; show config keys as ui
  (let [res (:result wf)

        ; render-results (get (group-by namespace (keys res)) "render")
        ; posts (get RENDER-KV :render/POSTS [])

        rendered-kvs (get RENDER-KV :render/TEST-RENDER [])
        ]

    [:div {:style {:margin-bottom "1rem"}}
     [:header "RENDER - rendering process"]


     [:div.f-box
      (map
        #(<f-item> (get % :f)
                   [:i (:html %)])
        rendered-kvs)
      ]
     ]
    )
  )



(rum/defcs <ui-prototype-1> < rum/reactive
           (rum/local {} ::inline-results?)

           [local *UI-STATE *wf]

           (let [wf @*wf

                 state @*UI-STATE
                 actions (:log state)
                 ready? (:ready? state)

                 ]

             [:div.job-prototype
              (str "R E A D Y — " (pr-str ready?))

              ;; for now display results on top
              (<CFG-results> wf (get state :CFG {}))

              (<CONTENT-results> wf (get state :POSTS {}))

              (<RENDER-results> wf (get state :RENDER {}))

              [:hr]

              (map (fn [[id v chan]]
                     [:div id
                      (if (seq? v)
                        (map (fn [item]
                               [:div (d/pretty! item)]
                               ) v)
                        (d/pretty! v)
                        )
                      [:hr]
                      ]

                     ) actions)

              ;[:hr]
              ;[:pre (pr-str (keys (get wf :result {})))]
              ;(tree-results (get wf :result {}))
              ;[:hr]


              ;; persistent filters in UI - to hide/show un-needed steps

              [:pre
               (d/pretty! (group-by namespace (keys (:result wf))))
               ]

              ;;(wf-ui/<default-body> *wf)

              ]
             )

           )