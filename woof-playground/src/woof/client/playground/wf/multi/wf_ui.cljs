(ns woof.client.playground.wf.multi.wf-ui
  (:require
    [cljs.core.async :as async]

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

(rum/defc <CONTENT-results> < rum/static
          [POSTS-KV]

          (let [
                file-names (get POSTS-KV :content/FILE-NAMES [])
                file-contents (get POSTS-KV :content/FILE-CONTENTS [])

                ;; reduce assets into it's owner
                root-files (filter #(get % :content) file-contents)
                ]
            [:div
             [:header "POSTS"]

             ;(pr-str file-names)

             (map (fn [item]
                    [:div
                     [:header (pr-str item)]


                     (let [assets-set (set (:assets item))
                           assets (filter (fn [asset]
                                            (assets-set (:f asset))
                                            ) file-contents )]
                       [:code (pr-str assets)]
                       )

                     ]
                    ) root-files)




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

              [:div {:style {:display "flex" :flex "0 1 auto"}}

               (<CONTENT-results> (get state :POSTS {}))
               ]
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

              (wf-ui/<default-body> *wf)

              ]
             )

           )