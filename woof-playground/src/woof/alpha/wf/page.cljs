(ns ^:figwheel-hooks woof.alpha.wf.page
  (:require
    [rum.core :as rum]

    ;; v2 deps
    [woof.client.stateful :as st-wf]

    [woof.alpha.ui.wf :as wf-ui]

    [woof.data :as d]
    [woof.wf :as wf]
    [cljs.core.async :as async]
    [woof.utils :as u])
  (:import [goog.net.XhrIo ResponseType])

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))




;;
;; alpha workflow example

;; idea from - https://twitter.com/yoshikischmitz/status/1226877576678756352


;;  return WF map containing:
;;    * necessary stuff: init-fn/ctx-fn/steps-fn
;;    * optional stuff:
;;        ui fn, wf actions, etc
;;        also we pass WF atom where this map will live

(declare stateless-init!)
(declare stateful-init!)

(defn initialize! [*WF-STATE-MAP]
  (merge
    ;;
    (stateless-init!)
    ;;
    (stateful-init! *WF-STATE-MAP))
  )



;;


(defn init-evt-loop [params]
  ;; this should prepare all stuff needed for ctx and steps
  (let [evt-loop-chan (st-wf/&chan params (wf/rand-sid "evt-loop"))]
    {
     ;; keep the evt loop chan
     ::evt-loop-chan evt-loop-chan
     }
    )
  )

(defn ctx-evt-fn [params]
  {
   :test     {:fn (fn [v] v)}

   :evt-loop {
              :fn       (fn [in-chan] in-chan)
              :infinite true
              :expands? true
              }
   }
  )

(defn steps-evt-fn [params]
  {
   ::evt-loop [:evt-loop (::evt-loop-chan params)]
   }
  )



(defn stateless-init! []
  {

   :init-fns  [
               (fn [params]
                 {::initial-path "some/dir/"}
                 )
               ;; event loop
               init-evt-loop
               ;; channel factory
               st-wf/chan-factory-init-fn
               ]

   :ctx-fns   [ctx-evt-fn

               (fn [params]
                 {
                  :test      {:fn (fn [v] v)}
                  :v         {:fn (fn [v] v)}

                  :directory {:fn (fn [dirname]
                                    {
                                     :meta                          {
                                                                     :type :folder
                                                                     }

                                     "00_bg.jpg"                    {:meta {:type :img}}
                                     "01_static_site_generator.txt" {:meta {:type    :text
                                                                            :content "azazaza\nururuur\nolololo"
                                                                            }}
                                     "02_images"                    {
                                                                     :meta             {:type :gallery}
                                                                     "01_jesus.jpeg"   {:meta {:type :img}}
                                                                     "02_studishe.png" {:meta {:type :img}}
                                                                     "03_toot.jpg"     {:meta {:type :img}}
                                                                     }

                                     "03_woof_pitch.txt"            {:meta {:type    :text
                                                                            :content "booo\n\naaaaa\n"
                                                                            }}
                                     }
                                    )
                              }

                  }
                 )]

   :steps-fns [steps-evt-fn

               ;; initial steps
               (fn [params]

                 {
                  ::initial-path [:v (::initial-path params)]
                  ::initial-tree [:directory ::initial-path]

                  ;::YO [:test "YO"]

                  })

               ]

   :opt-fns   [;; ls/ls-opts-fn
               st-wf/chan-factory-opts-fn
               ]

   }  ;;
  )


(defn clazz [item]
  (str "t-" (name (get item :type :unknown)))
  )

(rum/defc <header> < rum/static [path item]
  [:header
   {:class (clazz item)}
   (str path)]
  )


(rum/defc <tree> < rum/static [tree path parent-path]

  (let [m (get tree :meta {:type :unknown})

        full-path (str parent-path path)
        ]
    (if (#{:folder :gallery} (:type m))
      [:div.page-tree {:class (clazz m)}
       (<header> path m)

       (map (fn [nu-path]
              (if-let [f (get tree nu-path)]
                   (<tree> f (str "/" nu-path) full-path)))
            (sort (filter #(string? %) (keys tree))))
       ]

      [:div.page-item {:class (clazz m)}
       (<header> path m)

       (let [t (get m :type :unknown)]
         (cond
           (= t :img)

           [:div {:style
                  {:max-width "100%"
                   :height "auto"
                   :position "relative"
                   :margin "0 auto"}}

            [:img {:src full-path
                   :style {
                           :max-width "100%"
                           :height "auto"
                           :display "block"
                           }
                   }]
            ]

           (= t :text)
           (get m :content "")
           )
         )
       ;(pr-str tree)
       ]
      )
    )
  )


(rum/defc <root-tree> < rum/static [tree path]
  (<tree> tree "" "/page")
  )


(rum/defc <custom-wf-ui> < rum/static [wf]

  [:div

   (<root-tree>
     (get-in wf [:result ::initial-tree] {})
     (get-in wf [:result ::initial-path] "???")
     )

   [:hr]

   [:p "RESULTS!!!"]

   ;[:pre (d/pretty (sort (keys wf)))]



   ;[:hr]
   [:pre (d/pretty (:result wf))]
   ]
  )


(defn stateful-init! [*wf]
  {
   ;; for we provide a ui fn
   :ui-fn       (partial wf-ui/<default-wf-ui> <custom-wf-ui>)


   :title       "Present folder contents as web page"

   :explanation "Protoype: for now, use fake/hardcoded data"

   :wf-actions  {
                :not-started [
                              ["explain what happening (see console)"
                               (fn []
                                 (prn "example, to see whether woof workflow could handle real time scenarios"))
                               ]
                              ]
                :running [

                          ["send event" (fn []
                                          (let [loop-chan (st-wf/&wf-init-param *wf ::evt-loop-chan)]
                                               (async/put! loop-chan
                                                           {(wf/rand-sid "ui-") [:test (u/now)]})
                                               )
                                          )]

                          ]
                ; :done        []
                }


   }
  )