(ns woof.app
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]
    [cljs.test :as test :refer-macros [deftest is testing run-tests async]]



    [woof.data :as d]
    [woof.wf :as wf]
    [woof.utils :as u]

    [woof.testo :as ttt]

    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]
    [woof.utils-macros :refer [put!?]]
    ))


(enable-console-print!)

(defonce *APP-STATE
  (atom {
          :hello :woof
          :context {
                     :hello {:fn (fn [a]
                                   (println ":hello" (pr-str a))
                                  ;"Hello!"

                                   (let [c (async/chan)]

                                     (go
                                       (async/put! c "Hello!")
                                       )

                                     c
                                     )
                                )}
                     }
          :workflow {
                      :name "test workflowww"
                      :steps (assoc (array-map)
                                ::0 [:hello {}]
                                ;; ::1 [:hello {}]
                               )
                      }
        }))







(rum/defc menu-item
    < { :key-fn (fn [label _]
                  (str label))}
  [label action-fn]
    [:a.menu-item {:href "#"
                   :on-click (fn [e]
                               (action-fn)
                               (.preventDefault e)
                               false)}
     label])


(rum/defc menubar
  < rum/reactive
  "build a menubar"
  [menu-header menu-items] ; TODO: why its not a component?
  (into [:span.menubar
           [:.header menu-header]]
        (map (fn [[label action]]
               (menu-item label action)
               )
             menu-items)))



(defn run-wf! [executor exec-chan save-op-fn]
  (go-loop []
           (let [r (async/<! exec-chan)
                 [status data] r]
             (println "UPD:" (pr-str r))

             (if (save-op-fn r)
               (recur))
             ))
  (go
    ;; (async/<! (u/timeout 5000))
    ;;(wf/end! executor)

    )
  )









(rum/defcs wf-ui
  < rum/reactive
    (rum/local nil ::executor)
    (rum/local nil ::exec-chan)
    (rum/local []  ::result)

  [local *context *workflow]

  (let [executor  @(::executor local)
        exec-chan @(::exec-chan local)

        {steps :steps
         header :name
         } @*workflow]


    ;; short-hand version
    #_(do
        (when (nil? executor)
          (reset! (::executor local) (wf/executor *context steps))

          (let [exec-chan (wf/execute! @(::executor local))]
            (reset! (::exec-chan local) exec-chan)
            (run-wf! executor exec-chan (fn [r]
                                          (swap! (::result local) conj r)
                                          true
                                          ))))
        [:pre (d/pretty @(::result local))]
        )

    (if (nil? executor)
      [:div
       (menubar header [["init" (fn []
                                    (reset! (::executor local) (wf/executor *context steps)) )]])
       [:pre (d/pretty steps)]
       ]
      (if (nil? exec-chan)
        [:div
         (menubar header
                  [["run" (fn []
                    (let [exec-chan (wf/execute! @(::executor local))]
        (reset! (::exec-chan local) exec-chan)
        (run-wf! executor exec-chan (fn [r]
                                      (swap! (::result local) conj r)
                                      true
                                      )))
                            )]])]
       ;;
       [:pre (d/pretty @(::result local))]))


  )
)




(rum/defcs app-ui
  < [local *STATE]
  [:div#app
    ;[:header (str "Hello! " (:hello @*STATE))]

    (wf-ui (rum/cursor-in *APP-STATE [:context])
            (rum/cursor-in *APP-STATE [:workflow]))
   ])


(rum/mount (app-ui *APP-STATE)
           (. js/document (getElementById "app")))


(add-watch *APP-STATE :watcher
  (fn [key atom old-state new-state]
    (rum/mount (app-ui *APP-STATE)
               (. js/document (getElementById "app")))))


(defn on-js-reload []
  #_(.clear js/console)
)






(test/deftest hello
  (println "YO!!"))


;;(cljs.test/run-tests)


