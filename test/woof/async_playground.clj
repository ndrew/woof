(ns woof.async_playground
  (:require
    [clojure.core.async :as async :refer [go go-loop]]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.wf-data :as wdata]

    [woof.utils :as u]
    [woof.test-data :as test-data]
    [woof.graph :as g]

    [criterium.core :as criterium]

    [clojurewerkz.propertied.properties :as props]

    [clojure.java.io :as io]

    [clojure.java.shell :refer [sh]]
))





;;
;; step handlers
;;


;; transform

(defn id-handler []
  {:fn identity}
  )

;; collect

(defn id*-handler []
  {:fn identity
   :collect? true})


(defn concat-collect-handler []
  {:fn (fn[vs]
         (apply concat vs))
   :collect? true
   })


(defn zip-interleave-collect-handler []
  {:fn (fn [vs]
         (partition (count vs) (apply interleave vs)))
   :collect? true
   })


;; expands

(defn expand-handler
  [step-handler-id]
  {:fn (fn[sids]
         (into {} (map (fn[x]
                         [(wf/rand-sid)
                          (wf/sbody step-handler-id x)]) sids)))
   :expands? true}
  )

;; expand-collect

(defn expand-collect-handler [step-handler-id collect-fn]
  (wf/step-handler
    (fn [vs]
      (into {} (map
                 (fn [x]
                   [(wf/rand-sid) (wf/sbody step-handler-id (collect-fn x))])
                 vs))


      )
    :expands? true
    :collect? true)
  )


;;;;




(defn join-path [p1 p2]
  (str p1 p2))



(defn bump-version [version
                    root
                    prop-files-paths]

  ;; workflow that "bumps" the version in several .property files

  ;;
  (let [expand-with-join-handler (fn [files* root]
                                   (into {}
                                         (map (fn[x]
                                                [(wf/rand-sid) (wf/sbody :id (join-path root x))])
                                              files*)))

        save-props-handler (fn [vs]
                             (map
                               (fn [[path props]]
                                 (props/store-to props (io/file path))
                                 [:ok (str "saved to " path)])
                               vs))

        bump-version (fn [version p]
                       (let [nu (props/load-from p)]
                         (.setProperty nu "version" version)
                         nu))


        context-map {
                      :id (id-handler)              ;; identity, for storing values in wf
                      :collect (id*-handler)        ;; collect values from sids

                      ;; provide the list of version file paths to work with
                      :versions-provider* {:fn (partial expand-with-join-handler prop-files-paths)
                                           :expands? true}

                      ;; path -> java.util.Property
                      :prop! {:fn (fn [path] (props/load-from (io/file path)))}

                      ;; path[] -> java.util.Property[]
                      :props* (expand-handler :prop!)

                      ;; duplicate properties file and set new version
                      :bump-versions*! (expand-collect-handler :id (partial bump-version version))

                      ;; save property
                      :save! {:fn save-props-handler}

                      ;; group (zip) sid-lists
                      :zip (zip-interleave-collect-handler)

                      }
        context (wf/make-context context-map)

        steps (assoc (array-map)
                ::root   [:id root]

                ::&paths       [:versions-provider* ::root]
                ::&props       [:props* ::&paths]
                ::&new-props   [:bump-versions*! ::&props] ;;

                ::#paths   [:collect ::&paths]
                ::#props    [:collect ::&new-props]

                ::paths-n-props [:zip [::#paths ::#props]]

                ::save     [:save! ::paths-n-props]


                )
        ]

    @(wf/sync-execute! (wf/build-executor context steps) 100)

    )
  )



;(sh "bash" "build.sh" :dir "/Users/ndrw/w/RELEASE/trunk/icews/build/")


;(defn run-build []
(let [context-map {
                    :sh! {:fn (fn[path]
                               (apply sh path)
                               )}





                    }
      context (wf/make-context context-map)

      steps {

              ;; all args have to be stringified
              ::cd [:sh! ["bash" "build.sh" :dir "/Users/ndrw/w/RELEASE/trunk/icews/build/"]]


              }
      ]
  @(wf/sync-execute! (wf/build-executor context steps) 150000)
  )
;  )

#_(let [result (bump-version
                "21.0.1.7"
                "/Users/ndrw/w/RELEASE/trunk/"
                ["acs/build/version.txt" "frontend/build/ice-version.txt" "icews/build/icews-build.properties"])]

   (wdata/inline-results result)
  )





;(clojure.pprint/pprint (wdata/inline-results result))



;#_(doall (map dbg result) )



      ;;(::new-versions (wdata/inline-results result))
      ;(println (d/pretty result))
      ;; (wdata/inline-results result)
      ;(::z result)

      ;(::save result)



















#_(let [context-map {
                    :id (id-handler)
                    :expand-id (expand-handler :id)

                    :collect (id*-handler)
                    :concat-ids (concat-collect-handler)
                    :concat-vals (concat-collect-handler)

                    :bump*! {:fn (fn [props]
                                  {
                                    ::bump-args [:id props]
                                    }
                                   )
                             :expands? true
                             :collect? true
                             }

                    :zip (zip-interleave-collect-handler)

      }
      steps (assoc (array-map)

              ;; insert new :id
              ::n [:expand-id [1 2 3]]
              ::n-vals [:collect ::n]

              ::zip    [:zip [::n ::n-vals]]


              ::k [:expand-id [:1 :2 :3]]
              ::k-vals [:collect ::k]

              ;; ::ids    [:concat-ids [::n ::k]]
              ;; ::vals   [:concat-vals [::n-vals ::k-vals]]

              ;;::zip    [:zip [::ids ::vals]]

              )

      context (wf/make-context context-map)
      result @(wf/sync-execute! (wf/build-executor context steps) 100)
      ]

  #_[
    (::ids result)
    (::vals result)
    ]

    #_(::zip result)


  (doall
      (map clojure.pprint/pprint result)
      )

  ;;(::bump-args result)
  )


(defn- dbg [z]
  (let [[k & v] z]
    (clojure.pprint/pprint (name k))
    (clojure.pprint/pprint v)
    (println "\n")
    )
  z
  )



;; enrichment pattern



(comment
                      ;;
                    :bump! {:fn (fn [props]
                                 (println "boo: \t" (d/pretty props))

                                 (let [{x :props} props
                                        nu (props/load-from x)]
                                    (.setProperty nu "version" version)
                                    (assoc props :props nu)
                                   )
                                 )}

                    ;; (wf/expand-handler #(wf/sid :bump- (name %))  ...

                    :bump* {:fn (fn [props]
                                  (into {} (map
                                               (fn [x]
                                                 [(wf/sid :bump- (name x)) [:bump! x]])
                                               props)))
                            :expands? true
                            }



                    :bump<> {:fn (fn [sids]
                                  (println sids)

                                        sids
                                  )
                            :collect? true
                            }

  )





#_(defonce SAMPLE-CONTEXT {

    :hello {:fn (fn [a]                  ;; each step handler accepts 1 argument
                  (str "Hello " a))}

    :timeout200 {:fn (fn [a]
                       (Thread/sleep 200)
                       (str "Hello " a))}

})



#_(let [context-map {
          :expand {:fn (fn [xs]
                         (into (array-map)
                               (map-indexed (fn [i x]
                                              [(keyword (str *ns* "/" i)) [:map x]]) xs)))
                   :expands? true
                   }


          :collect {
                     :fn (fn[xs] xs)
                     :collect? true
                     }

          :map {:fn (fn [x] x)}

          :async-map {:fn (fn[x]
                            (let [chan (async/chan)]
                              (go
                               (async/<! (u/timeout 500))
                               (async/put! chan x))
                              chan))}
        }
        context (wf/make-context context-map)]

    ;; collect step functions gather the results the sids provided to them
    (let [steps {
                  ::map-reduce [:collect [::1 ::2]]

                  ::1 [:map 1]
                  ::2 [:map 2]

                  ::async-map-reduce [:collect [::a1 ::a2 ::1 ::2]]

                  ::a1 [:async-map 3]
                  ::a2 [:async-map 4]


                  ;::map-reduce-vals [:collect [1 2]]
                  ;::map-reduce-after-wf [:expand [1 2 3 4]]
                  }

          result @(wf/sync-execute! (wf/build-executor context steps))]

          ;; also results can be collected after workflow had been executed
    ;;
    #_(let [steps {
            ::map-reduce-after-wf [:expand [1 2 3 4]]
          }
          result @(wf/sync-execute! (wf/build-executor context steps))]
      (wdata/inline-results result)
      ;(is (= (wdata/inline-results result) {::map-reduce-after-wf '(1 2 3 4)})))

    )


       result
        ; (is (= (::map-reduce result) '(1 2)))
        ; (is (= (::map-reduce-vals result) '(1 2)))
        ; (is (= (::async-map-reduce result) '(1 2 1 2)))


      )
)








