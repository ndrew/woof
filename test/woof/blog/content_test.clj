(ns woof.blog.content-test
  (:require
    [clojure.core.async :as async :refer [go go-loop]]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.wf-data :as wdata]

    [woof.core.runner :as runner]

    [woof.wfc :as wfc]
    [woof.utils :as u]
    [woof.xform :as x]

    ;;
    [me.raynes.fs :as fs]

))



;; testing higher order wfs

(defn context-fn [& r] ;; & {:keys []}
  (println "->CTX: " r)
  {
    :log  {:fn (fn[a]
                 (locking *out* (println "DBG:" a))
                 (identity a))}

    :log*  {:fn (fn[a]
                  (locking *out* (println "DBG:" a))
                  (identity a))
            :collect? true
            }

    ;; 'cd'
    :cd {:fn (fn [v]
               (let [file (fs/normalized (apply fs/file v))]
                 (println "SERVER: `cd " (.getAbsolutePath file) "`")
                 (.getAbsolutePath file))

               )}

    ;; 'ls'
    :dir {:fn (fn[f]
                (println "SERVER: `dir " f "`")
                (let [base (fs/normalized (fs/file f))]
                  (map (fn [f]
                         (str
                           (if (fs/directory? f) "/" "")
                           (fs/base-name f))

                         ) (fs/list-dir base))))

          }


    :post {:fn (fn [f]
                 f
                 )}

    :posts {
             :fn (fn[root]

                   (let [base (fs/normalized (fs/file root))]
                     (into (array-map)
                           (map-indexed (fn [i f]

                                          #_(str
                                              (if (fs/directory? f) "/" "")
                                              (fs/base-name f))

                                          (if (fs/directory? f)
                                            [(keyword (str "z/" (fs/base-name f)) ) [:posts f]]
                                            [(keyword (str "f/" (fs/base-name f))) [:post f]])


                                          ) (fs/list-dir base))))


                   )
             :expands? true
             }



      :names {:fn (fn[files]
                    (map (fn [a] (.getAbsolutePath a)) (flatten files))

                    )

              ;; fixme:
              :collect? true
              }


    })



(defn steps-fn [& {:keys [content-dir]}]  ;; & {:keys []}
  ;; (println "->STEPS: " r)
  {
    ; ::hello [:log (str "Reading from " content-dir)]

    ;::dir [:dir content-dir]
    ::posts [:posts content-dir]

    ::out [:names ::posts]

    })




(defn wf-fn [initial-params]
  (println "wf-fn: " initial-params)

  {
    :wf (fn [params]
            (wfc/params-wf params context-fn steps-fn)
          ) ;; (partial wwf in-chan> out-chan< *local) ;; wf constructor
    :params (merge initial-params {})
    }
)


(defn opts-fn [params]
  (println "opts-fn: " params)
  {
      :params params
      :opts {

              :timeout 2000
              :before-process (fn[wf-chan xtor]
                                (println "Hello World")
                                :ok
                                )

              :op-handlers-map {

                   :done (fn [data]

                           #_(println (d/pretty data))

                           (println "\n\n\n\n\n=======")



                           #_(println
                             (d/pretty (wdata/inline-results data))
                             )


                           (println "-----\n")

                           ;; as now

                           #_(println (d/pretty

                                      (map (fn [a]
                                             (clojure.string/replace
                                               (.getAbsolutePath a)
                                               (:content-dir params)
                                               "")
                                             )
                                           (wdata/extract-result data ::posts) )))

                           (println (::out data))

                           ;(swap! (:*state params) assoc-in [:content :dir] (::dir data))


                           )

                   :error (fn [data]
                            (println "ERROR" data))

                   }
              }
      }
  )




;; todo: wrap to a test

(defonce *blog (atom {:config {}
                       :content {}

                      }))

(runner/run-wf
      (fn []
        (println "INIT")
        {;:hello :world
          :content-dir "/Users/ndrw/m/woof/blog/posts/"

          ;:*state *blog

         }) ;; defaults
      wf-fn  ;; (fn [params] -> {:wf <wf>, :params {}})
      opts-fn
      runner/default-run-fn ;; todo: sync runner
  )


;(println @*blog)
