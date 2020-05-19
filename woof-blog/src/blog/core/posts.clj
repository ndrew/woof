(ns blog.core.posts
  (:require
    [clojure.data.json :as json]

    [woof.base :as base]
    ))


;;
;; Blog post retrieving

(defn posts-ctx-fn [params]
  {
   :paths         {
                   :fn       (fn [files]
                               (map (fn [v]
                                      (prn (get v :meta))
                                      (get v :meta)
                                      ) files)
                               )
                   :collect? true
                   }



   :read-meta     {
                   :fn (fn [file-map]
                         (let [contents (:contents file-map)
                               ;regex #"(?is)^(<!--!(.+)-->)"
                               regex #"(?is)<!--!(.*)-->"
                               [_ m] (re-find regex contents)
                               ]

                           (if m
                             (let [meta-json (clojure.string/trim m)
                                   meta-obj (json/read-json meta-json)
                                   ]
                               ;(prn (:name file-map) meta-obj)
                               (assoc file-map :meta meta-obj)
                               )
                             (assoc file-map :meta {
                                                    ;; unpublished
                                                    })
                             )
                           )
                         )
                   }

   :enrich-posts* {
                   :fn       (fn [kv]
                               (reduce (fn [a [k v]]
                                         ;; if in-cache - return identity from cache
                                         ;; if not - introduce read step
                                         (let [in-cache? false]
                                           (if in-cache?
                                             (assoc a (base/rand-sid "from-cache-") v)
                                             (let [read-sid (base/rand-sid "read-")]
                                               (assoc a
                                                 read-sid [:slurp! v]
                                                 (base/rand-sid "meta-") [:read-meta read-sid])
                                               )

                                             )

                                           )
                                         ) {} kv)
                               )
                   :expands? true
                   }

   :final-posts*  {
                   :fn       (fn [files]
                               (filter (fn [f]
                                         (if-let [m (get f :meta)]
                                           (:Ready m))) files)
                               )
                   :collect? true
                   }

   :sort-posts    {
                   :fn (fn [posts]
                         (sort-by (fn [p]
                                    ;(prn (get-in  p [:meta :Date]))
                                    (get-in p [:meta :Date])
                                    ) posts)
                         )

                   }

   ;; collect posts

   }
  )

;;
;; POSTS


(defn posts-impl []
  {
   :ctx posts-ctx-fn

   ;; todo: should posts steps be hardcoded here?
   }
  )