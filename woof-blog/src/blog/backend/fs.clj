(ns blog.backend.fs
  (:require
    [woof.base :as base]
    [taoensso.timbre :refer [info]]
    [me.raynes.fs :as fs]))


(defn files-ctx-fn [params]
  {
   :file {:fn (fn [path]
                ;; (info :file path)
                (let [file (fs/normalized (fs/file path))]
                  {
                   :initial-path path
                   :f file
                   :name (fs/base-name file)
                   })
                )
          }

   ;; read text contents of the file
   :slurp! {:fn (fn [file-map]
                  ;; todo: encoding
                  (let [path (fs/absolute (:f file-map))]
                    (assoc file-map
                      :contents (slurp path))
                    ))}

   ;; one level, include filters
   :ls* {
         :fn (fn [f]
               (let [root (:f f)
                     children (if-let [incl (:include f)]
                                (fs/find-files root incl)
                                (fs/list-dir root))]

                 (reduce
                   (fn [a f]
                     (assoc a (base/rand-sid) [:id {
                                                    :f f
                                                    :name (fs/base-name f)
                                                    }]))
                   {} children
                   )

                 )
               )
         :expands? true
         }
   })



(defn fs-impl []
  {
   :ctx files-ctx-fn
   }
  )