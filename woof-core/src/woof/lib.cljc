(ns woof.lib
    (:require [woof.u :as u]))

;; FIXME: clean this!

(defn foo []
      (str "foooooooo!" (u/rand-sid) ))


(defn -main []
      (println (foo)))