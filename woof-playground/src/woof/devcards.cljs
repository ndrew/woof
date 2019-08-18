(ns ^:figwheel-hooks woof.devcards
  (:require [devcards.core :as devcards]
            [sablono.core :as sab]
            )
  (:require-macros
    [devcards.core :refer [defcard]])
  )

(enable-console-print!)

(defn render []
  (devcards/start-devcard-ui!*))


(defcard my-first-card
  (sab/html [:h1 "Testing dev cards"]))

;; todo: devcard for running the wf


(defn ^:after-load render-on-relaod []
  (render))

(render)