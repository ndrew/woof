(ns woof.u
  "helper and utils base"
  (:require

    #?(:clj [clojure.core.async :as async :refer [go go-loop]])
    #?(:cljs [cljs.core.async :as async])

    )
  #?(:cljs
     (:require-macros
       [cljs.core.async.macros :refer [go go-loop]]))
  #?(:clj (:import (java.util UUID)))
  ;(:gen-class)
  )


(defn sid
  "generates a particular id for a step — sid"
  ([id]
   (keyword (str *ns*
                 "/"
                 (if (keyword? id) (name id) id))))
  ([prefix id]
   (sid (str prefix
             (if (keyword? id) (name id) id)))))


;; use our version to support clojure 1.8
(defn is-qualified-keyword?
  "Return true if x is a keyword with a namespace"
  [x] (boolean (and (keyword? x) (namespace x) true)))


;; predicate that checks parameter in workflow is a link to other action
(defn sid?
  "checks if id is sid: it should be qualified keyword. So we can distinguish it as parameter"
  [id]
  (is-qualified-keyword? id))


(defn sid-list?
  "checkis if sids is a collection where all items are sid"
  [sids]
  (and (or (seq? sids) (vector? sids))
       (every? sid? sids)))


(defn sid-map
  "creates a sid map for specified sid-sbody pairs
  or just use (assoc (array-map) k1 v1 k2 v2)
  "
  [pairs] ;; or use (assoc (array-map) k1 v1 k2 v2)
  (into (array-map) (map identity pairs)))

(defn- gen-uuid []
  #?(:clj  (UUID/randomUUID)
     :cljs (random-uuid)))


;; todo: use uid shortener?

(defn str-uuid [uuid]
  ;(uuid/str uuid)
  (str uuid)
  )


(defn rand-sid
  "generates random sid"
  ([]
    (sid (str-uuid (gen-uuid))))
  ([prefix]
    (sid prefix (str-uuid (gen-uuid)))))



(defn nil-get
  "predicate for checking the if the value is nil or :nil in a map
  this is needed as nils are not allowed to put into channels
  "
  [rr id]
  (let [r (get rr id)]
    (if (nil? r)
      (if (contains? rr id) :nil nil)
      r))
  )




(defn wiretap-chan
  "splits in-chan into internal channel and resulting channel"
  [in-chan wiretap-handler]
  (let [mult-c (async/mult in-chan)

        dbg-chan (async/chan)
        piped-c  (async/chan)]

    (async/tap mult-c dbg-chan)
    (async/tap mult-c piped-c)

    (go-loop []
             (when-let [v (async/<! dbg-chan)]
               (wiretap-handler v)
               (recur)
               )
             )

    piped-c)
  )



(defn subsitute-with-rand-sids
  "substitutes the sids with a rand-sid "
  [context-map expand-map]
  ;; as specifying random sids can be cumbersome
  ;; we can substitute these automatically

  (let [*subs-map (volatile! {})]

    (doseq [[sid [shandler-id param]] expand-map]
      ;; store all sids in substitution map
      (vswap! *subs-map
                        assoc sid (rand-sid))
      )

    (let [subs-map @*subs-map]
      (reduce (fn[a [sid [shandler-id param]]]

                (let [shandler (get context-map shandler-id)
                      new-param (cond
                                  (:collect? shandler) (if (sid-list? param)
                                                         (map #(get subs-map %) param)
                                                         (if (sid? param)
                                                           (get subs-map param)
                                                           param
                                                           )
                                                         )

                                  ;; (and (:collect? shandler) (:expands? shandler)) ""
                                  ;; (:expands? shandler) ""
                                  ;;
                                  :else (if (sid? param) (get subs-map param) param)

                                  )

                      ]



                  (assoc a (get subs-map sid) [shandler-id new-param])
                  )
                ) {} expand-map)

      )
    )
  )



(defn gen-seq-sid-fn
  "generates a sid function generator that produces sequential sids"
  []
  (let [counters (atom {})]
    (fn sid-fn
      ([]
       (sid-fn :i))
      ([prefix]
       (sid (str prefix (get (swap! counters update-in [prefix] (fnil inc -1)) prefix)))))))

