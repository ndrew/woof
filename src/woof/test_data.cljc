(ns woof.test-data
  (:require
    [woof.data :as d]
    [woof.wf :as exec]
    [woof.graph :as g]
    [woof.utils :as u]

    #?(:clj [clojure.core.async :as async :refer [go go-loop]])
    #?(:cljs [clojure.core.async :as async]))

  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]])))



(def DEBUG-TO-PRINT false)

(defn- debug-out! [& args]
  (if DEBUG-TO-PRINT
    (apply println args)))


(defn gen-id [id]
  (keyword (str id)))

(defn gen-ns-id [id]
  (keyword (str *ns* "/" id)))


(defn build-handler
  "builds a test step handler. Returns a reducer function."
  [id-fn step-cfg f] ;; todo: pass fn , pass keyword notion
  (fn[a x]
    (let [k (gen-id (id-fn x))]
      (assoc a k
        (merge step-cfg {:fn (partial f k x)})))))


(defn gen-step-fn
  "builds a test step handler. Returns a reducer function."
  [id-fn f] ;; todo: pass fn , pass keyword notion

  (build-handler id-fn {} f))


(defn gen-expand-fn
  "generates a test expand handler"
  [id-fn f]
  (build-handler id-fn {:expands? true} f))


(defn prepare-step-fns
  "builds a context map with step fns"
  [idx]
  (reduce
    (gen-step-fn #(str "s-" %) (fn [k i v]
                                 (debug-out! "FN" k i v)
                                 (let [c (async/chan)
                                       t (+ (int (rand 1000))
                                            (if (odd? i) (int (rand 10000)) 0))]
                                   (go
                                     (async/<! (u/timeout t))
                                     (debug-out! k "DONE!")
                                     (async/put! c i))
                                   c)))
    {} idx))

;;((:fn (:s-0 (prepare-step-fns [0]))) "Hello")


(defn prepare-expand-fns
  "builds a context map with expand fns"
  [idxs xpand-idxs sampling-fn]
  (reduce
    (gen-expand-fn #(str "xpand-" %)
                   (fn [k z s]
                     (let [ext-idxs (sampling-fn idxs)
                           r (reduce (fn[a x]
                                       (assoc a (gen-id (str "x-step-" z "--" x)) [(gen-id (str "s-" x)) {}]))
                                     {} ext-idxs)]
                       ;(println "expanded into " r)
                       r)))


    {} xpand-idxs))

;((:fn (:xpand-3 (prepare-expand-fns [0 1 2 3] [3] #(random-sample 0.13 %)))) "azaza")
;sampling-fn random-sample 0.13



(defn prepare-steps
  "builds a test workflow"
  [idxs link-sample-fn]
  (let [*idx (atom (into #{} idxs))
        link-idx (into #{} (link-sample-fn idxs))
        *r (atom (array-map))]

    (loop [j 0]
      (let [c (count @*idx)
            i (inc (rand-int (count @*idx)))
            v (first (if (= 1 i)
                       (take 1 @*idx)
                       (drop (dec i) (take i @*idx))))

            li (inc (rand-int (count link-idx)))
            lv (first (if (= 1 li)
                        (take 1 link-idx)
                        (drop (dec li) (take li link-idx))))]

        ;(println @*idx i v [(keyword (str "s-" v)) {}])
        (if (or (> 0.75 (rand))
                (or (nil? lv) (= lv j)))
          (swap! *r assoc (gen-ns-id (str "step-" j)) [(gen-id (str "s-" v)) {}])
          (swap! *r assoc (gen-ns-id (str "step-" j)) [(gen-id (str "s-" v)) (gen-ns-id (str "step-" lv))]))

        ;;(swap! *r assoc (keyword (str *ns* "step-" j)) [(keyword (str "s-" v)) {}])
        (swap! *idx disj v)
        (if-not (empty? @*idx)
          (do
            (recur (inc j)))
          @*r)))))



(defn- gen-steps-and-context [N ]
  (let [xpand-sample-fn (fn[x]
                          (random-sample 0.333 x))
        expand-step-sample-fn (fn[x]
                                (random-sample 0.05 x))
        link-sample-fn (fn[x]
                         (random-sample 0.123 x))

        idxs (take N (range))
        xpand-idxs (xpand-sample-fn idxs)]

    {
      :context (merge
                 (prepare-step-fns idxs)
                 (prepare-expand-fns idxs xpand-idxs expand-step-sample-fn))
      :steps (merge
               (prepare-steps idxs link-sample-fn)
               (reduce (fn[a [i x]]
                         (assoc a (gen-ns-id (str "x-" i)) [(keyword (str "xpand-" x)) {}]))
                       (array-map)
                       (map-indexed vector xpand-idxs)))}
      )
)


(defn get-test-steps-and-context [N]
  (let [d (gen-steps-and-context N)]
    (if (g/has-cycles (:steps d))
      (recur N)
      d)))


;;
;; step handlers for tests




(defn gen-expand-wf [xpand-values]
  (let [*nested-i (volatile! 1)]
     {:context
   {
     :x-str {:fn (fn [s] (str s))}

     :x-expand-sync {:fn (fn [vs]
                           (into (array-map)
                                 (map-indexed (fn[i a] [(gen-ns-id (str "x-sync-xpand-" i))
                                                        [:x-str a]])
                                              vs)))
                     :expands? true}

     :x-expand-async {:fn (fn [vs]

                            (let [c (async/chan)]
                              (go-loop []
                                       (let [v (async/<! (u/timeout 1000))]

                                         (async/put! c (into (array-map)
                                                             (map-indexed (fn[i a] [(gen-ns-id (str "x-async-xpand-" i))
                                                                                    [:x-str a]])
                                                                          vs)))
                                         (recur)))
                              c))
                      :expands? true}


     :x-nested-expand-sync {:fn (fn [actions]
                                  (let [start-i @*nested-i]

                                    (vswap! *nested-i + (count actions))


                                    (if (u/sid-list? actions)
                                      (into (array-map)
                                            (map-indexed (fn[i a] [(gen-ns-id (str (name a) "-nested-"
                                                                                   (+ start-i i)))
                                                                   [:x-str a]])
                                                         actions))
                                      actions)
                                    )
                                  )
            :expands? true
    }
   }
   :steps (assoc (array-map)
            ::sync-xpand  [:x-expand-sync xpand-values]
            ::sync-nested-xpand [:x-nested-expand-sync ::sync-xpand]


            ::async-xpand [:x-expand-async xpand-values]
            ::async-nested-xpand [:x-nested-expand-sync ::async-xpand]

                                    ;; ::8 [:8 10]
                                    ;:xpand-1 [:exp1 (list (test-data/gen-ns-id "woof"))]

                                    ;;:u [:exp "Hello!"]


                                    ;;::h [:h "============="]
                                    ;;::z [:hello-wait "1234"]

            )}
    )

  )

