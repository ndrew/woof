(ns woof.test-data
  (:require
    [woof.data :as d]
    [woof.wf :as exec]
    [woof.utils :as u]

    #?(:clj [clojure.core.async :as async :refer [go]])
    #?(:cljs [clojure.core.async :as async]))

  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]])))




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
                                 (println "FN" k i v)
                                 (let [c (async/chan)
                                       t (+ (int (rand 50))
                                            (if (odd? i) (int (rand 3000)) 0))]
                                   (go
                                     (async/<! (u/timeout t))
                                     (println k "DONE!")
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
                (nil? lv))
          (swap! *r assoc (gen-ns-id (str "step-" j)) [(gen-id (str "s-" v)) {}])
          (swap! *r assoc (gen-ns-id (str "step-" j)) [(gen-id (str "s-" v)) (gen-ns-id (str "step-" lv))]))

        ;;(swap! *r assoc (keyword (str *ns* "step-" j)) [(keyword (str "s-" v)) {}])
        (swap! *idx disj v)
        (if-not (empty? @*idx)
          (do
            (recur (inc j)))
          @*r)))))



(defn get-test-steps-and-context [N]
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
                       (map-indexed vector xpand-idxs)))}))


