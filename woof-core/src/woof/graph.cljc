(ns woof.graph
  "graph functions"
  (:require
    [woof.data :as d]
    [woof.utils :as u]

    [viz.core :as viz]))


(defn tarjan
  "Returns the strongly connected components of a graph specified by its nodes
  and a successor function succs from node to nodes.
  The used algorithm is Tarjan's one."
  [nodes succs]
  (letfn [(sc [env node]
              ; env is a map from nodes to stack length or nil, nil means the node is known to belong to another SCC
              ; there are two special keys: ::stack for the current stack and ::sccs for the current set of SCCs
              #_{:post [(contains? % node)]}
              (if (contains? env node)
                env
                (let [stack (::stack env)
                      n (count stack)
                      env (assoc env node n ::stack (conj stack node))
                      env (reduce (fn [env succ]
                                    (let [env (sc env succ)]
                                      (assoc env node (min (or (env succ) n) (env node)))))
                                  env (succs node))]
                  (if (= n (env node)) ; no link below us in the stack, call it a SCC
                    (let [nodes (::stack env)
                          scc (set (take (- (count nodes) n) nodes))
                          env (reduce #(assoc %1 %2 nil) env scc)] ; clear all stack lengths for these nodes since this SCC is done
                      (assoc env ::stack stack ::sccs (conj (::sccs env) scc)))
                    env))))]
    (::sccs (reduce sc {::stack () ::sccs #{}} nodes))))




(defn- graph-component! [steps]
  (let [graph (reduce (fn [a [k [op p]]]
                        (if (u/sid? p)
                          (assoc a k (conj (get a k []) p))
                          (if  (u/sid-list? p)
                            (do
                              ;(assoc a k (concat (get a k []) p))
                              a
                              )
                            a
                            )
                          )) {} steps)]

    ;(println "[GRAPH-CMP]: " graph)

    (tarjan (keys graph) graph)))

(def graph-component (memoize graph-component!))


(defn has-cycles! [steps]
  (let [components (graph-component steps)
        cycles (filter #(> (count %) 1) components)]
    (if-not (empty? cycles)
      cycles)))

(def has-cycles (memoize has-cycles!))


(defn get-dependant-steps [steps initial-step]
  (let [graph (reduce (fn [a [k [op p]]]
                        (if (u/sid? p)
                          (assoc a p (conj (get a p []) k))
                          (if (u/sid-list? p)
                            (reduce (fn [A p*]
                                      (assoc A p* (conj (get a p* []) k)))
                                    a p)
                            a
                            )
                          )) {} steps)]

    (loop [in [initial-step]
           out []]

      (let [z (mapcat #(get graph %) in)]
        (if (empty? z)
          (concat out in)
          (recur z (concat out in)))
        ))))



(defn graph-to-svg [steps rfn]
  (let [graphviz-edges (reduce rfn "" steps)]
    (viz/image (str "digraph { " graphviz-edges " }"))))

