(require '[lumo.build.api :as b])

(b/build "src"
  {:main 'woof.node
   :output-to "workflow.js"
   ;:optimizations :none
   :optimizations :advanced
   :target :nodejs})