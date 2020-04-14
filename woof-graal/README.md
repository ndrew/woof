# woof-graal

packaging your workflow as a binary via GraalVM — **WIP**

* sample workflow is implemented in src/workflow.clj 
* run build.sh for to generate a workflow binary
    * provide your $GRAALVM_HOME there
    * do not forget to have native image installed for GraalVM (via gu install native-image)

Now packaging is working only for clojure 1.10.2-alpha1 (see [CLJ-1472](https://clojure.atlassian.net/browse/CLJ-1472))
and GraalVM 19.3.
 
GraalVM 20.0 has following problems now: 
* [bug](https://github.com/oracle/graal/issues/2177)
* [performace issue](https://github.com/oracle/graal/issues/2136)

## Future work

* use some knowledge from (clj-graal-docs)[https://github.com/lread/clj-graal-docs]
* investigate if any optimizations could be made to a base library, like adding :gen-class or disabling reflection

---------------

built via [clj.native-image](https://github.com/taylorwood/clj.native-image)
