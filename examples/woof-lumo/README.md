### `woof-lumo`

Example of packaging woof workflow for node js via lumo
 
run as 

    lumo -c "src:$WOOF_CORE_SRC:$ANDARE_JAR" build.cljs

where
* WOOF_CORE_SRC=<path-to-woof-core>/src
* ANDARE_JAR=~/.m2/repository/andare/andare/1.1.587/andare-1.1.587.jar

or via build.sh

run via node

    node workflow.js

