#!/usr/bin/env bash


# provide your own path to woof-core
export WOOF_CORE_SRC=/Users/ndrw/m/woof/woof-core/src
# provide path for andare - cljs only version of core async
export ANDARE_JAR=/Users/ndrw/.m2/repository/andare/andare/1.1.587/andare-1.1.587.jar


lumo -c "src:$WOOF_CORE_SRC:$ANDARE_JAR" build.cljs

# node workflow.js