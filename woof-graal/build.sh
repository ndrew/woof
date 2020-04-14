#!/usr/bin/env bash


#export GRAALVM_HOME=/Library/Java/JavaVirtualMachines/graalvm-ce-java11-20.0.0/Contents/Home

# use version 19, because of https://github.com/oracle/graal/issues/2177
export GRAALVM_HOME=/Library/Java/JavaVirtualMachines/graalvm-ce-java11-19.3.1/Contents/Home


export PATH=$GRAALVM_HOME/bin:$PATH

# do not forget to install native-image via 
#
#   gu install native-image

clojure -A:native-image
#clojure -A:native-image --verbose
