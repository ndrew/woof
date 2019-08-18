(ns woof.core.messages
  "woof message handling"
  (:require ;[woof.wf :as wf]

            ;#?(:clj [woof.utils :as u :refer [put!? debug! inline--fn inline--fn1]])
            ;#?(:cljs [woof.utils :as u])


            #?(:clj [clojure.core.async :as async :refer [go go-loop]])
            #?(:cljs [cljs.core.async :as async])
            )

  #?(:cljs
     (:require-macros
       [cljs.core.async.macros :refer [go go-loop]]
       [woof.utils-macros :refer [put!? debug! inline--fn inline--fn1]]
       )))


;;; todo: move all the executor stuff here