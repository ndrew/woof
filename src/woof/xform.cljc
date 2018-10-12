(ns woof.xform
  "woof transducer/reducers stuff"
  (:require [woof.data :as d]

            #?(:clj [woof.utils :as u :refer [put!? debug! inline--fn inline--fn1]])
            #?(:cljs [woof.utils :as u])

            #?(:clj [clojure.core.async :as async :refer [go go-loop]])
            #?(:cljs [cljs.core.async :as async]))

  #?(:cljs
      (:require-macros
          [cljs.core.async.macros :refer [go go-loop]]
          [woof.utils-macros :refer [put!? debug! inline--fn inline--fn1]]
        )))

;;
;; higher order funcs that generate the step handler functions


; woof workflow and transducers
;
; can transducers be used in the step handler function?

; is woof workflow a transducible process ?
;   — a process that is a succession of steps where each step ingests an input


; transducing fn

;; init []
;; step [a b]
;; completion [a]


; rules for applying transducers

; * If a step function returns a reduced value,
;;    the transducible process must not supply any more inputs to the step function.
;;  The reduced value must be unwrapped with deref before completion.
; * A completing process must call the completion operation on the final accumulated value exactly once.
; * A transducing process must encapsulate references to the function returned by invoking a transducer - these may be stateful and unsafe for use across threads.


;; so,

;; step handler function can be made from a transducer (xf or xform), where
;; reducing function
;;   * has signature (whatever, input -> whatever)
;;   * is a func that takes accumulated result and a new input and returns a new accumulated result
;;     or less formal
;;     reducible is a recipe for how to transform a given collection
;;


;; ?
;; reducing function transformers
;; algorithmic combinators
;; separate transformation composition from transformation application



(defn local-shandler
  "defines a step handler function from xform

  init is called per each step handler invocation,
  before transforming input and wrapping the result

  like:
  (xf)
  (xf input (xf input))"

  [xform]
  (fn [input]
    (xform)
    (xform input (xform input))
    ))


;; transducer with reduction state

(defn global-shandler
  "defines a step handler function from xform

  init is called once prior to creating step handler function.
  each step handler invocation triggers step and complete
  "
  [rf]

  (rf)
  (fn [input]
    (rf input (rf input))))




;; xform that waits for values via channel
;;
;; transuder
;; init [] - return a channel
;; step [v] -
;; completion [] - not used
(defn infinite-expand-rf [rf]
  (let [chan> (async/chan)]
    (fn
      ([]
       (let [in-chan (rf)]
         (locking *out* (println "STARTED infinite loop:"))
         (go-loop []
                  (if-let [v (async/<! in-chan)]
                    (do
                      (locking *out* (println "GOT:" v))
                      (if-let [z (rf v)]
                        (async/put! chan> z))
                      (recur)
                      )
                    )
                  ))
       )
      ([v]
       chan>)
      ([initial out-chan]
       (if-let [v (rf initial)]
         (go
            (locking *out* (println "SEND from infinite loop:" v))

           (async/put! out-chan v)))
       out-chan
      ))
  ))


;; todo:

(defn channel-collect-rf [rf]
  (let []
    (fn
      ([]
       (let [out-chan< (rf)]
         (go-loop []
             (if-let [v (async/<! out-chan<)] ;; redirect the wf output onto wire
               (do
                 (inline--fn1 rf v)
                 (recur))
               (do
                 (rf out-chan< nil)
                 )
               )
             ))

       )
      ([v]
       (if-let [vs v]
         (do
           (go ;; send the new value to out-chan<
             (async/>! (rf) vs))
           vs
           )
         v
         )
       )
      ([a b]
       (rf a b)
       )
      )
    )
  )


