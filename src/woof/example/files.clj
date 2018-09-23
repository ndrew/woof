(ns woof.example.files
  "woof client/server communication endpoint."
  (:require
    [compojure.core :as compojure]
    [compojure.route :as route]
    [org.httpkit.server :as httpkit]
    [ring.util.response :as response]
    [cognitect.transit :as t]

    [clojure.core.async :as async :refer [go go-loop]]
    [clojure.test :refer :all]

    [woof.data :as d]
    [woof.wf :as wf]
   ; [woof.wf-data :as wdata]

    [woof.utils :as u]
    [woof.test-data :as test-data]

    [woof.server.utils :refer [read-transit-str write-transit-str]]

    [me.raynes.fs :as fs]

    )
)


(defn prepare-content-map
  "returns the context config for the ws workflow"
  [socket-chan in out-chan<]  ;; context-map

  (merge {  ;; use context 'shared' between client and server

      ;; identity function
      :v  {:fn (fn[a] (identity a))}

      ;; debug
      :log  {:fn (fn[a]
                   (locking *out* (println "DBG:" a))
                   (identity a))}

      ;; reference
      :&v {:fn (fn[a]
                 {(wf/rand-sid) [:v a]})
           :expands? true}

      ;; collect
      :v* {:fn identity :collect? true}

      ;; zipper
      :zip {:fn (fn [vs]
                  (partition (count vs) (apply interleave vs)))
            :collect? true}


      ;; older steps
      :debug (wf/step-handler (fn[x]
                                (println "DBG: " (d/pretty x))
                                x))

      :server-time (wf/step-handler (fn[x]
                                [:server-time (u/now)]))

      }

        {

           ;; expand step that adds steps to the current ws
           :client<  (wf/receive-steps-handler
                       :step-fn (fn [steps]
                                  (println "FILES RECEIVE: " (d/pretty steps))
                                  steps))


           ;; step that sends the value back to client
           :client> (wf/send-value-handler out-chan<
                                           :v-fn (fn[v]
                                                   (println "SERVER SEND: " (d/pretty v))
                                                   v)
                                           :out-fn (fn [z]
                                                     (println "SEND TO SOCKET:" z)
                                                     (httpkit/send! socket-chan (write-transit-str z))
                                            ))

           ;; todo :client>*

           }))


(defn prepare-steps
  "returns initial steps for the ws workflow"
  [in-chan> out]

  {
    ;; optional initialization can be done here
    ::receive-loop [:client< in-chan>]

    ::initial-cwd [:v [:cwd (.getAbsolutePath (fs/home))]]

    ::client-init [:client> ::initial-cwd]

    }
  )
