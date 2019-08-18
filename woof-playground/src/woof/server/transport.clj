(ns woof.server.transport
  "woof server transport stuff"
  (:require
    [cognitect.transit :as t])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream)))


(defn read-transit-str [s]
  (-> s
      (.getBytes "UTF-8")
      (ByteArrayInputStream.)
      (t/reader :json)
      (t/read)))


(defn write-transit-str [o]
  (let [os (ByteArrayOutputStream.)]
    (t/write (t/writer os :json) o)
    (String. (.toByteArray os) "UTF-8")))
