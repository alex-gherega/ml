(ns dbscan.test
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [dbscan.core :as dbs]
            [dbscan.data :as data]))

(defn read-raw [path]
  (with-open [in-file (io/reader path)]
    (doall
     (csv/read-csv in-file))))

(defn process [raw]
  (let [raw (-> raw vec rest vec)]
    (vec (map #(vec (map read-string %)) raw))))

(def raw (read-raw "resources/iris2.csv"))

(def parsed (atom (data/coerce (process raw))))

(println (dbs/dbscan parsed 0.1 2))
