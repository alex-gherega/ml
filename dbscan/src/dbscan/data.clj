(ns dbscan.data
  (:require [clojure.math.numeric-tower :as math]))


(defn rec [id raw]
  "Treat the output of this function as a map-entry"
  {:id id :data raw})

(defn coerce [datavec]
  (loop [id 0, result [], datavec datavec]
    (if (-> datavec seq not)
      result
      (recur (inc id)
             (conj result (rec id (first datavec)))
             (rest datavec)))))

(defn update [dsp dataset]
  (assoc dataset (:id dsp) dsp))

