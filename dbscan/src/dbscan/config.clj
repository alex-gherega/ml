(ns dbscna.config
  (:require [dbscan.math :as math]))

(def default-map {:eps 0 :min-pts 0 :distance-fn math/ecld-distance})
