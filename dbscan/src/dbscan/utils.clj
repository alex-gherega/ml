(ns dbscan.utils)

(defn from-csv [path]
  )

(defn is-visited? [dsp] ;; dsp -> DBSCAN point
  (if (:visited dsp) true false))

(defn next-cluster [cid] ;; cid -> cluster id
  (inc cid))

(defn mark [dsp k v]
  (assoc dsp k v))

(defn mark-visited [dsp]
  (mark dsp :visited true))

;; dsp points have 3 classes: core, border, noise
(defn mark-core [dsp] 
  (mark dsp :class :core))

(defn mark-border [dsp] 
  (mark dsp :class :border))

(defn mark-noise [dsp] 
  (mark dsp :class :noise))

(defn add-cluster [dsp cid]
  (mark dsp :cluster cid))

(defn get-clusters [datavec]
  (reduce (fn [x y] (if-let [y (:cluster y)]
                      (conj x y) x))
          #{} datavec))
