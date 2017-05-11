(ns dbscan.core
  (:use [dbscan.utils])
  (:require [dbscan.data :as data]
            [dbscan.math :as math]))


; .................................... DBSCAN
(declare query-region)
(declare expand-cluster)

(defn- swap-update [dsp a-datavec]
  (swap! a-datavec #(data/update dsp %)))

(defn explore [dsp a-datavec cid config ;eps min-pts distance-fn
               ]
  (let [neighbors (query-region dsp a-datavec config ;eps distance-fn
        )
        eps (config :eps)
        min-pts (config :min-pts)
        distance-fn (config :distance-fn)
        n-cid (next-cluster cid)]

    (swap-update (mark-visited dsp) a-datavec)
    
    (if (-> neighbors count (< min-pts))
      (do (swap-update (mark-noise dsp) a-datavec) cid)
      (expand-cluster dsp neighbors a-datavec n-cid config;eps min-pts distance-fn
                      ))))

(defn- dbrecur [index a-datavec clusters config;eps min-pts distance-fn
                ]
  (let [dsp (@a-datavec index)
        ;_ (println "DBRECUR: " dsp)
        ]
    (if (is-visited? dsp)
      clusters
      (conj clusters (explore dsp a-datavec (last clusters) config;eps min-pts distance-fn
                              )))))

;; DBSCAN(D, eps, MinPts) {
;;    C = 0
;;    for each point P in datavec D {
;;       if P is visited
;;          continue next point
;;       mark P as visited
;;       NeighborPts = regionQuery(P, eps)
;;       if sizeof(NeighborPts) < MinPts
;;          mark P as NOISE
;;       else {
;;          C = next cluster
;;          expandCluster(P, NeighborPts, C, eps, MinPts)
;;       }
;;    }
;; }
(defn dbscan
  ([a-datavec eps min-pts]
   (dbscan a-datavec {:eps eps :min-pts min-pts :distance-fn math/ecld-distance}))
  ([a-datavec {:keys [eps min-pts distance-fn] :as config}                ;eps min-pts distance-fn
    ]
   "Parameters explained: 
 * a-datavec: an atom holding the data as-a vector
 * eps: epsilon param in the DBSCAN alg
 * min-pts: MinPts param in the DBSCAN alg"
   
   (loop [clusters [0]
          n (count @a-datavec)]
                                        ;(println "DBSCAN: " n clusters)
     (println "CLUSTERS: " (count clusters))
     (if (zero? n)
       (get-clusters @a-datavec)                             ;clusters
       (recur (dbrecur (dec n) a-datavec clusters config;eps min-pts distance-fn
                       )
              (dec n))))))

(def discover-neighbors
  (memoize
   (fn [dsp neighbors a-datavec config;eps min-pts distance-fn
        ]
     (let [n-neighbors (query-region dsp a-datavec config ;eps
                                     )
         
           min-pts (config :min-pts)]
       (swap-update (-> dsp mark-visited) a-datavec)
       (if (-> n-neighbors count (>= min-pts))
         (into (rest neighbors) n-neighbors)
         (rest neighbors))))))

(defn- discover-points [neighbors a-datavec cid config;eps min-pts distance-fn
                        ]
  (loop [neighbors neighbors
         n (first neighbors)
         ]
    (if (-> neighbors seq not)
      cid
      (do (when (-> n :cluster not)
            (swap-update (-> n (assoc :cluster cid)) a-datavec))
          (if (-> n :visited not)
            (recur (discover-neighbors n neighbors a-datavec config;eps min-pts distance-fn
                                       )
                   (first (discover-neighbors n neighbors a-datavec config;eps min-pts distance-fn
                                              )))
            (recur (rest neighbors) (-> neighbors rest first)))))))

;; expandCluster(P, NeighborPts, C, eps, MinPts) {
;;    add P to cluster C
;;    for each point P' in NeighborPts { 
;;       if P' is not visited {
;;          mark P' as visited
;;          NeighborPts' = regionQuery(P', eps)
;;          if sizeof(NeighborPts') >= MinPts
;;             NeighborPts = NeighborPts joined with NeighborPts'
;;       }
;;       if P' is not yet member of any cluster
;;          add P' to cluster C
;;    }
;; }

(defn expand-cluster [dsp neighbors a-datavec cid config;eps min-pts distance-fn
                      ]
  (swap-update (add-cluster dsp cid) a-datavec)
  (discover-points neighbors a-datavec cid config;eps min-pts distance-fn
                   )
  cid)




;; TODO:
;; regionQuery(P, eps)
;;    return all points within P's eps-neighborhood (including P)

(defn query-region [dsp a-datavec config;eps distance-fn
                    ]
  (let [eps (config :eps)
        distance-fn (config :distance-fn)
        filter-pred (fn [dsp-p dsp-q]
                      (math/is-in-distance? distance-fn ;;math/ecld-distance
                                            eps
                                            (:data dsp-p)
                                            (:data dsp-q)))]
    (filter (partial filter-pred dsp) @a-datavec)))
