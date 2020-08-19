;; numerimap/core.clj
;; AndrewJ 2020-07-25

(ns numerimap.core
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]))

;; Define the numeric map type as a map of keywords and numbers.
(s/def ::nmap (s/map-of keyword? number?))

;;-----------------------
(defn m-apply
  "Apply a unary function to the values of the map."
  [f m]
  (apply f (vals m)))

;;-----------------------
;; Simple unary functions

(def m-min (partial m-apply min))
(def m-max (partial m-apply max))
(def m-sum (partial m-apply +))

(defn m-map
  "Map a function across the values of the map."
  [f m]
  (into {} (map (juxt key (comp f val))) m))

;;-----------------------
(def m-union merge-with)

(defn m-intersection
  "Return a map with pairwise function f of the values of the intersection of keys in the two maps. This is the intersection version of `merge-with`."
  [f m1 m2]
  (into {}
        (for [k (set/intersection (set (keys m1)) (set (keys m2)))]
          {k (f (k m1) (k m2))})))

;;-----------------------
;; Basic multi-argument functions
(def m-add (partial m-union +))
(def m-sub (partial m-union -))
(def m-mul (partial m-intersection *)) ;; pairwise (Hadamard) product
(def m-dot (comp m-sum m-mul)) ;; dot product

;;-----------------------
(defn m-enumerate
  "For each pair [k v] in a numeric map, add v copies of k, and concatenate into a single list."
  [m]
  (reduce-kv
   (fn [m k v]
     (into m (repeat v k))) [] m))

;;-----------------------
(defn m-collect
  "Collect a list into a numeric map of counts. This is the opposite of `m-enumerate`."
  [lst]
  (into {} (map (fn [[k v]] [k (count v)])
                (group-by identity lst))))

;; The End
