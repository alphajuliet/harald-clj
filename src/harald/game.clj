;; game.clj

(ns harald.game
  (:require [harald.state :refer :all]
            [harald.actions :refer :all]
            [harald.hash-calc :as h]
            [lentes.core :as l]
            [random-seed.core :as r]
            [clojure.math.combinatorics :as c]
            [clojure.java.io :as io])
  (:refer-clojure :exclude [rand rand-int rand-nth]))

;;-------------------------------
;; Logging setup

(def log-file-name "game.txt")
(io/delete-file log-file-name :quiet)

(defn- spit-seq
  "Convert a sequence to a string in parentheses."
  [dest s]
  (spit dest
        (str "(" (clojure.string/join " " s) ")\n")
        :append true))

(defn log
  "Print a string to a log file."
  [s]
  (if (seq? s)
    (spit-seq log-file-name s)
    (spit log-file-name (str s "\n")
          :append true)))

;;-------------------------------
;; Utilities

(defn val-combinations
  "Return all combinations of the enumerated values in a numeric map of length `n`."
  [m n]
  (distinct (c/combinations (h/hash-enumerate m) n)))

(defn val-permutations
  "Return all permutations of length `n` of the enumerated values in numeric map `m`."
  [m n]
  (distinct (c/permuted-combinations (h/hash-enumerate m) n)))

;;-------------------------------
;; Generate action options

;;-------------------------------
; play-card-options :: Player -> State -> [Action]
(defn play-cards-options
  "Generate all play-card actions for a player `player`."
  [player st]
  (for [c (val-permutations (l/focus (_hand player) st) 2)]
    {:action :play-cards
     :player player
     :cc (first c)
     :cv (second c)}))

;;-------------------------------
; take-reserve-card-options :: Player -> State -> [Action]
(defn take-reserve-card-options
  "Generate all take-reserve-card actions."
  [player st]
  (for [c (keys (l/focus _reserve st))]
    {:action :take-reserve-card
     :player player
     :card c}))

;;-------------------------------
; get-turnover-locations :: State -> [Lens]
(defn- get-turnover-locations
  "Get candidate locations to turn over cards."
  [st]
  (-> (for [i (range (count (:villages st)))]
        (list '_village i))
      (into '(_council))))

(defn get-cards [_loc st] (l/focus _loc st))

; turn-over-cards-options :: State -> [Action]
; TODO to be completed
(defn turn-over-cards-options
  "Generate all turn-over-cards actions."
  [st]

  #_(for [p (nplayers st)
        c (h/hash-enumerate (l/focus (_village p) st))])
  
  ; Get all the potential locations
  (let [locs (get-turnover-locations st)
        srcs (into (c/combinations locs 1)
                   (c/combinations locs 2))]
    (map (fn [e] (map #(l/focus (eval %) st) e)) 
         srcs))


  #_(defn f [n]
      (let [cards (hash-enumerate (view (lookup-src n) st))]
        (map (fn (e) (list e (lookup-src n))) cards)))

  #_(for [p pairs]
      {:action :turn-over-cards
       :cards p}))

;;-------------------------------
(defn return-card-options 
  "Generate all the options for returning any village card."
  [st]
  (for [p (range (nplayers st))
        c (h/hash-enumerate (l/focus (_village p) st))]
    {:action :return-card
     :player p
     :card c}))

;;-------------------------------
(defn swap-hand-village-options
  "Generate all options for swapping a hand card with a village card."
  [player st]
  (for [ch (h/hash-enumerate (l/focus (_hand player) st))
        cv (h/hash-enumerate (l/focus (_village player) st))]
    {:action :swap-hand-village
     :player player
     :ch ch
     :cv cv}))

;;-------------------------------
(defn swap-village-council-options
  "Generate all options for swapping a village card with a council card."
  [st]
  (for [p (range (nplayers st))
        cv (h/hash-enumerate (l/focus (_village p) st))
        cc (h/hash-enumerate (l/focus _council st))]
    {:action :swap-village-council
     :player p
     :cv cv
     :cc cc}))

;;-------------------------------
(defn swap-village-village-options
  "Generate all options for swapping two cards from different villages."
  [st]
  (for [p1 (range (nplayers st))
        c1 (h/hash-enumerate (l/focus (_village p1) st))
        p2 (range (nplayers st))
        c2 (h/hash-enumerate (l/focus (_village p2) st))
        :when (< p1 p2)
        :when (not (= c1 c2))]
    {:action :swap-village-village
     :p1 p1
     :c1 c1
     :p2 p2
     :c2 c2}))

;; The End