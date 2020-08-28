;; game.clj

(ns harald.game
  (:require [harald.state :as st]
            [harald.actions :as act]
            [harald.hash-calc :as h]
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
  "Return all combinations of the enumerated values in a numeric
   map of length `n`."
  [m n]
  (distinct (c/combinations (h/hash-enumerate m) n)))

(defn val-permutations
  "Return all permutations of length `n` of the enumerated values in
   a numeric map `m`."
  [m n]
  (distinct (c/permuted-combinations (h/hash-enumerate m) n)))

;;-------------------------------
;; Generate action options

;;-------------------------------
;; play-card-options :: Player -> State -> [Action]
(defn play-cards-options
  "Generate all play-card actions for a player `player`."
  [player st]
  (for [c (val-permutations (get-in st [:hand player]) 2)]
    {:action :play-cards
     :player player
     :cc (first c)
     :cv (second c)}))

;;-------------------------------
;; take-reserve-card-options :: Player -> State -> [Action]
(defn take-reserve-card-options
  "Generate all take-reserve-card actions."
  [player st]
  (for [c (keys (:reserve st))]
    {:action :take-reserve-card
     :player player
     :card c}))

;;-------------------------------
;; get-pairs :: LensX -> State -> [[Card LensX]]
(defn- get-pairs
  [h st]
  (->> (keys (get-in st h))
       (map #(vector % h))))

;; get-location-cards :: State -> [[Card LensX]]
(defn- get-location-cards
  "Return all the locations where cards can be turned over."
  [st]
  (->> (range (nplayers st))
       (map #(get-pairs [:villages %] st))
       (reduce #(into %1 %2)
               (get-pairs :council st))))

;; different-pile? :: ∀ a b, [[a b] [a b]] -> Boolean
(defn- different-pile?
  "Ensure pairs of cards are in different piles."
  [pair]
  (not (= (second (first pair))
          (second (second pair)))))

;; turn-over-cards-options :: State -> [Action]
(defn turn-over-cards-options
  "Generate all turn-over-cards actions."
  [st]
  (->> (c/combinations (get-location-cards st) 2) ; options for 2 cards
       (filter different-pile?)
       (into (get-location-cards st)) ; options for 1 card
       (cons [])                      ; options for 0 cards
       (map (fn [p] {:action :turn-over-cards
                    :cards p}))))

;;-------------------------------
;; return-card-options :: State -> [Action]
(defn return-card-options
  "Generate all the options for returning any village card."
  [st]
  (for [p (range (nplayers st))
        c (h/hash-enumerate (get-in st [:village p]))]
    {:action :return-card
     :player p
     :card c}))

;;-------------------------------
;; swap-hand-village-options :: Int -> State -> [Action]
(defn swap-hand-village-options
  "Generate all options for swapping a hand card with a village card."
  [player st]
  (for [ch (h/hash-enumerate (get-in st [:hand player]))
        cv (h/hash-enumerate (get-in st [:village player]))]
    {:action :swap-hand-village
     :player player
     :ch ch
     :cv cv}))

;;-------------------------------
;; swap-village-council-options :: State -> [Action]
(defn swap-village-council-options
  "Generate all options for swapping a village card with a council card."
  [st]
  (for [p (range (nplayers st))
        cv (h/hash-enumerate (get-in st [:village p]))
        cc (h/hash-enumerate (:council st))]
    {:action :swap-village-council
     :village p
     :cv cv
     :cc cc}))

;;-------------------------------
(defn swap-village-village-options
  "Generate all options for swapping two cards from different villages."
  [st]
  (for [v1 (range (nplayers st))
        c1 (h/hash-enumerate (get-in st [:village v1]))
        v2 (range (nplayers st))
        c2 (h/hash-enumerate (get-in st [:village v2]))
        :when (< v1 v2)
        :when (not (= c1 c2))]
    {:action :swap-village-village
     :v1 v1
     :cv1 c1
     :v2 v2
     :cv2 c2}))

;;-------------------------------
;; available-actions :: Int -> State -> [Action]
(defn available-actions
  "List all the available actions for `player` given the `state`."
  [player st]
  (play-cards-options player st))


;; The End
