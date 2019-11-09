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
  "Return all permutations of the enumerated values in a numeric map of length `n`."
  [m n]
  (distinct (c/permuted-combinations (h/hash-enumerate m) n)))

;;-------------------------------
;; Generate action options

; play-card-options :: Player -> State -> [Action]
(defn play-cards-options
  "Generate all play-card actions for a player `player`."
  [player st]
  (for [c (val-permutations (l/focus (_hand player) st) 2)]
    {:action :play-cards
     :player player
     :cc (first c)
     :cv (second c)}))


;; The End