;; state.clj
;; AndrewJ 2019-11-04

(ns harald.state
  (:require [numerimap.core :as n]
            [clojure.spec.alpha :as s]))

;;-----------------------
;; Cards are:
;; - Blacksmith/Wild Boar -> Blk (red)'
;; - Warrior/Bear         -> War (blue)
;; - Bard/Fox             -> Brd (yellow)
;; - Seafarer/Goat        -> Sea (cyan)
;; - Merchant/Lynx        -> Mer (purple)
;; - Scout/Wolf           -> Sct (green)

(def card-types [:blk :war :brd :sea :mer :sct])
(def all-cards (vec (concat card-types [:blkX :warX :brdX :seaX :merX :sctX])))
(def null-hand (zipmap all-cards (repeat 0)))

;; A Hand is a collection of zero or more cards for each character. Stored as a hash table.
;; Examples of hands are: the council, each player's village, each player's own hand, and the
;; reserve pile available for restocking a player's hand.
(s/def ::hand (s/map-of all-cards nat-int?))

;;-----------------------
(defn empty-state
  "Game state."
  [nplayers]
  {:pre (>= nplayers 2)}

  {:council {}
   :village (vec (repeat nplayers {}))
   :hand (vec (repeat nplayers {}))
   :reserve {}
   :score (vec (repeat nplayers 0))
   :turn 0})

(defn nplayers [st] (count (:village st)))

;;-----------------------
;; score :: Hand -> Hand -> Integer
(defn score
  "Score a player hand against a reference hand."
  [ref player]
  (let [scoring-multiplier {:blk  1 :war  1 :brd  1 :sea  1 :mer  1 :sct  1
                            :blkX 0 :warX 0 :brdX 0 :seaX 0 :merX 0 :sctX 0}]
    (n/m-sum (n/m-mul scoring-multiplier
                      (n/m-mul ref
                               player)))))

;; scores :: Hand -> [Hand] -> [Integer]
(defn scores
  "Show player scores."
  [ref villages]
  (map (partial score ref) villages))

;;-----------------------
;; score-states :: State -> [Integer]
(defn score-state
  "Score the current state as a vector of scores."
  [st]
  (vec (scores (:council st)
               (:village st))))

;; end-of-game? :: State -> Boolean
(defn end-of-game?
  "Determine end of game"
  [st]
  (>= [:turn st] 10))

;; TODO: implement bonus point scoring
(defn end-score
  "Score the final state."
  [st]
  st)

;;-----------------------
(defn encode-hand
  "Encode the state as a vector with all types."
  [h]
  (vec (vals (n/m-add null-hand h))))

(defn encode-state
  "Encode state in a compact form."
  [s]
  (vector (encode-hand (:council s))
          (vec (flatten (map encode-hand (:hand s))))
          (vec (flatten (map encode-hand (:village s))))
          (encode-hand (:reserve s))))

;; The End
