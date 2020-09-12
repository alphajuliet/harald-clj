;; state.clj
;; AndrewJ 2019-11-04

(ns harald.state
  (:require [numerimap.core :as n]
            [spec-dict :refer [dict dict*]]
            [clojure.spec.alpha :as s]))

;;-----------------------
;; Cards are:
;; - Blacksmith/Wild Boar -> Blk (red)
;; - Warrior/Bear         -> War (blue)
;; - Bard/Fox             -> Brd (yellow)
;; - Seafarer/Goat        -> Sea (cyan)
;; - Merchant/Lynx        -> Mer (purple)
;; - Scout/Wolf           -> Sct (green)

(def card-types [:blk :war :brd :sea :mer :sct])
(def all-cards (vec (concat card-types [:blkX :warX :brdX :seaX :merX :sctX])))
(def null-hand (zipmap all-cards (repeat 0)))

;; A Hand is a collection of zero or more cards for each character.
;; Stored as a hash table.
;; Examples of hands are: the council, each player's village, each player's
;; own hand, and the reserve pile available for restocking a player's hand.
(s/def ::hand (s/map-of all-cards nat-int?))

;;-----------------------
(s/def ::state (dict {:council ::hand
                      :village (s/+ ::hand)
                      :hand (s/+ ::hand)
                      :reserve ::hand
                      :score (s/+ nat-int?)
                      :turn nat-int?}))

(defn empty-state
  "Game state."
  [nplayers]
  {:pre [(<= 2 nplayers 4)]}

  {:council {}
   :village (vec (repeat nplayers {}))
   :hand (vec (repeat nplayers {}))
   :reserve {}
   :score (vec (repeat nplayers 0))
   :turn 0})

(defn nplayers
  "Quick way to count the number of players."
  [st]
  (count (:village st)))

;;-----------------------
;; score :: Hand -> Hand -> Integer
(defn score
  "Score a player hand against a reference hand."
  [ref player]
  (let [scoring-multiplier {:blk  1 :war  1 :brd  1 :sea  1 :mer  1 :sct  1
                            :blkX 0 :warX 0 :brdX 0 :seaX 0 :merX 0 :sctX 0}]
    (n/m-sum (n/m-mul scoring-multiplier (n/m-mul ref player)))))

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
  {:pre [(s/valid? ::state st)]}
  (vec (scores (:council st)
               (:village st))))

;; end-of-game? :: State -> Boolean
(defn end-of-game?
  "Determine end of game"
  [st]
  {:pre [(s/valid? ::state st)]}
  (>= (:turn st) 10))

;; @@TODO: implement bonus point scoring
;;
;; Rules
;; 1. +4 points if more warriors than blacksmiths in your village
;; 2. +1 point per bard in your village.
;; 3. +4 points if more bards than merchants in your village.
;; 4. +4 points if even number of scouts and seafarers in your village.
;; 5. +1 point per seafarer in your village.
;; 6. +2 points per face-down card in your village.

;; end-score :: State -> State
(defn end-score
  "Score the final state with bonuses."
  [st]
  st)

;;-----------------------
;; encode-hand :: Hand -> Vector
(defn encode-hand
  "Encode the state as a vector with all types."
  [h]
  (vec (vals (n/m-add null-hand h))))

;; encode-state :: State -> Vector (Vector nat-int?)
(defn encode-state
  "Encode state in a compact form."
  [st]
  {:pre [(s/valid? ::state st)]}
  (vector (encode-hand (:council st))
          (vec (flatten (map encode-hand (:hand st))))
          (vec (flatten (map encode-hand (:village st))))
          (encode-hand (:reserve st))))

;; The End
