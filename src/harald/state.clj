;; state.clj
;; AndrewJ 2019-11-04

(ns harald.state
  (:require [harald.hash-calc :as h]
            [lentes.core :as l]))

;;-----------------------
;; Cards are:
;; - Blacksmith/Wild Boar -> Blk (red)'
;; - Warrior/Bear         -> War (blue)
;; - Bard/Fox             -> Brd (yellow)
;; - Seafarer/Goat        -> Sea (cyan)
;; - Merchant/Lynx        -> Mer (purple)
;; - Scout/Wolf           -> Sct (green)

(def card-types [:blk :war :brd :sea :mer :sct])
(def all-cards [:blk :war :brd :sea :mer :sct
                :blkX :warX :brdX :seaX :merX :sctX])
(def null-hand {:blk 0 :war 0 :brd 0 :sea 0 :mer 0 :sct 0
                :blkX 0 :warX 0 :brdX 0 :seaX 0 :merX 0 :sctX 0})

;; A Hand is a collection of zero or more cards for each character. Stored as a hash table.
;; Examples of hands are: the council, each player's village, each player's own hand, and the
;; reserve pile available for restocking a player's hand.

;; State :: Map Card Hand
(defn empty-state
  "Game state."
  [nplayers]
  {:council {}
   :villages (vec (repeat nplayers {}))
   :hands (vec (repeat nplayers {}))
   :reserve {}
   :turn 0})

;;-----------------------
;; Define some lenses

(defn _hand
  "A lens to hand n."
  [n]
  (comp (l/key :hands) (l/nth n)))

(defn _village
  "A lens to village n."
  [n]
  (comp (l/key :villages) (l/nth n)))

(defn _hand_card [n t] (comp (_hand n) (l/key t)))
(defn _village_card [n t] (comp (_village n) (l/key t)))
(def _reserve (l/key :reserve))
(def _council (l/key :council))

;;-----------------------
;; score :: Hand -> Hand -> Integer
(defn score
  "Score a player hand against a reference hand."
  [ref player]
  (let [scoring-multiplier {:blk 1 :war 1 :brd 1 :sea 1 :mer 1 :sct 1
                            :blkX 0 :warX 0 :brdX 0 :seaX 0 :merX 0 :sctX 0}]
    (h/hash-sum (h/hash-mul scoring-multiplier
                            (h/hash-mul ref
                                        player)))))

;; scores :: Hand -> [Hand] -> [Integer]
(defn scores
  "Show player scores."
  [ref villages]
  (map (partial score ref) villages))

;;-----------------------
;; score-states :: State -> [Integer]
(defn score-state
  "Score the current state"
  [st]
  (vec (scores (:council st)
               (:villages st))))

;;-----------------------
(defn encode-hand
  "Encode the state as a vector with all types."
  [h]
  (vals (h/hash-add null-hand h)))

(defn encode-state
  "Encode state in a compact form."
  [s]
  (conj (encode-hand (:council s))
        (flatten (map encode-hand (:hands s)))
        (flatten (map encode-hand (:villages s)))
        (encode-hand (:reserve s))))

;; The End