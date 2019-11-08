;; actions.clj
;; AndrewJ 2019-11-04

(ns harald.actions
  (:require [harald.state :refer :all]
            [harald.hash-calc :as h]
            [random-seed.core :as r]
            [lentes.core :as l])
  (:refer-clojure :exclude [rand rand-int rand-nth]))

;;-----------------------
;; Helper functions

;; deal-card :: Card
(defn deal-card
  "Pick a random card."
  []
  {(r/rand-nth card-types) 1})

;; deal-n-cards :: Integer -> Hand -> Hand
(defn deal-n-cards
  "Deal n random cards to a given hand."
  [n init-h]
  (reduce (fn [m _]
            (h/hash-add (deal-card) m))
          init-h
          (range n)))

;; deal-reserve :: State -> State
(defn deal-reserve
  "Deal a card into the Reserve."
  [st]
  (l/over _reserve (partial h/hash-add (deal-card)) st))

(def >>> comp)

;; move-card :: Card -> Hand -> Hand -> State -> State
(defn move-card
  "Move a card of type t from the src hand to the dest hand.
   e.g. (move-card :blk (_hand 1) _council s0)."
  [t _src _dest st]

  ;; Ensure the card is available
  (if (or (not (contains? (l/focus _src st) t))
          (<= (l/focus (comp _src (l/key t)) st) 0))
    (throw (Exception.
            (format "### Error: card %s not available to move" t)))
    (->> st
         (l/over _dest (partial h/hash-add {t 1}))
         (l/over _src (partial h/hash-sub {t 1})))))


;; swap-cards :: Card -> Lens Hand -> Card -> Lens Hand -> State -> State
(defn swap-cards
  "Swap two cards between two hands, i.e. exchange t1 from lens1 with t2 from lens2."
  [t1 _lens1 t2 _lens2 st]
  (->> st
       (move-card t1 _lens1 _lens2)
       (move-card t2 _lens2 _lens1)))

;; invert :: Card -> Card
(defn invert
  "Turn over a card, e.g. (invert 'Sea) => 'SeaX."
  [c]
  (all-cards (mod (+ 6 (.indexOf all-cards c)) 12)))

;; turn-over-card :: Card -> Lens Hand -> State -> State
(defn turn-over-card
  [c _h st]
  (if (= 0 (get-in st (conj _h c)))
    (throw (Exception.
            (format "### Card %s doesn't exist to turn over." c)))
    (-> st
        (update-in _h #(h/hash-sub % {c 1}))
        (update-in _h #(h/hash-add % {(invert c) 1})))))


;===============================
;; Game actions
;; - Init game
;; - Player turn
;;   1. Play hand card to council
;;   2. Play hand card to village
;;   3. Apply effect from the played village card
;;   4. Take card from reserve
;;   5. Deal new card to reserve
;;   6. Take card from reserve
;;
;; Effects
;; - Turn over 0-2 village cards (Blacksmith effect)
;; - Replace any village card with a random card (Warrior effect)
;; - Swap hand card with own village card (Bard effect)
;; - Swap any village card with a council card (Seafarer effect)
;; - Swap your village card with another village card (Merchant effect)
;; - Apply effect from council card instead (Scout effect)


;;----------------------- 
;; init-game :: Integer -> State
(defn init-game
  "Define the initial state."
  [nplayers seed]

  (if-not (<= 2 nplayers 4)
    (throw (Exception. "### The game needs 2-4 players."))
    (r/set-random-seed! seed))

  {:council {}
   :villages (vec (repeat nplayers {}))
   :hands (vec (repeatedly nplayers #(deal-n-cards 4 {})))
   :reserve (deal-n-cards 4 {})
   :turn 1})

;;----------------------- 
;; make-turn :: Player -> Card -> Card -> State -> State
(defn play-cards
  "Moves 1 and 2: play one card to council (cc) and one to the village (cv)."
  [p cc cv st]
  (->> st
       (move-card cc p :council)
       (move-card cv [:hand p] [:village p])))

;-----------------------
; 
; take-reserve-card :: Player -> Card -> State -> State
(defn take-reserve-card
  "Take a reserve card into a player's hand, and deal a new card to the reserve."
  [p c st]
  (->> st
       (move-card c :reserve ([:hand p]))
       (deal-reserve)))

;-----------------------
; 
; turn-over-cards :: [(Card (Lens Hand)] -> State -> State
(defn turn-over-cards
  "(Blk effect) Turn over up to two cards in any two different villages or council, e.g. (turn-over-cards '((:blk [:hand 0]) :mer [:council] s0)"
  [xs st]
  (cond (> (count xs) 2)
        (throw (Exception. "Can only turn over max 2 cards.")))

  (for [state st
        x xs]
    (turn-over-card (first x) (eval (second x)) state)))

;-----------------------
;; return-card :: Player -> Card -> State -> State
(defn return-card
  "(War effect) Throw away a card from any player's village and replace with a random card"
  [p c st]
  (->> st
       (update-in [:village p c] dec)
       (update-in [:village p] (h/hash-add (deal-card)))))

;;-----------------------
;; swap-hand-card :: Player -> Card -> Card -> State -> State
(defn swap-hand-card
  "(Brd effect) Swap a hand card with your village card, e.g. (swap-hand-card 'Brd 0 'Mer s0)."
  [p ch cv st]
  (swap-cards ch [:hand p] cv [:village p] st))

;-----------------------
; 
; swap-council-card :: Player -> Card -> Card -> State -> State
(defn swap-council-card
  "(Sea effect) Swap any village card with a council card, e.g. (swap-council-card 'Brd 0 'Mer s0)."
  [p cv cc st]
  (swap-cards cv [:village p] cc [:council] st))

;-----------------------
; 
; swap-village-card :: Player -> Card -> Player -> Card -> State -> State
(defn swap-village-card
  "(Mer effect) Swap two village cards."
  [p1 cv1 p2 cv2 st]
  (swap-cards cv1 [:village p1] cv2 [:village p2] st))


;-----------------------
(defn apply-action
  "Fold a list of actions over an initial state and record the game state."
  [act init-state]
  (eval
   (conj act (vec init-state))))

; apply-actions :: [(m ... -> State -> State)] -> State -> State
(defn apply-actions
  "Apply a sequence of actions to a given state."
  [lst init-state]
  (reduce apply-action init-state lst))

;-----------------------
; Example data
(def s0
  "Default state.:"
  (init-game 2 0))

#_(def actions
    (vec '(play-cards 0 :mer :sea)
         '(swap-council-card 0 :sea :mer)
         '(take-reserve-card 0 :war)))

;========================
; Unit tests


; The End
