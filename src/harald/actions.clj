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
  "Select a random card."
  []
  {(r/rand-nth card-types) 1})

;; deal-n-cards :: Integer -> Hand -> Hand
(defn deal-n-cards
  "Deal n random cards to a given pile."
  [n init-h]
  (reduce (fn [m _]
            (h/hash-add (deal-card) m))
          init-h
          (range n)))

;; deal-reserve :: State -> State
(defn deal-to
  "Deal a card to a pile."
  [_dest st]
  (l/over _dest (partial h/hash-add (deal-card)) st))

#_(def >>> comp)

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
         (l/over _src #(h/hash-sub % {t 1})))))


;; swap-cards :: Card -> Lens Hand -> Card -> Lens Hand -> State -> State
(defn swap-cards
  "Swap two cards between two hands, i.e. exchange t1 from lens1 with t2 from lens2."
  [c1 _lens1 c2 _lens2 st]
  (->> st
       (move-card c1 _lens1 _lens2)
       (move-card c2 _lens2 _lens1)))

;; invert :: Card -> Card
(defn invert
  "Turn over a card, e.g. (invert :sea) => :seaX"
  [c]
  (all-cards (mod (+ 6 (.indexOf all-cards c)) 12)))

;; turn-over-card :: Card -> Lens Hand -> State -> State
(defn turn-over-card
  "Turn over an existing card"
  [card _h st]
  (if (nil? (l/focus (_card _h card) st))
    (throw (Exception.
            (format "### Card %s doesn't exist to turn over." card)))
    (->> st
         (l/over _h #(h/hash-sub % {card 1}))
         (l/over _h #(h/hash-add % {(invert card) 1})))))

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
;; - Turn over 0-2 village or council cards (Blacksmith effect)
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
  {:pre [(<= 2 nplayers 4)]}

  (r/set-random-seed! seed)

  {:council {}
   :villages (vec (repeat nplayers {}))
   :hands (vec (repeatedly nplayers #(deal-n-cards 4 {})))
   :reserve (deal-n-cards 4 {})
   :scores (vec (repeat nplayers 0))
   :turn 0})

;;----------------------- 
;; make-turn :: Player -> Card -> Card -> State -> State
(defn play-cards
  "Moves 1 and 2: play one card to council (cc) and one to the village (cv)."
  [player cc cv st]
  (->> st
       (move-card cc (_hand player) _council)
       (move-card cv (_hand player) (_village player))))

;-----------------------
; 
; take-reserve-card :: Player -> Card -> State -> State
(defn take-reserve-card
  "Take a reserve card into a player's hand, and deal a new card to the reserve."
  [player card st]
  (->> st
       (move-card card _reserve (_hand player))
       (deal-to _reserve)))

;-----------------------
(defn ->lens
  "Convert a description to a lens"
  [d]
  (if (vector? d)
    (comp (l/key (first d)) (l/nth (second d)))
    ;else
    (l/key d)))


; turn-over-cards :: [[Card Hand]] -> State -> State
(defn turn-over-cards
  "(Blk effect) Turn over 0-2 cards in different villages or the council.
   e.g. (turn-over-cards [[:blk [:village 0]], [:mer :council]] s0)"
  [cards st]
  {:pre [(<= 0 (count cards) 2)]}
  (reduce (fn [s x]
            (turn-over-card (first x) (->lens (second x)) s))
          st
          cards))

;-----------------------
;; return-card :: Player -> Card -> State -> State
(defn return-card
  "(War effect) Throw away a card from any player's village and replace with a random card."
  [player card st]
  (if (nil? (l/focus (_village_card player card) st))
    (throw (Exception. (format "### Card %s isn't available to remove." card)))
    ;else
    (->> st
         (l/over (_village player) #(h/hash-sub % {card 1}))
         (deal-to (_village player)))))

;;-----------------------
;; swap-hand-card :: Player -> Card -> Card -> State -> State
(defn swap-hand-village
  "(Brd effect) Swap a hand card with your village card. 
  e.g. (swap-hand-village 0 :brd :mer s0)."
  [player ch cv st]
  (swap-cards ch (_hand player) cv (_village player) st))

;-----------------------
; swap-council-card :: Player -> Card -> Card -> State -> State
(defn swap-village-council
  "(Sea effect) Swap any village card with a council card. 
   e.g. (swap-village-council 0 :brd :mer s0)."
  [p cv cc st]
  (swap-cards cv (_village p) cc _council st))

;-----------------------
; swap-village-card :: Player -> Card -> Player -> Card -> State -> State
(defn swap-village-village
  "(Mer effect) Swap two village cards."
  [p1 cv1 p2 cv2 st]
  (swap-cards cv1 (_village p1) cv2 (_village p2) st))

;-----------------------
(defn update-score
  "Update the scores."
  [st]
  (l/put _scores (score-state st) st))

(defn advance-turn
  "Advance the turn number by one."
  [st]
  (l/over _turn inc st))

;;-----------------------
;; Dispatch on actions

(defmulti do-action
  "Apply a given action to the given state and return a new state."
  :action)

(defmethod do-action :play-cards
  [{:keys [action player cc cv state]}]
  (play-cards player cc cv state))

(defmethod do-action :take-reserve-card
  [{:keys [action player cr state]}]
  (take-reserve-card player cr state))

(defmethod do-action :turn-over-cards
  [{:keys [action cards state]}]
  (turn-over-cards cards state))

(defmethod do-action :return-card
  [{:keys [action player card state]}]
  (return-card player card state))

(defmethod do-action :swap-hand-village
  [{:keys [action player ch cv state]}]
  (swap-hand-village player ch cv state))

(defmethod do-action :swap-village-council
  [{:keys [action village cv cc state]}]
  (swap-village-council village cv cc state))

(defmethod do-action :swap-village-village
  [{:keys [action v1 cv1 v2 cv2 state]}]
  (swap-village-village v1 cv1 v2 cv2 state))

(defmethod do-action :default [{:keys [action]}]
  (throw (Exception. (format "Unknown action: %s" action))))

;; apply-action :: Action -> State -> State
(defn apply-action
  "Apply an action command to `state`."
  [cmd state]
  {:pre [(map? cmd)
         (map? state)]}
  (->> (into cmd {:state state}) ; first inject the current state
       (do-action)))

; apply-player-actions :: [(m ... -> State -> State)] -> State -> State
(defn apply-player-actions
  "Apply a series of actions in a player's turn to an initial state, update the scores, and increment the turn.
  The first action must be `play-cards`, then an optional effect action, followed by two `take-reserve-card` actions."
  [cmds init-state]
  {:pre (<= 3 (count cmds) 4)}
  (->> cmds
       (reduce (fn [st cmd] (apply-action cmd st))
               init-state)
       (update-score)
       (advance-turn)))

(defn apply-game-actions
  "Apply a sequence of player turn actions to `state`."
  [actions state]
  (reduce (fn [st acts] (apply-player-actions acts st)) state actions))

;;-----------------------
;; Example data

(def s0 (init-game 3 0))
(def s1 (play-cards 0 :mer :sea s0))

(def a0
  "Example action sequence."
  [[{:action :play-cards, :player 0, :cc :mer, :cv :mer}
    {:action :take-reserve-card, :player 0, :cr :sea}
    {:action :take-reserve-card, :player 0, :cr :sct}]
   
   [{:action :play-cards, :player 1, :cc :sea, :cv :sea}
    {:action :take-reserve-card, :player 1, :cr :sct}
    {:action :take-reserve-card, :player 1, :cr :brd}]])

;; The End
