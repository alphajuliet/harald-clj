;; actions.clj
;; AndrewJ 2019-11-04

(ns harald.actions
  (:require [harald.state :as st]
            [numerimap.core :as n]
            [random-seed.core :as r])
  (:refer-clojure :exclude [rand rand-int rand-nth]))

;;-----------------------
;; Helper functions

;; deal-card :: Card
(defn deal-card
  "Select a random card."
  []
  {(r/rand-nth (seq st/card-types)) 1})

;; deal-n-cards :: Integer -> Hand -> Hand
(defn deal-n-cards
  "Deal n random cards to a given pile."
  [n init-h]
  (n/m-add init-h
           (apply n/m-add (repeatedly n #(deal-card)))))

;; deal-to :: Hand -> State -> State
(defn deal-to
  "Deal a card to a pile."
  [_dest st]
  (update-in st _dest (partial n/m-add (deal-card))))

;; move-card :: Card -> Hand -> Hand -> State -> State
(defn move-card
  "Move a card of type t from the src hand to the dest hand.
   e.g. (move-card :blk [:hand 1] [:council] s0)."
  [t _src _dest st]

  ;; Ensure the card is available
  (if (or (not (contains? (get-in st _src) t))
          (<= (get-in st (conj _src t)) 0))
    (throw (Exception.
            (format "### Error: card %s not available to move" t)))
    (-> st
         (update-in _dest (partial n/m-add {t 1}))
         (update-in _src #(n/m-sub % {t 1})))))


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
  (get st/all-cards (mod (+ 6 (.indexOf st/all-cards c)) 12)))

;; turn-over-card :: Lens Card -> State -> State
(defn turn-over-card
  "Turn over an existing card"
  [_card st]
  (if (nil? (get-in st _card))
    (throw (Exception.
            (format "### Card %s doesn't exist to turn over." _card)))
    ; else
    (let [card (last _card)
          _hand (drop-last _card)]
      (-> st
          (update-in _card #(- % 1))
          (update-in _hand #(n/m-add % {(invert card) 1}))))))

;;===============================
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
;; Effects from hand card:
;; - Turn over 0-2 village or council cards (Blacksmith effect)
;; - Replace any village card with a random card (Warrior effect)
;; - Swap hand card with own village card (Bard effect)
;; - Swap any village card with a council card (Seafarer effect)
;; - Swap your village card with another village card (Merchant effect)
;; - Apply effect from council card instead (Scout effect)


;;-----------------------
;; init-game :: Integer (-> Integer)? -> State
(defn init-game
  "Define the initial state, and seed the random number generator."
  ([nplayers]
   (init-game nplayers 0))

  ([nplayers seed]
   {:pre [(<= 2 nplayers 4)]}

   (r/set-random-seed! seed)

   {:council {}
    :village (vec (repeat nplayers {}))
    :hand (vec (repeatedly nplayers #(deal-n-cards 4 {})))
    :reserve (deal-n-cards 4 {})
    :score (vec (repeat nplayers 0))
    :turn 0}))

;;-----------------------
;; make-turn :: Player -> Card -> Card -> State -> State
(defn play-cards
  "Moves 1 and 2: play one card to council (cc) and one to the village (cv)."
  [player cc cv st]
  (->> st
       (move-card cc [:hand player] [:council])
       (move-card cv [:hand player] [:village player])))

;;-----------------------
;; take-reserve-card :: Player -> Card -> State -> State
(defn take-reserve-card
  "Take a reserve card into a player's hand, and deal a new card to the reserve."
  [player card st]
  (->> st
       (move-card card [:reserve] [:hand player])
       (deal-to [:reserve])))

;;-----------------------
;; turn-over-cards :: [[Card Hand]] -> State -> State
(defn turn-over-cards
  "(Blk effect) Turn over 0-2 cards in different villages or the council.
   e.g. (turn-over-cards [[:village 0 :blk] [:council :mer]] s0)"
  [cards st]
  {:pre [(<= 0 (count cards) 2)]}
  (reduce (fn [s c]
            (turn-over-card c s))
          st
          cards))

;;-----------------------
;; return-card :: Player -> Card -> State -> State
(defn return-card
  "(War effect) Throw away a card from any player's village and replace with a random card."
  [player card st]
  (if (nil? (get-in st [:village player card]))
    (throw (Exception. (format "### Card )%s isn't available to remove." card)))
    ;;else
    (as-> st <>
      (update-in <> [:village player] #(n/m-sub % {card 1}))
      (deal-to [:village player] <>))))

;;-----------------------
;; swap-hand-card :: Player -> Card -> Card -> State -> State
(defn swap-hand-village
  "(Brd effect) Swap a hand card with your village card.
  e.g. (swap-hand-village 0 :brd :mer s0)."
  [player ch cv st]
  (swap-cards ch [:hand player] cv [:village player] st))

;;-----------------------
;; swap-council-card :: Player -> Card -> Card -> State -> State
(defn swap-village-council
  "(Sea effect) Swap any village card with a council card.
   e.g. (swap-village-council 0 :brd :mer s0)."
  [p cv cc st]
  (swap-cards cv [:village p] cc [:council] st))

;;-----------------------
;; swap-village-card :: Player -> Card -> Player -> Card -> State -> State
(defn swap-village-village
  "(Mer effect) Swap two village cards."
  [p1 cv1 p2 cv2 st]
  (swap-cards cv1 [:village p1] cv2 [:village p2] st))

;;-----------------------
(defn update-score
  "Update the scores."
  [st]
  (assoc-in st [:score] (st/score-state st)))

(defn advance-turn
  "Advance the turn number by one."
  [st]
  (update-in st [:turn] inc))

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

;; apply-player-actions :: [(m ... -> State -> State)] -> State -> State
(defn apply-player-actions
  "Apply a series of actions in a player's turn to an initial state, update the
  scores, and increment the turn. The first action must be `play-cards`, then an
  optional effect action, followed by two `take-reserve-card` actions."
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
(def s1 (play-cards 0 :mer :mer s0))

(def a0
  "Example sequence of player turns."
  [[{:action :play-cards, :player 0, :cc :mer, :cv :mer}
    {:action :take-reserve-card, :player 0, :cr :sea}
    {:action :take-reserve-card, :player 0, :cr :sct}]

   [{:action :play-cards, :player 1, :cc :war, :cv :blk}
    {:action :take-reserve-card, :player 1, :cr :brd}
    {:action :take-reserve-card, :player 1, :cr :sct}]])

(def s2 (apply-game-actions a0 s0))

;; The End
