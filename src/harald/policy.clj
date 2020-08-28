;; policy.clj
;; Game playing policies
;; AndrewJ 2019-11-16

(ns harald.policy
  (:require [harald.state :as st]
            [harald.actions :as act]
            [harald.game :as g]
            [harald.hash-calc :as h]
            [clojure.java.io :as io]))

;;-------------------------------
;; Utility functions

; random-value :: Hash a b -> b
(def random-value (comp rand-nth vals))

(defn argmax-map
  "Find the key with the maximum value."
  [m]
  (key (apply max-key val m)))

(defn argmax
  "Return the value x in xs that maximises (f x)."
  [f xs]
  (apply max-key f xs))

(defn argmin
  "Return the value x in xs that minimises (f x)."
  [f xs]
  (apply min-key f xs))

; From https://stackoverflow.com/questions/1601321/idiomatic-mode-function-in-clojure
(defn tally-map
  " Create a map where the keys are all of the unique elements in the input
   sequence and the values represent the number of times those elements
   occur. Note that the keys may not be formatted as conventional Clojure
   keys, i.e. a colon preceding a symbol."
  [aseq]
  (apply merge-with + (map (fn [x] {x 1}) aseq)))

;-------------------------------
; type Policy = Player -> State -> Action
; apply-policy :: Policy -> Player -> State -> State
(defn apply-policy
  "Apply a given policy function to generate the next state."
  [policy plyr st]
  (let [action (policy plyr st)
        new-state (act/apply-action action st)]
    (g/log action)
    (g/log (st/encode-state new-state))
    new-state))

;-------------------------------
; play-game :: Policy -> State -> State
(defn play-game
  "Play a game with a policy function for each player. 
   Limit the number of turns per player to `max-turns` with default 100."
  ([policy-a policy-b initial-state]
   (play-game policy-a policy-b initial-state 100))

  ([policy-a policy-b initial-state max-turns]
   ; Log the initial state
   (g/log (st/encode-state initial-state))
   (g/log (st/encode-state initial-state))

  ; Iterate through the actions for each player to generate a final state
   (reduce
    (fn [state i]
      (if (st/end-of-game? state)
        (reduced (st/end-score state))
        (do
          (g/log (format "---- Iteration %d:" i))
          (->> state
               (apply-policy policy-a :a)
               (apply-policy policy-b :b)))))
    initial-state
    (range max-turns))))

(defn winner
  "Identify the winner"
  [st]
  (argmax-map (:scores st)))

(defn play-n-games
  "Play n games using the same policies and initial state, and aggregate the wins."
  [n policy-a policy-b initial-state]
  (tally-map
   (reduce (fn [s _]
             (conj s (winner (play-game policy-a policy-b initial-state))))
           []
           (range n))))

;;-------------------------------
;; Policies

; random-policy :: Player -> State -> Action
(defn random-policy
  "Choose a random action from the ones available."
  [player state]
  (->> (g/available-actions player state)
       (group-by first)
       (random-value)
       (rand-nth)))

(defn- score
  "Helper function"
  [player st]
  (get-in st [:scores player]))

; greedy-policy :: Player -> State -> Action
(defn greedy-policy
  "Choose the available action that maximises the points in the target states. If none, then pick a random one."
  [player state]
  (argmax #(score player (act/apply-action % state))
          (shuffle (g/available-actions player state))))

; points-delta :: Player -> State -> State -> Integer
(defn score-delta
  "Measure the score difference between two states."
  [player curr-st next-st]

  (- (score player next-st)
     (score player curr-st)))

; alpha-policy :: Player -> State -> Action
(defn alpha-policy
  "Maximise difference in points between current and next state."
  [player state]

  (argmax #(score-delta player state (act/apply-action % state))
          (g/available-actions player state)))

;; The End
