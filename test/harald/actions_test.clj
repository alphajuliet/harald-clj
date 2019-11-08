;; actions_test.clj
;; AndrewJ 2019-11-04

(ns harald.actions-test
  (:require [clojure.test :refer :all]
            [harald.state :refer :all]
            [harald.actions :refer :all]
            [harald.hash-calc :as h]
            [lentes.core :as l]))

(deftest action-tests
  (testing "Unit tests"
    (is (= (+ 2 2) 4)))

  (testing "Move cards"
    (let [s0 (init-game 3 0)
          s1 (move-card :war (_hand 1) _council s0)
          s2 (move-card :war (_hand 2) (_village 0) s1)]

        ; Test move-card
      (is (= [0 0 0] (score-state s0)))
      (is (= 1 (l/focus (_card _council :war) s1)))
      (is (= [1 0 0] (score-state s2))))
    #_(check-exn exn:fail? (λ () (move-card 'Blk (_hand 0) _council s0))))

    ; Test swap-cards
  (testing "Swap cards"
    (let [s0 (init-game 3 0)
          s1 (move-card :war (_hand 1) _council s0)
          s2 (move-card :war (_hand 2) (_village 0) s1)
          s3 (swap-cards :mer (_hand 0) :blk (_hand 1) s0)
          s4 (deal-reserve s0)
          s5 (turn-over-card :war _council s2)]
      (is (= 4 (h/hash-sum (l/focus (_hand 0) s3))))
      (is (= 4 (h/hash-sum (l/focus (_hand 1) s3))))
      (is (= 1 (l/focus (_hand_card 0 :mer) s3)))
      (is (= 1 (l/focus (_hand_card 0 :blk) s3)))

        ; Test deal-reserve
      (is (= 5 (h/hash-sum (:reserve s4))))

        ; Test turn-over-card
      (is (= :seaX (invert :sea)))
      (is (= [0 0 0] (score-state s5)))

        ; Encode state to a vector
      (is (= 96 (count (flatten (encode-state s0)))))))
  
  (testing "Game actions: play-cards, take-reserve-card"
    (let [s0 (init-game 3 0)
          s1 (play-cards 0 :mer :sea s0)
          s2 (take-reserve-card 0 :sct s1)
          s3 (take-reserve-card 0 :brd s2)]
      (is (= 1 (l/focus (_card _council :mer) s1)))
      (is (= 1 (l/focus (_village_card 0 :sea) s1)))
      (is (= 3 (h/hash-sum (l/focus (_hand 0) s2))))
      (is (= 4 (h/hash-sum (l/focus (_hand 0) s3)))))))


;; The End