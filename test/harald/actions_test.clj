;; actions_test.clj
;; AndrewJ 2019-11-04

(ns harald.actions-test
  (:require [clojure.test :refer :all]
            [harald.state :refer :all]
            [harald.actions :refer :all]))

(deftest action-tests
  (testing "Unit tests"
    (is (= (+ 2 2) 4)))

  (testing "Move cards"
    (let [s0 (init-game 3 0)
          s1 (move-card :war [:hands 1] [:council] s0)
          s2 (move-card :war [:hands 2] [:village 0] s1)
          s3 (swap-cards :brd (:hands 0) :sea (_hand 1) s0)
          s4 (deal-reserve s0)
          s5 (turn-over-card :war :council s2)]

        ; Test move-card
      (is (= (score-state s0) '(0 0 0)))
      (is (= (get-in s1 [:council :war]) 1))
      (is (= (score-state s2) '(1 0 0)))
      #_(check-exn exn:fail? (λ () (move-card 'Blk (_hand 0) _council s0)))

        ; Test swap-cards
      (is (= (h/hash-sum (get-in s3 [:hands 0]))) 4)
      (is (= (h/hash-sum (get-in s3 [:hands 1])) 4))
      (is (= (get-in s3 [:hand 0 :brd]) 0))
      (is (= (get-in s3 [:hand 0 :sea]) 2))

        ; Test deal-reserve
      (is (= (hash-sum (:reserve s4)) 5))

        ; Test turn-over-card
      (is (= (invert :sea) :seaX))
      (is (= (score-state s5) [0 0 0]))

        ; Encode state to a vector
      (is (= (length (encode-state s0)) 96)))))

;; The End