;; actions_test.clj
;; AndrewJ 2019-11-04

(ns harald.actions-test
  (:require [clojure.test :refer :all]
            [harald.state :refer :all]
            [harald.actions :refer :all]
            [lentes.core :as l]))

(deftest action-tests
  (testing "Unit tests"
    (is (= (+ 2 2) 4)))

  (testing "Move cards"
    (let [s0 (init-game 3 0)
          s1 (move-card :war (_hand 1) _council s0)
          s2 (move-card :war (_hand 2) (_village 0) s1)]

        ; Test move-card
      (is (= (score-state s0) [0 0 0]))
      (is (= (l/focus (l/in [:council :war]) s1) 1))
      (is (= (score-state s2) [1 0 0])))
    #_(check-exn exn:fail? (λ () (move-card 'Blk (_hand 0) _council s0))))

    ; Test swap-cards
  (testing "Swap cards"
      (let [s0 (init-game 3 0)
            s1 (move-card :war (_hand 1) (_council) s0)
            s2 (move-card :war (_hand 2) (_village 0) s1)
            s3 (swap-cards :brd (_hand 0) :sea (_hand 1) s0)
            s4 (deal-reserve s0)
            s5 (turn-over-card :war :council s2)]
        (is (= (h/hash-sum (l/focus (_hand 0) s3))) 4)
        (is (= (h/hash-sum (l/focus (_hand 1) s3)) 4))
        (is (= (l/focus (_hand_card 0 :brd) s3) 0))
        (is (= (l/focus (_hand_card 0 :sea) s3) 2))

        ; Test deal-reserve
        (is (= (hash-sum (:reserve s4)) 5))

        ; Test turn-over-card
        (is (= (invert :sea) :seaX))
        (is (= (score-state s5) [0 0 0]))

        ; Encode state to a vector
        (is (= (length (encode-state s0)) 96)))))

;; The End