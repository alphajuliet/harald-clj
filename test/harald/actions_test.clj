;; actions_test.clj
;; AndrewJ 2019-11-04

(ns harald.actions-test
  (:require [clojure.test :refer [deftest testing is]]
            [harald.state :as st]
            [harald.actions :as act]
            [numerimap.core :as n]))

(deftest move-tests
  (testing "Unit tests"
    (is (= (+ 2 3) 5)))

  (testing "Move cards"
    (let [s0 (act/init-game 3 0)
          s1 (act/move-card :war [:hand 1] [:council] s0)
          s2 (act/move-card :war [:hand 2] [:village 0] s1)]
      (is (= [0 0 0] (st/score-state s0)))
      (is (= 1 (get-in s1 [:council :war])))
      (is (= [1 0 0] (st/score-state s2)))))

  (testing "Game actions: play-cards, take-reserve-card"
    (let [s0 (act/init-game 3 0)
          s1 (act/play-cards 0 :mer :sea s0)
          s2 (act/take-reserve-card 0 :sct s1)
          s3 (act/take-reserve-card 0 :brd s2)]
      (is (= 1 (get-in s1 [:council :mer])))
      (is (= 1 (get-in s1 [:village 0 :sea])))
      (is (= 3 (n/m-sum (get-in s2 [:hand 0]))))
      (is (= 4 (n/m-sum (get-in s3 [:hand 0]))))))

  (testing "Game actions: turn-over-cards"
    (let [s0 (act/init-game 3 0)
          s1 (act/turn-over-cards [[:hand 0 :mer] [:reserve :brd]] s0)]
      (is (= 1 (get-in s1 [:hand 0 :merX])))
      (is (= 1 (get-in s1 [:reserve :brdX]))))))

(deftest return-card-tests
  (testing "Game actions: return-card"
    (let [s0 (act/init-game 3 0)
          s1 (act/play-cards 0 :mer :sea s0)
          s2 (act/return-card 0 :sea s1)]
      (is (= 3 (count (get-in s2 [:hand 0])))))))

(deftest swap-tests 
  (testing "Swap cards"
    (let [s0 (act/init-game 3 0)
          s1 (act/move-card :war [:hand 1] [:council] s0)
          s2 (act/move-card :war [:hand 2] [:village 0] s1)
          s3 (act/swap-cards :mer [:hand 0] :sea [:hand 1] s0)
          s4 (act/deal-to [:reserve] s0)
          s5 (act/turn-over-card [:council :war] s2)]
      (is (= 4 (n/m-sum (get-in s3 [:hand 0]))))
      (is (= 4 (n/m-sum (get-in s3 [:hand 1]))))
      (is (= 1 (get-in s3 [:hand 0 :mer])))
      (is (= 2 (get-in s3 [:hand 0 :sea])))

      ; Test deal-reserve
      (is (= 5 (n/m-sum (:reserve s4))))

      ; Test turn-over-card
      (is (= :seaX (act/invert :sea)))
      (is (= [0 0 0] (st/score-state s5)))

      ; Encode state to a vector
      (is (= 96 (count (flatten (st/encode-state s0)))))))

  (testing "Game actions: swap-hand-card"
    (let [s0 (act/init-game 3 0)
          s1 (act/play-cards 0 :mer :sea s0)
          s2 (act/swap-hand-village 0 :war :sea s1)]
      (is (= 1 (n/m-sum (get-in s2 [:village 0])))))))

;; The End
