;; state_test.clj
;; AndrewJ 2019-11-04

(ns harald.state-test
  (:require [clojure.test :refer :all]
            [harald.state :refer :all]))

(deftest state-tests
  (testing "Scoring tests"
    (is (= (+ 2 2) 4))
    (is (= (score {:blk 1, :war 3, :sea 2}
                  {:blk 1, :war 2, :brd 3})
           7))
    (is (= (score {:blkX 1 :war 3 :sea 2}
                  {:blk 1 :war 2 :brd 3})
           6))
    (is (= (score-state (empty-state 4))
           [0 0 0 0]))))

;; The End