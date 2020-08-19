;; state_test.clj
;; AndrewJ 2019-11-04

(ns harald.state-test
  (:require [clojure.test :refer [deftest testing is]]
            [harald.state :as st]))

(deftest state-tests
  (testing "Scoring tests"
    (is (= (+ 2 3) 5))
    (is (= (st/score {:blk 1, :war 3, :sea 2}
                     {:blk 1, :war 2, :brd 3})
           7))
    (is (= (st/score {:blkX 1 :war 3 :sea 2}
                     {:blk 1 :war 2 :brd 3})
           6))
    (is (= (st/score-state (st/empty-state 4))
           [0 0 0 0]))))

;; The End
