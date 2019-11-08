;; hash-calc_test.clj
;; AndrewJ 2019-09-21

(ns harald.hash-calc-test
  (:require [clojure.test :refer :all]
            [harald.hash-calc :refer :all]
            [clojure.test :refer :all]))

(deftest hash-calc-tests
  (testing "Basics"
    (let [h1 {:a 1 :b 2 :c 3}]
      (is (= 1 (hash-min h1)))
      (is (= 6 (hash-sum h1)))
      (is (= [:a :b :b :c :c :c] (hash-enumerate h1)))))

  (testing "hash-union"
    (let [h1 {:a 1 :b 2 :c 3}
          h2 {:a 4 :b 5 :d 6}]
      (is (= {:a 5 :b 7 :c 3 :d 6} (hash-add h1 h2)))
      (is (= {:a 3 :b 3 :c 3 :d 6} (hash-sub h2 h1)))))

  (testing "hash-intersection"
    (let [h1 {:a 1 :b 2 :c 3}
          h2 {:a 4 :b 5 :d 6}]
      (is (= {:a 4 :b 10} (hash-mul h1 h2)))))

  (testing "hash-collect"
    (let [lst '(:a :b :b :c :c :c)]
      (is (= {:a 1 :b 2 :c 3} (hash-collect lst))))))

; The End