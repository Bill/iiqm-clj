(ns iiqm.core-test
  (:require [clojure.test :refer :all]
            [iiqm.core :refer :all]))

(comment (testing "sorting"
           (deftest sorting-test
             (is (= (prefix-sum-set 10 9 8 7 6 5 1 2 3 4) (range 1 11))))))

(testing "weighting"
  (deftest weighting-test
    (are [x y] (= x (weight y))  
         1   4
         3/4 5
         1/2 6
         1/4 7
         1   8
         3/4 9)))

(comment (testing "prefix-sum-at"
           (let [cont (apply prefix-sum-set (range 1 3))]
             (deftest prefix-sum-at-test
               (are [result n] (= result (prefix-sum-at cont n))
                               1 0
                               3 1)))))

(comment (testing "multiset"
           (deftest multiset-test
             (is (= 2 (count (prefix-sum-set 1 1))) "a sample occurring twice should not be lost"))))

(comment (testing "iiqm"
           (deftest four-test
             (is (= (-> (+ 2 3) (/ 4/2)) (iqm-csr-tree (prefix-sum-set 1 2 3 4))) "algorithm works for base case (4 samples)"))
           (deftest five-test
             (is (= (-> (+ 3) (+ (-> (+ 2 4) (* 3/4))) (/ 5/2)) (iqm-csr-tree (prefix-sum-set 1 2 3 4 5))) "works for base+1 case (5 samples)"))
           (deftest nine-odds-test
             ;; from Wikipedia entry for IIQM: http://en.wikipedia.org/wiki/Interquartile_mean
             (is (= 9 (iqm-csr-tree (apply prefix-sum-set (filter odd? (range 1 18))))) "Wikipedia example works"))))
