(ns iiqm.core-test
  (:require [clojure.test :refer [testing deftest are is]]
            [iiqm.recess :as recess]
            [iiqm.core :as core]))

(testing "weighting"
  (deftest weighting-test
    (are [x y] (= x (recess/weight y))
         1   4
         3/4 5
         1/2 6
         1/4 7
         1   8
         3/4 9)))

;; Test the various implementations of IIQM

(defn iiqm-tests [algo]
  (testing "IIQM"
    (testing "four test"
      (is (= (-> (+ 2 3) (/ 4/2)) (recess/interquartile-mean (conj algo 1 2 3 4))) "algorithm works for base case (4 samples)"))
    (testing "five test"
      (is (= (-> (+ 3) (+ (-> (+ 2 4) (* 3/4))) (/ 5/2)) (recess/interquartile-mean (conj algo 1 2 3 4 5))) "works for base+1 case (5 samples)"))
    (testing "nine odds test"
      ;; from Wikipedia entry for IIQM: http://en.wikipedia.org/wiki/Interquartile_mean
      (is (= 9 (recess/interquartile-mean (apply conj algo (filter odd? (range 1 18))))) "Wikipedia example works"))))

(deftest iiqm1-tests (iiqm-tests core/iiqm1))
(deftest iiqm2-tests (iiqm-tests core/iiqm2))
(deftest iiqm3-tests (iiqm-tests core/iiqm3))
(deftest iiqm4-tests (iiqm-tests core/iiqm4))


;; these test internals of csr-tree type and will be moved to the csr-tree project
(comment (testing "sorting"
           (deftest sorting-test
             (is (= (prefix-sum-set 10 9 8 7 6 5 1 2 3 4) (range 1 11)))))
         (testing "prefix-sum-at"
           (let [cont (apply prefix-sum-set (range 1 3))]
             (deftest prefix-sum-at-test
               (are [result n] (= result (prefix-sum-at cont n))
                               1 0
                               3 1))))
         (testing "multiset"
           (deftest multiset-test
             (is (= 2 (count (prefix-sum-set 1 1))) "a sample occurring twice should not be lost"))))
