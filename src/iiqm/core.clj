(ns iiqm.core
  (:use [clojure.data.finger-tree :only (finger-tree meter measured split-tree ft-concat ft-split-at conjl)])
  (:require [spyscope.core])
  (:gen-class))

(require 'spyscope.core)
(require '[clojure.test.check :as tc])
(require '[clojure.test.check.generators :as gen])
(require '[clojure.test.check.properties :as prop])


;; The commented form below was a first try at IIQM. I made the mistake of basing my collection
;; on a set. Of course that was wrong because it didn't capture duplicate elements which are
;; in general, significant.

(comment
  ;; A prefix-sum set is a counted-sorted set that also carries the prefix sum (in each node).

  ;; Here is the meter. Notice that it is a simple extension of the built-in Len-Right-Meter
  ;; built in to CountedSortedSet/counted-sorted-set:
  (defrecord Len-Right-Prefix-Sum-Meter [^int len right ^int prefix-sum])

  (defn combine-keyed [a b key-fun] (+ (key-fun ^Len-Right-Prefix-Sum-Meter a)
                                       (key-fun ^Len-Right-Prefix-Sum-Meter b)))

  (let [measure (fn [x] (Len-Right-Prefix-Sum-Meter. 1 x x))
        measure-of-empty (Len-Right-Prefix-Sum-Meter. 0 nil 0)
        combine (fn [a b] (Len-Right-Prefix-Sum-Meter.
                            (combine-keyed a b :len)
                            (or (:right b) (:right a))
                            (combine-keyed a b :prefix-sum)))
        len-rps-meter (meter measure measure-of-empty combine)
        empty-tree (->EmptyTree len-rps-meter)
        default-empty-set (->CountedSortedSet compare empty-tree nil)]

    (defn prefix-sum-set-by [cmpr & args]
      (into (->CountedSortedSet cmpr empty-tree nil) args))
    (defn prefix-sum-set [& args]
      (into default-empty-set args)))

  (defn weight [n]
    (- 1 (/ (mod n 4) 4)))

  (defn prefix-sum-at [prefix-sum-set n]
    (let [[left _ _] (ft-split-at prefix-sum-set (inc n))] (:prefix-sum (measured left))))

  (defn iiqm [prefix-sum-set]
    (let [n (count prefix-sum-set)                          ;; total sample size
          w (weight n)                                      ;; weight for edge samples
          q2 (quot n 4)                                     ;; index of second quartile
          q3 (- n 1 q2)                                     ;; index of third quartile
          denominator (/ n 2)]                              ;; samples in IQR
      (if (< n 4)
        (throw (Exception. "IIQM requires at least 4 data points."))
        (/ (+ (* w (+ (nth prefix-sum-set q2) (nth prefix-sum-set q3)))
              (if (< n 5)
                0
                (- (prefix-sum-at prefix-sum-set (dec q3)) (prefix-sum-at prefix-sum-set q2))))
           denominator)))))

;; counted-double-list provides all the features of double-list plus constant-time count and log-n nth.
;; If that tree were sorted, and I could gain log-n access to the nth position (thanks counted-double-list!)
;; then I could split the (sorted) tree efficiently at the n-th element.
;; Once I'd split the tree (at a quartile) I'd need the sum of all the node values in that tree.
;; What's really needed is a counted-double-list with another couple meters composed onto it:
;;   the right-most value for sorting (a la counted-sorted-set)
;;   the sum (not the number of nodes, the sum of the node values)
;; let's start with some simple experiments...

;; A sequence consisting of random integers between 0 and 100. Its IIQM should tend toward 50.
(def rando (repeatedly #(rand-int 101)))

;; it is easy to re-implement the core of counted-double-list
(def count-tree (finger-tree (meter (constantly 1) 0 +)))

;; and here's a finger tree that will sum its nodes...
(def sum-tree (finger-tree (meter identity 0 +)))

;; and here's a finger tree that knows its right-most value
;; (to be used in efficient sorted insertion and retrieval later on)
;; order is important on the or here (and btw since or is a macro we have to swaddle it in a function)
(def right-tree (finger-tree (meter identity nil #(or %2 %1))))

;; so how about both things in the same tree: a counted sum tree?
(defrecord Pair [^int count sum])
(let [measure         #(Pair. 1 %)
      measure-of-empty (Pair. 0 0)                          ;; a.k.a. identity
      combine         #(Pair. (+ (:count %1) (:count %2)) (+ (:sum %1) (:sum %2)))]
  (def count-sum-tree
    (finger-tree
      (meter measure measure-of-empty combine))))

;; If we want to sort, we need the right-most value in the tree. How about a tree that does all three things?
;; The count is an int. Sum and right are Objects so they can accept whatever kind of math-doing object you like.
(defrecord Measure [^int count sum right])
(let [measure         #(Measure. 1 % %)
      measure-of-empty (Measure. 0 0 nil)                   ;; a.k.a. identity
      combine         #(Measure. (+ (:count %1) (:count %2)) (+ (:sum %1) (:sum %2)) (or (:right %2) (:right %1)))]
  (def csr-tree
    (finger-tree
      (meter measure measure-of-empty combine))))


(defn conj-ordered
  "conj values into the tree. The values are positioned according to the :right value of the tree's measure.
  This will become the (IPersistentCollection) cons function on the new type. Yes you read that right! It turns
  out that contrary to the central design idea of Clojure sequences, the cons function on that protocol does not
  return a sequence at all. It returns a new collection!"
  ([tree]
   tree)
  ([tree value]
    ;; Booger: EmptyTree doesn't define split()--see finger-tree.clj
   (if (isa? (type tree) clojure.data.finger_tree.EmptyTree)
     (conj csr-tree value)
     (let [cmpr compare
           [l x r] (split-tree tree #(>= 0 (cmpr value (:right %))))
           compared (cmpr value x)]
       (let [[a b] (if (>= 0 compared) [value x] [x value])]
         (ft-concat (conj l a) (conjl r b))))))
  ([tree x & xs]
   (if xs
     (recur (conj-ordered tree x) (first xs) (next xs))
     (conj-ordered tree x))))

;; Now I'd like to define the iiqm function. It calls prefix-sum-at and nth.
;; That means we have to make nth work. But it works through protocols and we haven't defined a type.
;; And prefix-sum-at calls ft-split-at which calls split under the covers. Again, that one is part
;; of a protocol that our tree type must define. It seems that trying to define a finger tree simply
;; by defining a meter is a losing battle. It seems we have to be all-in on defining a new type.
;; But let's try to hack our way through it with plain old functions and see how far we can get...


;; First, we define the functions pertaining to the "counted" nature of our collection. Each of these depends
;; on the tree's meter defining :count which is the number of elements in the tree.

(defn count-count [c-tree]
  "Return the number of items in the tree. The tree's meter must define :count which is the number of elements in the
   tree. This will become the (Counted) count function on the new collection type"
  (:count (measured c-tree)))


(defn empty-count [c-tree]
  "Return the empty collection. This will become the (IPersistentCollection) empty function on the new type"
  csr-tree)

(defn ft-split-at-count [c-tree n]
  "Split the tree at the nth position. The tree's meter must define :count which is the number of elements in the tree.
  This will become the (SplitAt) ft-split-at function on the new type."
  (let [notfound nil]
    (cond
      ;; Specifying three conditions instead of only two seems overly fussy. But I'm copying what finger-tree does.
      (< n 0) [(empty-count c-tree) notfound c-tree]
      (< n (count-count c-tree))
        (let [[pre m post] (split-tree c-tree #(> (:count %) n))]
          [pre m post])
      :else [c-tree notfound (empty-count c-tree)])))

(defn nth-count [c-tree n]
  "Return the nth element from the tree. The tree's meter must define :count which is the number of elements in the
  tree.This will become the (Indexed) nth function on the new collection type."
  (second (ft-split-at-count c-tree n)))

;; Second, we define the functions pertaining to the "summed" nature of our collection. Each of these depends on the
;; tree's meter defining :sum which is the sum of the elements (values) in the tree.

(defn prefix-sum-at [csr-tree n]
  "Return the sum of the first n elements of the (sorted) tree. Tree must be sorted (somehow) and its meter must define
  :sum which is the sum of the elements."
  (let [[left _ _] (ft-split-at-count csr-tree (inc n))] (:sum (measured left))))

;; Third, and last, we define the IIQM (incremental interquartile mean) function.

(defn weight [n]
  (- 1 (/ (mod n 4) 4)))

(defn iiqm [csr-tree]
  (let [n (count-count csr-tree)                          ;; total sample size
        w (weight n)                                      ;; weight for edge samples
        q2 (quot n 4)                                     ;; index of second quartile
        q3 (- n 1 q2)                                     ;; index of third quartile
        denominator (/ n 2)]                              ;; samples in IQR
    (if (< n 4)
      (throw (Exception. "IIQM requires at least 4 data points."))
      (/ (+ (* w (+ (nth-count csr-tree q2) (nth-count csr-tree q3)))
            (if (< n 5)
              0
              (- (prefix-sum-at csr-tree (dec q3)) (prefix-sum-at csr-tree q2))))
         denominator))))

;; OK let's exercise some of our definitions...

(def s (take 10 rando))
(def ct (apply conj count-tree s))
(measured ct)
(def st (apply conj sum-tree s))
(measured st)
(def rt (apply conj right-tree s))
(measured rt)
(def cst (apply conj count-sum-tree s))
(measured cst)
(def csrt (apply conj csr-tree s))
(measured csrt)

(def csro (apply conj-ordered csr-tree s))
(measured csro)

(double (iiqm csro))

(let [powers #{10 100 1000 10000 100000 1000000}]
  (loop [n 0 t csr-tree]
    (if (> n 1000000)
      (println "finished!")
      (if (contains? powers n)
        (do
          (printf "a tree of size %d:\n" n)
          (let [new-t (time (conj-ordered t (rand-int 101)))
                iiqm (time (when (> n 3) (iiqm new-t)))]
            (printf "IIQM is: %.2f\n" (double iiqm))
            (recur (inc n) new-t)))
        (let [new-t (conj-ordered t (rand-int 101))
              iiqm (when (> n 3) (iiqm new-t))]
          (recur (inc n) new-t))))))


(def sort-idempotent-prop
  (prop/for-all [v (gen/vector gen/int)]
                (= (sort v) (sort (sort v)))))

(tc/quick-check 100 sort-idempotent-prop)
