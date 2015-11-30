(ns iiqm.core
  (:use [clojure.data.finger-tree :only (finger-tree meter measured split-tree ft-concat ft-split-at conjl)])
  (:require [spyscope.core])
  (:gen-class))

(require 'spyscope.core)

;; a prefix-sum set is a counted-sorted set that also carries the prefix sum (in each node)

;; Here is the meter. Notice that it is a simple extension of the built-in Len-Right-Meter
;; built in to CountedSortedSet/counted-sorted-set:
(comment
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
    (let [n (count prefix-sum-set)
          w (weight n)
          q (quot n 4)
          denominator (/ n 2)
          b (- n 1 q)]
      (if (< n 4)
        (throw (Exception. "IIQM requires at least 4 data points."))
        (/ (+ (* w (+ (nth prefix-sum-set q) (nth prefix-sum-set b)))
              (if (< n 5)
                0
                (- (prefix-sum-at prefix-sum-set (dec b)) (prefix-sum-at prefix-sum-set q))))
           denominator)))))

;; WOOPS! this shouldn't be a set at all! What I really want is a counted-double-list with another meter
;; composed onto it: the sum (not the number of nodes, the sum of the node values).
;; double-list is a sequential collection that provides constant-time access to both the left and right ends.
;; I need constant-time access to the right end to add new elements.
;; counted-double-list provides all the features of double-list plus constant-time count and log-n nth.
;; I need log-n access to nth in order to split the tree efficiently at the n-th element
;; And additionally I need the sum of all the nodes in the tree.

;; let's start simple, with a finger tree that will sum its nodes...

(def rando (repeatedly #(rand-int 100)))

(def sum-tree (finger-tree (meter identity 0 +)))
(def st (apply conj sum-tree (take 5 rando)))
(measured st)

;; and is it easy to re-implement counted-double-list?
(def count-tree (finger-tree (meter (constantly 1) 0 +)))
(def cdl (apply conj count-tree (take 5 rando)))
(measured cdl)

;; so how about a counted sum tree?
(defrecord Pair [^int count sum])
(let [measure         #(Pair. 1 %)
      measure-of-empty (Pair. 0 0)
      combine         #(Pair. (+ (:count %1) (:count %2)) (+ (:sum %1) (:sum %2)))]
  (def count-sum-tree
    (finger-tree
      (meter measure measure-of-empty combine))))
(def tt (apply conj count-sum-tree (take 5 rando)))
(measured tt)

;; so how about a counted sum right tree?
;; The count is an int. Sum and right are Objects.
(defrecord Measure [^int count sum right])
(let [measure         #(Measure. 1 % %)
      measure-of-empty (Measure. 0 0 nil)
      combine         #(Measure. (+ (:count %1) (:count %2)) (+ (:sum %1) (:sum %2)) (or (:right %2) (:right %1)))]
  (def csr-tree
    (finger-tree
      (meter measure measure-of-empty combine))))
(def csr (apply conj csr-tree (take 5 rando)))
(measured csr)

;; Now can we leverage the :right value here to perform ordered insertion?
;; Clojure's SortedSet lets you conj into it so that'd be natural here.
;; To start with, we'll just define a new function. We'll worry about protocols later.
(defn conj-ordered
  ([tree]
   tree)
  ([tree value]
    ;; EmptyTree doesn't define split()--see finger-tree.clj
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

(def csro (apply conj-ordered csr-tree (take 5 rando)))
(measured csro)

(defn weight [n]
  (- 1 (/ (mod n 4) 4)))

;; Now I'd like to bring the iiqm function down and define it. It calls prefix-sum-at and nth.
;; That means I'd have to make nth work. But it works through protocols and I haven't defined a type.
;; And prefix-sum-at calls ft-split-at which calls split under the covers. Again, that one is part
;; of a protocol that my tree type must define. It seems that trying to define a finger tree simply
;; by defining a meter is a losing battle. It seems we have to be all-in on defining a new type.

;; I'm going to try a little bit more anyway

;; This would be the (Counted) count function on the new type
(defn count-count [c-tree]
  (:count (measured c-tree)))

;; This would become the (IPersistentCollection) empty function on the new type
(defn empty-count [c-tree]
  csr-tree)                                                 ;; this is the ugliest of the hacks!

;; Split any tree (with :count in its measure) at the nth position.
;; This would be the (SplitAt) ft-split-at function on the new type.
;; TODO: figure out if we can just call split-tree without all the range
;; checking we're doing here.
(defn ft-split-at-count [c-tree n]
  (let [notfound nil]
    (cond
      (< n 0) [(empty-count c-tree) notfound c-tree]
      (< n (count-count c-tree))
        (let [[pre m post] (split-tree c-tree #(> (:count %) n))]
          [pre m post])
      :else [c-tree notfound (empty-count c-tree)])))

;; This would be the (Indexed) nth function on the new type.
(defn nth-count [c-tree n]
  (second (ft-split-at-count c-tree n)))

(defn prefix-sum-at [csr-tree n]
  (let [[left _ _] (ft-split-at-count csr-tree (inc n))] (:sum (measured left))))

(defn iiqm [csr-tree]
  (let [n (count-count csr-tree)
        w (weight n)
        q (quot n 4)
        denominator (/ n 2)
        b (- n 1 q)]
    (if (< n 4)
      (throw (Exception. "IIQM requires at least 4 data points."))
      (/ (+ (* w (+ (nth-count csr-tree q) (nth-count csr-tree b)))
            (if (< n 5)
              0
              (- (prefix-sum-at csr-tree (dec b)) (prefix-sum-at csr-tree q))))
         denominator))))