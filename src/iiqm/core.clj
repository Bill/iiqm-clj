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
      combine         #(Measure. (+ (:count %1) (:count %2))
                                 (+ (:sum %1) (:sum %2))
                                 (or (:right %2) (:right %1)))]
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

(defn iqm-csr-tree [csr-tree]
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
(def st (apply conj sum-tree s))
(def rt (apply conj right-tree s))
(def cst (apply conj count-sum-tree s))
(def csrt (apply conj csr-tree s))

(def csro (apply conj-ordered csr-tree s))

(comment (let [powers #{10 100 1000}]
           (loop [n 0 t csr-tree]
             (if (> n 1000)
               (println "finished!")
               (if (contains? powers n)
                 (do
                   (printf "a tree of size %d:\n" n)
                   (let [new-t (time (conj-ordered t (rand-int 101)))
                         iiqm (time (when (> n 3) (iqm-csr-tree new-t)))]
                     (printf "IIQM is: %.2f\n" (double iiqm))
                     (recur (inc n) new-t)))
                 (let [new-t (conj-ordered t (rand-int 101))
                       iiqm (when (> n 3) (iqm-csr-tree new-t))]
                   (recur (inc n) new-t)))))))


(defprotocol IQM
  "Interquartile Mean."
  (iqm [iiqm]))

(defprotocol PrefixSum
  (prefix-sum [coll n]))

(extend-type clojure.lang.IPersistentVector
  PrefixSum
  (prefix-sum [vec n]
    (reduce + 0 (take (inc n) vec))))

(defn iqm-sorted-vector-reduce [coll]
  (let [n (count coll)                                    ;; total sample size
        w (weight n)                                      ;; weight for edge samples
        q2 (quot n 4)                                     ;; index of second quartile
        q3 (- n 1 q2)                                     ;; index of third quartile
        denominator (/ n 2)]                              ;; samples in range
    (if (< n 4)
      (throw (Exception. "IIQM requires at least 4 data points."))
      (/ (+ (* w (+ (nth coll q2) (nth coll q3)))
            (if (< n 5)
              0
              (- (prefix-sum coll (dec q3)) (prefix-sum coll q2))))
         denominator))))

;; IIQM1 behaves kind of like a collection in that conj returns a new instance of IIQM1
;; construct with an empty or sorted Vector
(deftype IIQM1 [sorted-vector]
  clojure.lang.IPersistentCollection
  ;; this means conj--it's called by the conj function
  (cons [iiqm x] (IIQM1. (into (empty sorted-vector) (sort (conj sorted-vector x)))))
  IQM
  (iqm [iiqm] (iqm-sorted-vector-reduce sorted-vector)))

(defn binary-search
  "Given a sorted vector, returns the an index at which to insert x to maintain sort order.
  NB: if x is not already in the collection, an index where x belongs is returned. If you need to
  know if x is already present or not, then use nth to look it up."
  ([sorted-vec value] (binary-search sorted-vec value 0 (dec (count sorted-vec))))
  ([sorted-vec value low high]
   (if (> low high)
     low
     (let [mid (+ low (quot (- high low) 2))
           c   (compare (nth sorted-vec mid) value)]
       (case c
         -1 (recur sorted-vec value (inc mid) high)
         0 mid
         1 (recur sorted-vec value low (dec mid)))))))


(defn insert [coll i x]
  "Insert x in vector at index i and move the rest of the elements down to make room. Returned vector is 1
  element larger."
  (into (conj (subvec coll 0 i) x) (nthrest coll i)))

(defn insert-sorted [coll x]
  "Given a sorted collection coll and a new element x, return a new sorted collection with x in the right place.
  NB: when it doesn't find x, it returns the index at which to insert x, so if you want to know if the value is
  already in the collection, you'll have to look it up."
  (let [i (binary-search coll x)]
    (insert coll i x)))

;; construct with an empty or sorted Vector
(deftype IIQM2 [sorted-vector]
  clojure.lang.IPersistentCollection
  (cons [iiqm x] (IIQM2. (insert-sorted sorted-vector x)))
  IQM
  (iqm [iiqm] (iqm-sorted-vector-reduce sorted-vector)))

;; IIQM3 will be an incremental algorithm based on the sorted array (like IIQM1-2)
;; but it will avoid calling the O(n) reduce in prefix sum.

;; construct with a csr-tree
(deftype IIQM4 [csr-tree]
  clojure.lang.IPersistentCollection
  (cons [iiqm x] (IIQM4. (conj-ordered csr-tree x)))
  IQM
  (iqm [iiqm] (iqm-csr-tree csr-tree)))

(defmacro time-nano
  "Evaluates expr and returns an array. First is the value of the expr, second is the time it took in nanos."
  [expr]
  `(let [start# (. System (nanoTime))
         result# ~expr]
     [result# (- (. System (nanoTime)) start#)]))

(defn run-iiqm [N algo-initial]
  (time-nano
    (loop [n 0 algo algo-initial]
      (when (< n N)
        (let
          [new-algo (conj algo (rand-int 101))
           iqm (when (> n 3) (iqm new-algo))]
          (recur (inc n) new-algo))))))


(defn benchmark [algo] (map (fn [%] [% (second (run-iiqm % algo))]) [10 100 1000]))
(defn scale [m] (map (fn [[n nanos]][n  (quot nanos 1000000)]) m))


(measured ct)
(measured st)
(measured rt)
(measured cst)
(measured csrt)
(measured csro)

(double (iqm-csr-tree csro))

(iqm (reduce conj (IIQM1. []) (filter odd? (range 1 18))))

(iqm (reduce conj (IIQM1. []) (take 10 rando)))

(benchmark (IIQM1. []))
(benchmark (IIQM2. []))

