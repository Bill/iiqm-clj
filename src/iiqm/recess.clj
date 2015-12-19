(ns iiqm.recess
  (:require [clojure.data.finger-tree :refer [finger-tree meter measured split-tree ft-concat ft-split-at conjl]])
  (:import (clojure.lang Counted IPersistentCollection Indexed)
           (clojure.data.finger_tree SplitAt EmptyTree Measured)))

(defprotocol PrefixSum
  (prefix-sum [coll n]))

;; I considered making this a multimethod instead. But the multimethod would've always selected the
;; function based on the type of the first argument (the collection). That's exactly what protocols
;; do, only more efficiently than multimethods, so...
(defprotocol InterquartileMean
  (interquartile-mean [coll]))

(defn weight [n]
  (- 1 (/ (mod n 4) 4)))

(deftype RecessTree [cmpr tree mdata]

  ;; Finger Tree protocols come first

  Measured

  (measured [_] (measured tree))

  SplitAt

  (ft-split-at [this n]
    "Split the tree at the nth position. The tree's meter must define :count
    which is the number of elements in the tree."
    (let [notfound nil]
      (cond
        ;; Specifying three conditions instead of only two seems overly fussy. But I'm copying what finger-tree does.
        (< n 0) [(empty this) notfound this]
        (< n (count this))
        (let [[pre m post] (split-tree tree #(> (:count %) n))]
          [pre m post])
        :else [this notfound (empty this)])))

  ;; Next up: Clojure collection protocols

  IPersistentCollection

  (cons [_ value]
    "conj values into the tree. The values are positioned according to the :right value of the tree's measure.
     Yes you read that right! It turns out that contrary to the central design idea of Clojure sequences, the cons
     function on IPersistentCollection protocol does not return a sequence at all. It returns a new collection!"
    (if (empty? tree)
      (RecessTree. cmpr (conj tree value) mdata)
      (let [cmpr compare
            [l x r] (split-tree tree #(>= 0 (cmpr value (:right %))))
            compared (cmpr value x)]
        (let [[a b] (if (>= 0 compared) [value x] [x value])]
          (RecessTree. cmpr (ft-concat (conj l a) (conjl r b)) mdata)))))

  (empty [_]
    "Return the empty collection."
    (RecessTree. cmpr (empty tree) mdata))

  Counted

  (count [_]
    "Return the number of items in the tree. The tree's meter must define :count which is the number of elements in the
     tree. This will become the (Counted) count function on the new collection type"
    (:count (measured tree)))

  Indexed

  (nth [this n]
    "Return the nth element from the tree. The tree's meter must define :count which is the number of elements in the
    tree."
    (second (ft-split-at this n)))

  ;; And lastly, the Interquartile Mean protocols

  PrefixSum

  (prefix-sum [this n]
    "Return the sum of the first n elements of the (sorted) tree. Tree must be sorted (somehow) and its meter must define
    :sum which is the sum of the elements."
    (let [[left _ _] (ft-split-at this (inc n))] (:sum (measured left))))

  InterquartileMean

  (interquartile-mean [this]
    (let [n (count this)                                    ;; total sample size
          w (weight n)                                      ;; weight for edge samples
          q2 (quot n 4)                                     ;; index of second quartile
          q3 (- n 1 q2)                                     ;; index of third quartile
          denominator (/ n 2)]                              ;; samples in IQR
      (if (< n 4)
        (throw (Exception. "Interquartile mean requires at least 4 data points."))
        (/ (+ (* w (+ (nth this q2) (nth this q3)))
              (if (< n 5)
                0
                (- (prefix-sum this (dec q3)) (prefix-sum this q2))))
           denominator)))))

;; The count is an int. Sum and right are Objects so they can accept whatever kind of math-doing object you like.
(defrecord Measure [right ^int count sum])

(let [measure          #(Measure. % 1 %)
      measure-of-empty  (Measure. nil 0 0)                   ;; a.k.a. identity
      combine          #(Measure. (or (:right %2) (:right %1))
                                  (+ (:count %1) (:count %2))
                                  (+ (:sum %1) (:sum %2)))
      meter-recess      (meter measure measure-of-empty combine)
      empty-rt          (EmptyTree. meter-recess)]
  (def empty-recess-tree
    (RecessTree. compare empty-rt nil))
  (defn recess-tree [& args]
    (into empty-recess-tree args)))
