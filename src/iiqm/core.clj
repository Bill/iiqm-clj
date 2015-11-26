(ns iiqm.core
  ;; This import does not suffice, even with the use before it. Neither Java class can be found. 
                                        ; (:use [clojure.data.finger-tree])
                                        ; (:import [clojure.data.finger-tree CountedSortedSet EmptyTree])
  ;; so I require everything and use the ->fns to construct the Java objects
  (:use [clojure.data.finger-tree :only [meter ft-split-at measured ->EmptyTree ->CountedSortedSet]])
  (:gen-class))

;; a prefix-sum set is a counted-sorted set that also carries the prefix sum (in each node)

;; WOOPS! this shouldn't be a set! It should be a MULTISET!

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
  (let [ [left _ _] (ft-split-at prefix-sum-set (inc n))] (:prefix-sum (measured left))))

(defn iiqm [prefix-sum-set]
  (let [n           (count prefix-sum-set)
        w           (weight n)
        q           (quot n 4)
        denominator (/ n 2)
        b           (- n 1 q)]
    (if (< n 4)
      (throw (Exception. "IIQM requires at least 4 data points."))
      (/ (+ (* w (+ (nth prefix-sum-set q) (nth prefix-sum-set b)))
            (if (< n 5)
              0
              (- (prefix-sum-at prefix-sum-set (dec b)) (prefix-sum-at prefix-sum-set q))))
         denominator))))
