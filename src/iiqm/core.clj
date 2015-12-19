(ns iiqm.core
  (:require [clojure.data.finger-tree :refer (finger-tree meter measured split-tree ft-concat ft-split-at conjl)])
  (:require [iiqm.recess
             :refer [PrefixSum InterquartileMean weight prefix-sum interquartile-mean empty-recess-tree]
             :as recess])
  (:import [iiqm.recess RecessTree])
  (:gen-class))
;(:require [spyscope.core])
;(:import [iiqm.recess RecessTree PrefixSum InterquartileMean])

(extend-type clojure.lang.IPersistentVector
  PrefixSum
  (prefix-sum [vec n]
    (reduce + 0 (take (inc n) vec))))

(defn iqm-sorted-vector-reduce [coll]
  (let [n (count coll)                                    ;; total sample size
        w (recess/weight n)                                      ;; weight for edge samples
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
  (cons [_ x] (IIQM1. (into (empty sorted-vector) (sort (conj sorted-vector x)))))
  InterquartileMean
  (interquartile-mean [_] (iqm-sorted-vector-reduce sorted-vector)))
(def iiqm1 (IIQM1. []))

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
  (cons [_ x] (IIQM2. (insert-sorted sorted-vector x)))
  InterquartileMean
  (interquartile-mean [_] (iqm-sorted-vector-reduce sorted-vector)))
(def iiqm2 (IIQM2. []))

;; IIQM3 will be an incremental algorithm based on the sorted array (like IIQM1-2)
;; but it will avoid calling the O(n) reduce in prefix sum.

;; construct with a csr-tree
(def iiqm4 empty-recess-tree)

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
           iqm (when (> n 3) (interquartile-mean new-algo))]
          (recur (inc n) new-algo))))))

(defn benchmark [algo] (map (fn [%] [% (second (run-iiqm % algo))]) [10 100 1000]))

(benchmark iiqm1)
(benchmark iiqm2)
(benchmark iiqm4)

(defn scale [m] (map (fn [[n nanos]] [n (quot nanos 1000000)]) m))
