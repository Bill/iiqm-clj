;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit ctrl+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
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

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;iiqm.recess/recess-tree</span>","value":"#'iiqm.recess/recess-tree"}
;; <=

;; @@
(ns iiqm.core
  (:require [clojure.data.finger-tree :refer (finger-tree meter measured split-tree ft-concat ft-split-at conjl)]
            [iiqm.recess
             :refer [PrefixSum InterquartileMean prefix-sum interquartile-mean empty-recess-tree]
             :as recess]
            [gorilla-plot.core :as plot])
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

(defn benchmark [algo] (map (fn [%] [% (second (run-iiqm % algo))]) [10 100 1000 3000 5000 7000 10000 20000]))

(defn scale [m] (map (fn [[n nanos]] [n (quot nanos 1000000)]) m))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;iiqm.core/scale</span>","value":"#'iiqm.core/scale"}
;; <=

;; @@
(plot/compose
  (plot/list-plot (scale (benchmark iiqm1)) :color "blue" :joined true )
  (plot/list-plot (scale (benchmark iiqm2)) :color "red" :joined true)
  (plot/list-plot (scale (benchmark iiqm4)) :color "green" :joined true))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":50,"bottom":20,"right":10},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"f50c2741-0b9b-47ce-a7be-3fe543cae11b","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"f50c2741-0b9b-47ce-a7be-3fe543cae11b","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"data":[{"name":"f50c2741-0b9b-47ce-a7be-3fe543cae11b","values":[{"x":10,"y":3},{"x":100,"y":8},{"x":1000,"y":578},{"x":3000,"y":2544},{"x":5000,"y":8396},{"x":7000,"y":16071},{"x":10000,"y":33342},{"x":20000,"y":114199}]},{"name":"e71e6aef-9608-4043-9d92-b6d7e5a33cd0","values":[{"x":10,"y":6},{"x":100,"y":7},{"x":1000,"y":525},{"x":3000,"y":3370},{"x":5000,"y":10543},{"x":7000,"y":19020},{"x":10000,"y":41220},{"x":20000,"y":159471}]},{"name":"ba8a12cd-e974-4adc-ac8f-c279c7fd989a","values":[{"x":10,"y":7},{"x":100,"y":26},{"x":1000,"y":516},{"x":3000,"y":1078},{"x":5000,"y":1625},{"x":7000,"y":1997},{"x":10000,"y":3494},{"x":20000,"y":7113}]}],"marks":[{"type":"line","from":{"data":"f50c2741-0b9b-47ce-a7be-3fe543cae11b"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"blue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"e71e6aef-9608-4043-9d92-b6d7e5a33cd0"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"red"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"ba8a12cd-e974-4adc-ac8f-c279c7fd989a"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"green"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 50, :bottom 20, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"f50c2741-0b9b-47ce-a7be-3fe543cae11b\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"f50c2741-0b9b-47ce-a7be-3fe543cae11b\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :data ({:name \"f50c2741-0b9b-47ce-a7be-3fe543cae11b\", :values ({:x 10, :y 3} {:x 100, :y 8} {:x 1000, :y 578} {:x 3000, :y 2544} {:x 5000, :y 8396} {:x 7000, :y 16071} {:x 10000, :y 33342} {:x 20000, :y 114199})} {:name \"e71e6aef-9608-4043-9d92-b6d7e5a33cd0\", :values ({:x 10, :y 6} {:x 100, :y 7} {:x 1000, :y 525} {:x 3000, :y 3370} {:x 5000, :y 10543} {:x 7000, :y 19020} {:x 10000, :y 41220} {:x 20000, :y 159471})} {:name \"ba8a12cd-e974-4adc-ac8f-c279c7fd989a\", :values ({:x 10, :y 7} {:x 100, :y 26} {:x 1000, :y 516} {:x 3000, :y 1078} {:x 5000, :y 1625} {:x 7000, :y 1997} {:x 10000, :y 3494} {:x 20000, :y 7113})}), :marks ({:type \"line\", :from {:data \"f50c2741-0b9b-47ce-a7be-3fe543cae11b\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"blue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"e71e6aef-9608-4043-9d92-b6d7e5a33cd0\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"red\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"ba8a12cd-e974-4adc-ac8f-c279c7fd989a\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"green\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; @@

;; @@
