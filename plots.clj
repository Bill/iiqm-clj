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
          q4 (- n 1 q2)                                     ;; index of end of fourth quartile
          denominator (/ n 2)]                              ;; samples in IQR
      (if (< n 4)
        (throw (Exception. "Interquartile mean requires at least 4 data points."))
        (/ (+ (* w (+ (nth this q2) (nth this q4)))
              (if (< n 5)
                0
                (- (prefix-sum this (dec q4)) (prefix-sum this q2))))
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
            [spyscope.core])
  (:gen-class))
;(:import [iiqm.recess RecessTree PrefixSum InterquartileMean])

(extend-type clojure.lang.IPersistentVector
  PrefixSum
  (prefix-sum [vec n]
    (reduce + 0 (take (inc n) vec))))

(defn iqm-sorted-vector-reduce [coll]
  (let [n (count coll)                                    ;; total sample size
        w (recess/weight n)                               ;; weight for edge samples
        q2 (quot n 4)                                     ;; index of second quartile
        q4 (- n 1 q2)                                     ;; index of end of fourth quartile
        denominator (/ n 2)]                              ;; samples in range
    (if (< n 4)
      (throw (Exception. "IIQM requires at least 4 data points."))
      (/ (+ (* w (+ (nth coll q2) (nth coll q4)))
            (if (< n 5)
              0
              (- (prefix-sum coll (dec q4)) (prefix-sum coll q2))))
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
  "Given a sorted collection coll and a new element x, return a new sorted collection with x in the right place."
  (let [i (binary-search coll x)]
    (insert coll i x)))

;; construct with an empty or sorted Vector
(deftype IIQM2 [sorted-vector]
  clojure.lang.IPersistentCollection
  (cons [_ x] (IIQM2. (insert-sorted sorted-vector x)))
  InterquartileMean
  (interquartile-mean [_] (iqm-sorted-vector-reduce sorted-vector)))
(def iiqm2 (IIQM2. []))

(defn edge-factor [n]
  (let [q (/ n 4)
        low-bound (dec (int (Math/ceil q)))
        high-bound (int (* 3 q))                            ; floor
        width (inc (- high-bound low-bound))]
    (printf "q: %f, low-bound: %d, high-bound: %d, width: %d" (double q) low-bound high-bound width)
    (- q (dec (/ width 2)))))

;; IIQM3 will be an incremental algorithm based on the sorted array (like IIQM1-2)
;; but it will avoid calling the O(n) reduce in prefix sum.
(defn iqm-sorted-vector-incremental [coll x old-low-bound old-high-bound old-low-value old-high-value old-sum]
  (let [n (count coll)]               ;; samples in range
    (case (compare n 4)
      -1 [0 0 0 0 0 0]
      0 (let [a (nth coll 1)
              b (nth coll 2)
              sum (+ a b)]
          [(/ sum 2) 0 3 a b sum])
      1 (let [q           (/ n 4)
              low-bound   (dec (int (Math/ceil q)))
              high-bound  (int (* 3 q))                     ; floor
              width       (inc (- high-bound low-bound))    ; actual samples in range
              weight      (- q (dec (/ width 2)))           ; weight for edge samples
              denominator (/ n 2)                           ; fraction of samples in range
              low-val     (nth coll low-bound)              ; lowest value in range
              high-val    (nth coll high-bound)             ; highest value in range
              deltas
              [(if (< x old-high-value)
                 [(* -1 old-high-value)
                  (if (>= x old-low-value)
                    x
                    old-low-value)]
                 0)
               (if (> low-bound old-low-bound)
                 (* -1 low-val)
                 0)
               (if (> high-bound old-high-bound)
                 (nth coll (dec high-bound))
                 0)]
              sum (reduce + old-sum (flatten deltas))
              iqm (/ (+ sum (* weight (+ low-val high-val)))
                     denominator)]
            [iqm low-bound high-bound low-val high-val sum]))))

(deftype IIQM3 [sorted-vector iqm low-bound high-bound low-value high-value sum]
  clojure.lang.IPersistentCollection
  (cons [_ x] (let [new-sorted-vector
                    (insert-sorted sorted-vector x)
                    [iqm new-low-bound new-high-bound new-low-value new-high-value new-sum]
                    (iqm-sorted-vector-incremental new-sorted-vector x low-bound high-bound low-value high-value sum)]
                (IIQM3. new-sorted-vector iqm new-low-bound new-high-bound new-low-value new-high-value new-sum)))
  InterquartileMean
  (interquartile-mean [_]
    (let [n (count sorted-vector)]
      (if (> n 3)
        iqm
        (throw (Exception. (format "can't compute interquartile mean on %d samples (need at least 4)" n)))))))

(def iiqm3 (IIQM3. [] 0 0 0 0 0 0))

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

(defn scale [m] (map (fn [[n nanos]] [n (quot nanos 1000000)]) m))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;iiqm.core/scale</span>","value":"#'iiqm.core/scale"}
;; <=

;; @@
(ns iiqm.core
  (:require [gorilla-plot.core :as plot]))

(defn benchmark [algo] (map (fn [%] [% (second (run-iiqm % algo))]) [10 100 1000 7000 20000]))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;iiqm.core/benchmark</span>","value":"#'iiqm.core/benchmark"}
;; <=

;; @@
(plot/compose
  (plot/list-plot (scale (benchmark iiqm1)) :color "blue" :joined true)
  (plot/list-plot (scale (benchmark iiqm2)) :color "red" :joined true)
  (plot/list-plot (scale (benchmark iiqm3)) :color "cyan" :joined true)
  (plot/list-plot (scale (benchmark iiqm4)) :color "green" :joined true))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":50,"bottom":20,"right":10},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"e2acd31a-e22f-4a2b-88bc-c7596cedcba3","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"e2acd31a-e22f-4a2b-88bc-c7596cedcba3","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"data":[{"name":"e2acd31a-e22f-4a2b-88bc-c7596cedcba3","values":[{"x":10,"y":0},{"x":100,"y":14},{"x":1000,"y":650},{"x":7000,"y":15083},{"x":20000,"y":101205}]},{"name":"4031a863-57a8-4adf-887b-6e23b2ce3761","values":[{"x":10,"y":0},{"x":100,"y":6},{"x":1000,"y":503},{"x":7000,"y":19982},{"x":20000,"y":167797}]},{"name":"1d400bf5-feba-4ed3-a6dc-445ad588cf2e","values":[{"x":10,"y":1},{"x":100,"y":4},{"x":1000,"y":159},{"x":7000,"y":11097},{"x":20000,"y":81569}]},{"name":"16b25f5a-1f94-49a0-b2cf-d62427000e78","values":[{"x":10,"y":1},{"x":100,"y":12},{"x":1000,"y":190},{"x":7000,"y":1887},{"x":20000,"y":6619}]}],"marks":[{"type":"line","from":{"data":"e2acd31a-e22f-4a2b-88bc-c7596cedcba3"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"blue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"4031a863-57a8-4adf-887b-6e23b2ce3761"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"red"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"1d400bf5-feba-4ed3-a6dc-445ad588cf2e"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"cyan"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"16b25f5a-1f94-49a0-b2cf-d62427000e78"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"green"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 50, :bottom 20, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"e2acd31a-e22f-4a2b-88bc-c7596cedcba3\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"e2acd31a-e22f-4a2b-88bc-c7596cedcba3\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :data ({:name \"e2acd31a-e22f-4a2b-88bc-c7596cedcba3\", :values ({:x 10, :y 0} {:x 100, :y 14} {:x 1000, :y 650} {:x 7000, :y 15083} {:x 20000, :y 101205})} {:name \"4031a863-57a8-4adf-887b-6e23b2ce3761\", :values ({:x 10, :y 0} {:x 100, :y 6} {:x 1000, :y 503} {:x 7000, :y 19982} {:x 20000, :y 167797})} {:name \"1d400bf5-feba-4ed3-a6dc-445ad588cf2e\", :values ({:x 10, :y 1} {:x 100, :y 4} {:x 1000, :y 159} {:x 7000, :y 11097} {:x 20000, :y 81569})} {:name \"16b25f5a-1f94-49a0-b2cf-d62427000e78\", :values ({:x 10, :y 1} {:x 100, :y 12} {:x 1000, :y 190} {:x 7000, :y 1887} {:x 20000, :y 6619})}), :marks ({:type \"line\", :from {:data \"e2acd31a-e22f-4a2b-88bc-c7596cedcba3\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"blue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"4031a863-57a8-4adf-887b-6e23b2ce3761\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"red\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"1d400bf5-feba-4ed3-a6dc-445ad588cf2e\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"cyan\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"16b25f5a-1f94-49a0-b2cf-d62427000e78\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"green\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; @@

;; @@
