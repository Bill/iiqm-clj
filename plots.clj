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
(ns foo
    (:require [gorilla-plot.core :as plot])
    (:use [clojure.data.finger-tree :only (finger-tree meter measured split-tree ft-concat ft-split-at conjl)]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(def rando (repeatedly #(rand-int 101)))

(defn weight [n]
      (- 1 (/ (mod n 4) 4)))

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
(deftype IIQM1 [coll]
         clojure.lang.IPersistentCollection
         ;; this means conj--it's called by the conj function
         (cons [iiqm1 x] (IIQM1. (into (empty coll) (sort (conj coll x)))))
         IQM
         (iqm [iiqm] (iqm-sorted-vector-reduce coll)))

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

(deftype IIQM2 [coll]
         clojure.lang.IPersistentCollection
         (cons [iiqm1 x] (IIQM2. (insert-sorted coll x)))
         IQM
         (iqm [iiqm] (iqm-sorted-vector-reduce coll)))

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


(defn benchmark [algo] (map (fn [%] [% (second (run-iiqm % algo))]) [10 100 1000 2000 3000 4000 5000]))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;foo/benchmark</span>","value":"#'foo/benchmark"}
;; <=

;; @@
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

;; Third, and last, we define the IQM (interquartile mean) function.

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

;; construct with a csr-tree
(deftype IIQM4 [csr-tree]
         clojure.lang.IPersistentCollection
         (cons [iiqm x] (IIQM4. (conj-ordered csr-tree x)))
         IQM
         (iqm [iiqm] (iqm-csr-tree csr-tree)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-class'>foo.IIQM4</span>","value":"foo.IIQM4"}
;; <=

;; @@
(benchmark (IIQM1. []))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-long'>6339278</span>","value":"6339278"}],"value":"[10 6339278]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>100</span>","value":"100"},{"type":"html","content":"<span class='clj-long'>13397987</span>","value":"13397987"}],"value":"[100 13397987]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1000</span>","value":"1000"},{"type":"html","content":"<span class='clj-long'>611699448</span>","value":"611699448"}],"value":"[1000 611699448]"}],"value":"([10 6339278] [100 13397987] [1000 611699448])"}
;; <=

;; @@
(benchmark (IIQM2. []))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-long'>10136630</span>","value":"10136630"}],"value":"[10 10136630]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>100</span>","value":"100"},{"type":"html","content":"<span class='clj-long'>23652379</span>","value":"23652379"}],"value":"[100 23652379]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1000</span>","value":"1000"},{"type":"html","content":"<span class='clj-long'>767558321</span>","value":"767558321"}],"value":"[1000 767558321]"}],"value":"([10 10136630] [100 23652379] [1000 767558321])"}
;; <=

;; @@
(benchmark (IIQM4. csr-tree))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-long'>55237035</span>","value":"55237035"}],"value":"[10 55237035]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>100</span>","value":"100"},{"type":"html","content":"<span class='clj-long'>65590088</span>","value":"65590088"}],"value":"[100 65590088]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1000</span>","value":"1000"},{"type":"html","content":"<span class='clj-long'>437075007</span>","value":"437075007"}],"value":"[1000 437075007]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2000</span>","value":"2000"},{"type":"html","content":"<span class='clj-long'>980651996</span>","value":"980651996"}],"value":"[2000 980651996]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3000</span>","value":"3000"},{"type":"html","content":"<span class='clj-long'>1123331613</span>","value":"1123331613"}],"value":"[3000 1123331613]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>4000</span>","value":"4000"},{"type":"html","content":"<span class='clj-long'>1309000366</span>","value":"1309000366"}],"value":"[4000 1309000366]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>5000</span>","value":"5000"},{"type":"html","content":"<span class='clj-long'>1638465036</span>","value":"1638465036"}],"value":"[5000 1638465036]"}],"value":"([10 55237035] [100 65590088] [1000 437075007] [2000 980651996] [3000 1123331613] [4000 1309000366] [5000 1638465036])"}
;; <=

;; @@
(defn scale [m] (map (fn [[n nanos]][n  (quot nanos 1000000)]) m))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;foo/scale</span>","value":"#'foo/scale"}
;; <=

;; @@
(scale (benchmark (IIQM4. csr-tree)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"[10 4]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>100</span>","value":"100"},{"type":"html","content":"<span class='clj-long'>38</span>","value":"38"}],"value":"[100 38]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1000</span>","value":"1000"},{"type":"html","content":"<span class='clj-long'>494</span>","value":"494"}],"value":"[1000 494]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2000</span>","value":"2000"},{"type":"html","content":"<span class='clj-long'>731</span>","value":"731"}],"value":"[2000 731]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3000</span>","value":"3000"},{"type":"html","content":"<span class='clj-long'>857</span>","value":"857"}],"value":"[3000 857]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>4000</span>","value":"4000"},{"type":"html","content":"<span class='clj-long'>1225</span>","value":"1225"}],"value":"[4000 1225]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>5000</span>","value":"5000"},{"type":"html","content":"<span class='clj-long'>1521</span>","value":"1521"}],"value":"[5000 1521]"}],"value":"([10 4] [100 38] [1000 494] [2000 731] [3000 857] [4000 1225] [5000 1521])"}
;; <=

;; @@
(plot/list-plot (scale (benchmark (IIQM4. csr-tree))) :color "green" :joined true)
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":50,"bottom":20,"right":10},"data":[{"name":"c8881435-4ac9-4717-929f-f2f5a54bc6b7","values":[{"x":10,"y":2},{"x":100,"y":27},{"x":1000,"y":265},{"x":2000,"y":526},{"x":3000,"y":918},{"x":4000,"y":1166},{"x":5000,"y":1538},{"x":10000,"y":3498},{"x":20000,"y":7361}]}],"marks":[{"type":"line","from":{"data":"c8881435-4ac9-4717-929f-f2f5a54bc6b7"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"green"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"c8881435-4ac9-4717-929f-f2f5a54bc6b7","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"c8881435-4ac9-4717-929f-f2f5a54bc6b7","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 50, :bottom 20, :right 10}, :data [{:name \"c8881435-4ac9-4717-929f-f2f5a54bc6b7\", :values ({:x 10, :y 2} {:x 100, :y 27} {:x 1000, :y 265} {:x 2000, :y 526} {:x 3000, :y 918} {:x 4000, :y 1166} {:x 5000, :y 1538} {:x 10000, :y 3498} {:x 20000, :y 7361})}], :marks [{:type \"line\", :from {:data \"c8881435-4ac9-4717-929f-f2f5a54bc6b7\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"green\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"c8881435-4ac9-4717-929f-f2f5a54bc6b7\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"c8881435-4ac9-4717-929f-f2f5a54bc6b7\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; @@
(plot/compose
  (plot/list-plot (scale (benchmark (IIQM1. []))) :color "blue" :joined true )
  (plot/list-plot (scale (benchmark (IIQM2. []))) :color "red" :joined true)
  (plot/list-plot (scale (benchmark (IIQM4. csr-tree))) :color "green" :joined true))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":50,"bottom":20,"right":10},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"2cc07867-a664-42cd-b4e7-505cd599b88f","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"2cc07867-a664-42cd-b4e7-505cd599b88f","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"data":[{"name":"2cc07867-a664-42cd-b4e7-505cd599b88f","values":[{"x":10,"y":0},{"x":100,"y":4},{"x":1000,"y":252},{"x":2000,"y":999},{"x":3000,"y":2194},{"x":4000,"y":3569},{"x":5000,"y":5473},{"x":10000,"y":23366},{"x":20000,"y":94156}]},{"name":"7dad938a-689f-4588-94ae-da0188079b6d","values":[{"x":10,"y":0},{"x":100,"y":4},{"x":1000,"y":294},{"x":2000,"y":1336},{"x":3000,"y":3115},{"x":4000,"y":5599},{"x":5000,"y":8313},{"x":10000,"y":36673},{"x":20000,"y":156086}]},{"name":"fa0bb4a5-c3b8-4f3b-8804-6761f65e1a5f","values":[{"x":10,"y":3},{"x":100,"y":13},{"x":1000,"y":245},{"x":2000,"y":594},{"x":3000,"y":863},{"x":4000,"y":1247},{"x":5000,"y":1579},{"x":10000,"y":3532},{"x":20000,"y":7663}]}],"marks":[{"type":"line","from":{"data":"2cc07867-a664-42cd-b4e7-505cd599b88f"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"blue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"7dad938a-689f-4588-94ae-da0188079b6d"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"red"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"fa0bb4a5-c3b8-4f3b-8804-6761f65e1a5f"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"green"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 50, :bottom 20, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"2cc07867-a664-42cd-b4e7-505cd599b88f\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"2cc07867-a664-42cd-b4e7-505cd599b88f\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :data ({:name \"2cc07867-a664-42cd-b4e7-505cd599b88f\", :values ({:x 10, :y 0} {:x 100, :y 4} {:x 1000, :y 252} {:x 2000, :y 999} {:x 3000, :y 2194} {:x 4000, :y 3569} {:x 5000, :y 5473} {:x 10000, :y 23366} {:x 20000, :y 94156})} {:name \"7dad938a-689f-4588-94ae-da0188079b6d\", :values ({:x 10, :y 0} {:x 100, :y 4} {:x 1000, :y 294} {:x 2000, :y 1336} {:x 3000, :y 3115} {:x 4000, :y 5599} {:x 5000, :y 8313} {:x 10000, :y 36673} {:x 20000, :y 156086})} {:name \"fa0bb4a5-c3b8-4f3b-8804-6761f65e1a5f\", :values ({:x 10, :y 3} {:x 100, :y 13} {:x 1000, :y 245} {:x 2000, :y 594} {:x 3000, :y 863} {:x 4000, :y 1247} {:x 5000, :y 1579} {:x 10000, :y 3532} {:x 20000, :y 7663})}), :marks ({:type \"line\", :from {:data \"2cc07867-a664-42cd-b4e7-505cd599b88f\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"blue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"7dad938a-689f-4588-94ae-da0188079b6d\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"red\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"fa0bb4a5-c3b8-4f3b-8804-6761f65e1a5f\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"green\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; @@

;; @@
