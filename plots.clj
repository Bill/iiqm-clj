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
  (:require [gorilla-plot.core :as plot]))
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

(defn iqm-sorted-vector [coll]
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
  (iqm [iiqm] (iqm-sorted-vector coll)))

(defn find-insertion-point
  "Given a sorted vector, returns the index at which to insert x to maintain sort order."
  ([sorted-vec x] (find-insertion-point sorted-vec x 0 (count sorted-vec)))
  ([sorted-vec x a b]
   (if (>= a b)
     b
     (let [mid (+ a (quot (- b a) 2))
           c   (compare x (nth sorted-vec mid))]
       (case c
         -1 (recur sorted-vec x a mid)
         0 mid
         1 (recur sorted-vec x (inc mid) b))))))


(deftype IIQM2 [coll]
  clojure.lang.IPersistentCollection
  (cons [iiqm1 x] (IIQM1. (let [i (find-insertion-point coll x)]
                            (into (empty coll)
                                  (concat
                                    (take i coll) [x] (nthrest coll i))))))
  IQM
  (iqm [iiqm] (iqm-sorted-vector coll)))

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

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;foo/benchmark</span>","value":"#'foo/benchmark"}
;; <=

;; @@
(benchmark (IIQM1. []))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-long'>4932416</span>","value":"4932416"}],"value":"[10 4932416]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>100</span>","value":"100"},{"type":"html","content":"<span class='clj-long'>14595437</span>","value":"14595437"}],"value":"[100 14595437]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1000</span>","value":"1000"},{"type":"html","content":"<span class='clj-long'>687776521</span>","value":"687776521"}],"value":"[1000 687776521]"}],"value":"([10 4932416] [100 14595437] [1000 687776521])"}
;; <=

;; @@
(benchmark (IIQM2. []))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-long'>1078053</span>","value":"1078053"}],"value":"[10 1078053]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>100</span>","value":"100"},{"type":"html","content":"<span class='clj-long'>8264447</span>","value":"8264447"}],"value":"[100 8264447]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1000</span>","value":"1000"},{"type":"html","content":"<span class='clj-long'>612540407</span>","value":"612540407"}],"value":"[1000 612540407]"}],"value":"([10 1078053] [100 8264447] [1000 612540407])"}
;; <=

;; @@
(defn scale [m] (map (fn [[n nanos]][n  (quot nanos 1000000)]) m))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;foo/scale</span>","value":"#'foo/scale"}
;; <=

;; @@
(plot/compose
  (plot/list-plot (scale (benchmark (IIQM1. []))) :color "blue" :joined true )
  (plot/list-plot (scale (benchmark (IIQM2. []))) :color "red" :joined true))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":50,"bottom":20,"right":10},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"27896c82-91bd-4b21-bebb-162de671f1c0","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"27896c82-91bd-4b21-bebb-162de671f1c0","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"data":[{"name":"27896c82-91bd-4b21-bebb-162de671f1c0","values":[{"x":10,"y":1},{"x":100,"y":13},{"x":1000,"y":675}]},{"name":"1c0653c9-4b02-4b1c-8e64-3a8a63f0885c","values":[{"x":10,"y":0},{"x":100,"y":6},{"x":1000,"y":446}]}],"marks":[{"type":"line","from":{"data":"27896c82-91bd-4b21-bebb-162de671f1c0"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"blue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"1c0653c9-4b02-4b1c-8e64-3a8a63f0885c"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"red"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 50, :bottom 20, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"27896c82-91bd-4b21-bebb-162de671f1c0\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"27896c82-91bd-4b21-bebb-162de671f1c0\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :data ({:name \"27896c82-91bd-4b21-bebb-162de671f1c0\", :values ({:x 10, :y 1} {:x 100, :y 13} {:x 1000, :y 675})} {:name \"1c0653c9-4b02-4b1c-8e64-3a8a63f0885c\", :values ({:x 10, :y 0} {:x 100, :y 6} {:x 1000, :y 446})}), :marks ({:type \"line\", :from {:data \"27896c82-91bd-4b21-bebb-162de671f1c0\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"blue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"1c0653c9-4b02-4b1c-8e64-3a8a63f0885c\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"red\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; @@

;; @@
