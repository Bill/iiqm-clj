(ns iiqm.experiments
  (:use [clojure.data.finger-tree :only (finger-tree meter measured split-tree ft-concat ft-split-at conjl)]))

;; Some experiments with finger trees

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


;; OK let's exercise some of our definitions...

(def s (take 10 rando))
(def ct (apply conj count-tree s))
(def st (apply conj sum-tree s))
(def rt (apply conj right-tree s))
(def cst (apply conj count-sum-tree s))

(measured ct)
(measured st)
(measured rt)
(measured cst)


(comment (let [powers #{10 100 1000}]
           (loop [n 0 t recess-tree]
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

