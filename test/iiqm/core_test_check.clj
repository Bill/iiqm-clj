(ns iiqm.core-test-check
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [iiqm.core :refer :all]))


(defspec insert-sorted-maintains-sort-order
         100
         (prop/for-all [v (gen/vector gen/int)
                        x gen/int]
                       (let [sorted_v (into [] (sort v))
                             sorted2 (sort (conj v x))]
                         (= (insert-sorted sorted_v x) sorted2))))
