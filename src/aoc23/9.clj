(ns aoc23.9
  (:require [aoc23.input.9 :refer [test-input
                                   input]]
            [clojure.string :as str]))

(defn get-diffs
  [nums result]
  (let [current (first nums)
        next (second nums)
        rest (rest nums)]
    (if (and current next)
      (let [diff (- next current)
            new-result (concat result [diff])]
        (if (empty? rest)
          new-result
          (recur rest new-result)))
      result)))

(def testline1 '(0 3 6 9 12 15))
(get-diffs testline1 [])

(defn get-to-zero
  [num-sequence result-sequences]
  (let [next-sequence (get-diffs num-sequence [])
        next-result-sequences (into result-sequences [next-sequence])]
    (if (every? #(= 0 %) next-sequence)
      (into result-sequences [next-sequence])
      (recur next-sequence next-result-sequences))))

(get-to-zero testline1 [])

(defn solve
  "sequencesses"
  [input]
  (let [number-sequences (->> (str/split-lines input)
                              (map (fn [line] (str/split line #" ")))
                              (map (fn [num-seq] (remove str/blank? num-seq)))
                              (map (fn [num-seq]
                                     (map (fn [num]
                                            (Long/parseLong num))
                                          num-seq))))
        reduced-sequences (map (fn [num-seq]
                                 (get-to-zero num-seq [num-seq])) number-sequences)
        next-nums (map (fn [reduced-seq]
                         (map (fn [reduced]
                                (last reduced))
                              reduced-seq))
                       reduced-sequences)]
    (reduce + (flatten next-nums))))

(solve test-input)
;; expect => 114

(solve input)

(defn get-previous-vals
  [sequences result]
  (let [base-row (first sequences)
        next-rows (rest sequences)]
    (if (empty? next-rows)
      result
      (let [diff (- (first (first next-rows)) (first base-row))
            new-next-row (conj (first next-rows) diff)
            new-result (conj result diff)]
        (if (empty? next-rows)
          new-result
          (recur (conj (rest next-rows) new-next-row) new-result))))))

(get-previous-vals ['(0 0) '(2 2 2) '(0 2 4 6) '(3 3 5 9 15) '(10 13 16 21 30 45)] [])

(defn solve2
  "sequencesses"
  [input]
  (let [number-sequences (->> (str/split-lines input)
                              (map (fn [line] (str/split line #" ")))
                              (map (fn [num-seq] (remove str/blank? num-seq)))
                              (map (fn [num-seq]
                                     (map (fn [num]
                                            (Long/parseLong num))
                                          num-seq))))
        reduced-sequences (map (fn [num-seq]
                                 (->> (get-to-zero num-seq [num-seq])
                                      reverse)) number-sequences)
        
        leftmost-nums (map (fn [num-seq]
                            (last (get-previous-vals num-seq [])))
                          reduced-sequences)]
    (reduce + leftmost-nums)))

(solve2 test-input)
;; expect => 2

(solve2 input)
;; returns => 1050
