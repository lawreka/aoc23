(ns aoc23.10
  (:require [aoc23.input.10 :refer [test-input1
                                    test-input2
                                    test-input3
                                   input]]
            [clojure.string :as str]))

;; | is a vertical pipe connecting north and south.
;; - is a horizontal pipe connecting east and west.
;; L is a 90-degree bend connecting north and east.
;; J is a 90-degree bend connecting north and west.
;; 7 is a 90-degree bend connecting south and west.
;; F is a 90-degree bend connecting south and east.
;; . is ground; there is no pipe in this tile.
;; S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.

(def north-connectors #{"S" "|" "7" "F"})
(def south-connectors #{"S" "|" "L" "J"})
(def east-connectors #{"S" "-" "J" "7"})
(def west-connectors #{"S" "-" "L" "F"})

(defn connected? [current-symbol next-symbol direction]
  (cond
    (= direction :north) (and (contains? south-connectors current-symbol)
                              (contains? north-connectors next-symbol))
    (= direction :south) (and (contains? north-connectors current-symbol)
                              (contains? south-connectors next-symbol))
    (= direction :east) (and (contains? west-connectors current-symbol)
                             (contains? east-connectors next-symbol))
    (= direction :west) (and (contains? east-connectors current-symbol)
                                   (contains? west-connectors next-symbol))
    :else false))

(connected? "S" "|" :north)
(connected? "|" "-" :east)

(defn solve
  [input]
  (let [lines (map-indexed (fn [index line]
                             {:line-index index
                              :line line})
                           (str/split-lines input))
        s-line (-> (filter #(str/includes? (:line %) "S") lines)
                   first) 
        s-index (str/index-of (:line s-line) "S")
        get-path (fn [line next-line prev-line pos-index]
                   (let [line-length (count line)
                         _ (print line-length)
                         current-symbol (-> (nth line pos-index)
                                            str)
                         north-node (-> (nth prev-line pos-index)
                                        str)
                         north? (connected? current-symbol north-node :north)
                         south-node (-> (nth next-line pos-index)
                                        str)
                         south? (connected? current-symbol south-node :south)
                         east-node (-> (nth line (inc pos-index))
                                       str)
                         east? (connected? current-symbol east-node :east)
                         west-node (-> (nth line (max (dec pos-index) 0))
                                       str)
                         west? (connected? current-symbol west-node :west)]
                     {:north? north? :south? south? :east? east? :west? west?}))
        follow-path (fn [line line-index pos-index]
                      (let [next-line (nth lines (inc line-index) "...")
                            prev-line (nth lines (dec line-index) "...")
                            next-nodes (get-path line (:line next-line) (:line prev-line) pos-index)]
                        
                        (print next-nodes)))
        _ (print :s-line s-line :s-index s-index)
        _ (println (follow-path (:line s-line) (:line-index s-line) s-index))
        ]
    ))

(solve test-input1)
(solve test-input2)
;; .....
;; .S-7.
;; .|.|.
;; .L-J.
;; .....
;; =
;; .....
;; .012.
;; .1.3.
;; .234.
;; .....

(solve test-input3)
;; ..F7.
;; .FJ|.
;; SJ.L7
;; |F--J
;; LJ...
;; =
;; ..45.
;; .236.
;; 01.78
;; 14567
;; 23...
