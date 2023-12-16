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
                   (let [north-node (nth prev-line pos-index)
                         south-node (nth next-line pos-index)
                         east-node (nth line (inc pos-index))
                         west-node (nth line (dec pos-index))]
                     {:north north-node :south south-node :east east-node :west west-node}))
        follow-path (fn [line line-index pos-index]
                      (let [next-line (nth lines (inc line-index) "...")
                            prev-line (nth lines (dec line-index) "...")
                            next-nodes (get-path line (:line next-line) (:line prev-line) pos-index)]
                        (print next-nodes)))
        _ (print :s-line s-line :s-index s-index)
        _ (println follow-path)
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
