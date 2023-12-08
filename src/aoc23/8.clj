(ns aoc23.8
  (:require [aoc23.input.8 :refer [test-input1
                                   test-input2
                                   test-input3
                                   input]]
            [clojure.math.numeric-tower :as math.num]
            [clojure.string :as str]))

;; --- Day 8: Haunted Wasteland ---
;; You're still riding a camel across Desert Island when you spot a sandstorm quickly approaching. When you turn to warn the Elf, she disappears before your eyes! To be fair, she had just finished warning you about ghosts a few minutes ago.

;; One of the camel's pouches is labeled "maps" - sure enough, it's full of documents (your puzzle input) about how to navigate the desert. At least, you're pretty sure that's what they are; one of the documents contains a list of left/right instructions, and the rest of the documents seem to describe some kind of network of labeled nodes.

;; It seems like you're meant to use the left/right instructions to navigate the network. Perhaps if you have the camel follow the same instructions, you can escape the haunted wasteland!

;; After examining the maps for a bit, two nodes stick out: AAA and ZZZ. You feel like AAA is where you are now, and you have to follow the left/right instructions until you reach ZZZ.

;; This format defines each node of the network individually. For example:

;; RL

;; AAA = (BBB, CCC)
;; BBB = (DDD, EEE)
;; CCC = (ZZZ, GGG)
;; DDD = (DDD, DDD)
;; EEE = (EEE, EEE)
;; GGG = (GGG, GGG)
;; ZZZ = (ZZZ, ZZZ)
;; Starting with AAA, you need to look up the next element based on the next left/right instruction in your input. In this example, start with AAA and go right (R) by choosing the right element of AAA, CCC. Then, L means to choose the left element of CCC, ZZZ. By following the left/right instructions, you reach ZZZ in 2 steps.

;; Of course, you might not find ZZZ right away. If you run out of left/right instructions, repeat the whole sequence of instructions as necessary: RL really means RLRLRLRLRLRLRLRL... and so on. For example, here is a situation that takes 6 steps to reach ZZZ:

;; LLR

;; AAA = (BBB, BBB)
;; BBB = (AAA, ZZZ)
;; ZZZ = (ZZZ, ZZZ)
;; Starting at AAA, follow the left/right instructions. How many steps are required to reach ZZZ?

(defn solve
  "zigzag thru the desert"
  [input]
  (let [instructions (-> (str/split-lines input)
                         first
                         (str/split #""))
        nodes (rest (->> (str/split-lines input)
                         (remove str/blank?)))
        nodes-keys (map (fn [node]
                          (-> (str/split node #" ")
                              first))
                        nodes)
        nodes-vals (map (fn [node]
                          (let [L-R (-> (str/split node #"= ")
                                        second
                                        (str/split #", "))]
                            {:L (subs (first L-R) 1 4)
                             :R (subs (second L-R) 0 3)}))
                        nodes)
        nodes-map (zipmap nodes-keys nodes-vals)
        walk (fn [position rest-instructions step-count]
               (let [instruction (first rest-instructions)
                     next-instructions (if (empty? (rest rest-instructions))
                                         instructions
                                         (rest rest-instructions))
                     next-position (get-in nodes-map [position (keyword instruction)])]
                 (if (= next-position "ZZZ")
                   (inc step-count)
                   (recur next-position next-instructions (inc step-count)))))]
    (walk "AAA" instructions 0)))

(solve test-input1)
;; expect => 2

(solve test-input2)
;; expect => 6

(solve input)
;; return => 22357

(defn solve2
  "zigzag spookily thru the desert"
  [input]
  (let [instructions (-> (str/split-lines input)
                         first
                         (str/split #""))
        nodes (rest (->> (str/split-lines input)
                         (remove str/blank?)))
        nodes-keys (map (fn [node]
                          (-> (str/split node #" ")
                              first))
                        nodes)
        nodes-vals (map (fn [node]
                          (let [L-R (-> (str/split node #"= ")
                                        second
                                        (str/split #", "))]
                            {:L (subs (first L-R) 1 4)
                             :R (subs (second L-R) 0 3)}))
                        nodes)
        nodes-map (zipmap nodes-keys nodes-vals)
        A-nodes (->> (map (fn [node-map-entry]
                            (let [last-char (-> (first node-map-entry)
                                                (str/split #"")
                                                last)]
                              (when (= "A" last-char)
                                (first node-map-entry))))
                          nodes-map)
                     (remove nil?))
        walk (fn [position rest-instructions step-count]
               (let [instruction (first rest-instructions)
                     next-instructions (if (empty? (rest rest-instructions))
                                         instructions
                                         (rest rest-instructions))
                     next-position (get-in nodes-map [position (keyword instruction)])
                     last-char-in-next-position (-> next-position
                                                    (str/split #"")
                                                    last)]
                 (if (= last-char-in-next-position "Z")
                   (inc step-count)
                   (recur next-position next-instructions (inc step-count)))))
        path-lengths (map (fn [node]
                            (walk node instructions 0))
                          A-nodes)]
    (reduce math.num/lcm path-lengths)))

(solve2 test-input3)
;; expect => 6

(solve2 input)
;; returns => 10371555451871
