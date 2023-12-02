(ns aoc23.2
  (:require [aoc23.input.2 :refer [test-input
                                   input]]
            [clojure.string :as str]))

;; --- Day 2: Cube Conundrum ---
;; You're launched high into the atmosphere! The apex of your trajectory just barely reaches the surface of a large island floating in the sky. You gently land in a fluffy pile of leaves. It's quite cold, but you don't see much snow. An Elf runs over to greet you.

;; The Elf explains that you've arrived at Snow Island and apologizes for the lack of snow. He'll be happy to explain the situation, but it's a bit of a walk, so you have some time. They don't get many visitors up here; would you like to play a game in the meantime?

;; As you walk, the Elf shows you a small bag and some cubes which are either red, green, or blue. Each time you play this game, he will hide a secret number of cubes of each color in the bag, and your goal is to figure out information about the number of cubes.

;; To get information, once a bag has been loaded with cubes, the Elf will reach into the bag, grab a handful of random cubes, show them to you, and then put them back in the bag. He'll do this a few times per game.

;; You play several games and record the information from each game (your puzzle input). Each game is listed with its ID number (like the 11 in Game 11: ...) followed by a semicolon-separated list of subsets of cubes that were revealed from the bag (like 3 red, 5 green, 4 blue).

;; For example, the record of a few games might look like this:

;; Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
;; Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
;; Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
;; Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
;; Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
;; In game 1, three sets of cubes are revealed from the bag (and then put back again). The first set is 3 blue cubes and 4 red cubes; the second set is 1 red cube, 2 green cubes, and 6 blue cubes; the third set is only 2 green cubes.

;; The Elf would first like to know which games would have been possible if the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?

;; In the example above, games 1, 2, and 5 would have been possible if the bag had been loaded with that configuration. However, game 3 would have been impossible because at one point the Elf showed you 20 red cubes at once; similarly, game 4 would also have been impossible because the Elf showed you 15 blue cubes at once. If you add up the IDs of the games that would have been possible, you get 8.

;; Determine which games would have been possible if the bag had been loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum of the IDs of those games?


(defn solve
  "Cubes game"
  [{:keys [input reds greens blues]}]
  (let [lines (seq (str/split-lines input))
        get-color (fn [s]
                    (cond
                      (str/includes? s "red") :red
                      (str/includes? s "green") :green
                      (str/includes? s "blue") :blue))
        valid-cube-set? (fn [{:keys [color count]}]
                          (cond
                            (= color :red) (<= count reds)
                            (= color :green) (<= count greens)
                            (= color :blue) (<= count blues)))
        games (map (fn [line]
                     (let [game-no (-> line
                                       (str/split #":")
                                       first
                                       str/trim
                                       (str/split #" ")
                                       last
                                       Integer/parseInt)
                           cubes-shown (-> line
                                           (str/split #":")
                                           rest
                                           first
                                           (str/split #";"))
                           cube-sets (map (fn [set]
                                            (let [num-color-sets (or (str/split set #",") set)]
                                              (map (fn [num-color]
                                                     (let [num (-> num-color
                                                                   str/trim
                                                                   (str/split #" ")
                                                                   first
                                                                   Integer/parseInt)
                                                           color (get-color num-color)
                                                           valid-cube-set? (valid-cube-set? {:count num :color color})]
                                                       valid-cube-set?))
                                                   num-color-sets)))
                                          cubes-shown)
                           valid? (every? true? (flatten cube-sets))]
                       (if valid?
                         game-no
                         0)))
                   lines)]
    (reduce + games)))

;; expected => 8
(solve {:input test-input
        :reds 12
        :greens 13
        :blues 14})

(solve {:input input
        :reds 12
        :greens 13
        :blues 14})
;; returns => 2377

(defn solve2
  "Power of cubes"
  [input]
  (let [lines (seq (str/split-lines input))
        get-color (fn [s]
                    (cond
                      (str/includes? s "red") :red
                      (str/includes? s "green") :green
                      (str/includes? s "blue") :blue))
        cube-set-by-color (fn [cubes-shown color-key]
                            (map (fn [set]
                                   (let [num-color-sets (or (str/split set #",") set)]
                                     (map (fn [num-color]
                                            (let [num (-> num-color
                                                          str/trim
                                                          (str/split #" ")
                                                          first
                                                          Integer/parseInt)
                                                  color (get-color num-color)]
                                              (if (= color color-key)
                                                num
                                                0)))
                                          num-color-sets)))
                                 cubes-shown))
        games (map (fn [line]
                     (let [cubes-shown (-> line
                                           (str/split #":")
                                           rest
                                           first
                                           (str/split #";"))
                           red-cube-sets (cube-set-by-color cubes-shown :red)
                           green-cube-sets (cube-set-by-color cubes-shown :green)
                           blue-cube-sets (cube-set-by-color cubes-shown :blue)
                           min-red (apply max (flatten red-cube-sets))
                           min-green (apply max (flatten green-cube-sets))
                           min-blue (apply max (flatten blue-cube-sets))]
                       ;; {:red min-red :green min-green :blue min-blue}
                       (* min-red min-green min-blue)))
                   lines)]
    (reduce + games)))

;; expect 4 red, 2 green, 6 blue 
;; expect 1 red, 3 green, 4 blue cubes.
;; expect 20 red, 13 green, 6 blue
;; expect 14 red, 3 green, 15 blue
;; expect 6 red, 3 green, 2 blue
;; expect (48, 12, 1560, 630, 36)
;; expect => 2286
(solve2 test-input)

(solve2 input)
;; returns => 71220
