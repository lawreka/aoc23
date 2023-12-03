(ns aoc23.3
  (:require [aoc23.input.3 :refer [test-input
                                   input]]
            [clojure.string :as str]))

;; --- Day 3: Gear Ratios ---
;; You and the Elf eventually reach a gondola lift station; he says the gondola lift will take you up to the water source, but this is as far as he can bring you. You go inside.

;; It doesn't take long to find the gondolas, but there seems to be a problem: they're not moving.

;; "Aaah!"

;; You turn around to see a slightly-greasy Elf with a wrench and a look of surprise. "Sorry, I wasn't expecting anyone! The gondola lift isn't working right now; it'll still be a while before I can fix it." You offer to help.

;; The engineer explains that an engine part seems to be missing from the engine, but nobody can figure out which one. If you can add up all the part numbers in the engine schematic, it should be easy to work out which part is missing.

;; The engine schematic (your puzzle input) consists of a visual representation of the engine. There are lots of numbers and symbols you don't really understand, but apparently any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum. (Periods (.) do not count as a symbol.)

;; Here is an example engine schematic:

;; 467..114..
;; ...*......
;; ..35..633.
;; ......#...
;; 617*......
;; .....+.58.
;; ..592.....
;; ......755.
;; ...$.*....
;; .664.598..
;; In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 114 (top right) and 58 (middle right). Every other number is adjacent to a symbol and so is a part number; their sum is 4361.

;; Of course, the actual engine schematic is much larger. What is the sum of all of the part numbers in the engine schematic?

(defn symbol-char? [char]
  (not (or (re-matches #"\." char)
           (re-matches #"\d*" char))))

(symbol-char? ".")
(symbol-char? "9")
(symbol-char? "*")
(symbol-char? "617*")

(defn int-in-range? [range int]
  (and (>= int (first range))
       (<= int (last range))))

(int-in-range? [0 1 2] 0)
(int-in-range? [0 1 2] 3)

(defn get-symbol-indexes [line]
  (->> (map-indexed (fn [index char]
                      (when (symbol-char? (str char))
                        index))
                    line)
       (remove nil?)
       seq))

(get-symbol-indexes "467..114..")
(get-symbol-indexes "...*......")
(get-symbol-indexes "617*......")

(defn line->numbers [line]
  (let [line-no-symbols (str/replace line #"[^\d\.]" ".")
        line-no-dots (str/split line-no-symbols #"\.")
        strip-empties (remove #(re-matches #"" %) line-no-dots)]
    (remove empty? strip-empties)))

(line->numbers "467..114..")
(line->numbers "...*......")
(line->numbers "617*......")
(line->numbers "...................703..332*259..+192.....504.....=......539..693...64..567*......*487................633.200.......886...550.........129...")

(defn numbers->positions
  ([nums-list line] (numbers->positions nums-list line [] 0))
  ([nums-list line found-nums offset]
   (let [num (first nums-list)
         rest-nums (rest nums-list)
         num-start (str/index-of line num)
         num-end (-> num-start
                     (+ (count num))
                     (- 1))
         found (conj found-nums {:num num
                                 :start (+ offset num-start)
                                 :end (+ offset num-end)})]
     (if (not-empty rest-nums)
       (let [new-line (subs line (+ num-start (count num)))
             new-offset (+ offset (- (count line) (count new-line)))]
         (recur rest-nums new-line found new-offset))
       found))))

;; expect [{:num "27" :start 2 :end 3}
;;         {:num "27" :start 6 :end 7}
;;         {:num "33" :start 10 :end 11}]
(def test-line "..27..27..33")
(numbers->positions (line->numbers test-line) test-line)

(defn solve
  "Add all numbers that are symbol-adjacent"
  [input]
  (let [lines (seq (str/split-lines input))
        nums-in-range (map-indexed (fn [index line]
                                     (let [current-line line
                                           next-line (nth lines (inc index) "...")
                                           prev-line (nth lines (dec index) "...")
                                           current-line-symbol-indexes (get-symbol-indexes current-line)
                                           next-line-symbol-indexes (get-symbol-indexes next-line)
                                           prev-line-symbol-indexes (get-symbol-indexes prev-line)
                                           surrounding-symbol-indexes (concat prev-line-symbol-indexes current-line-symbol-indexes next-line-symbol-indexes)
                                           symbol-ranges (map (fn [symbol-index]
                                                                [(dec symbol-index) symbol-index (inc symbol-index)])
                                                              surrounding-symbol-indexes)
                                           numbers (line->numbers line) 
                                           numbers-with-positions (when (not-empty numbers)
                                                                    (numbers->positions numbers line)) 
                                           nums-in-range (map (fn [{:keys [num start end]}]
                                                                (let [touching-symbol? (map (fn [symbol-range]
                                                                                              (let [start-in-range? (int-in-range? symbol-range start)
                                                                                                    end-in-range? (int-in-range? symbol-range end)]
                                                                                                (or start-in-range? end-in-range?)))
                                                                                            symbol-ranges)]
                                                                  {:num num :near-a-symbol? (some true? touching-symbol?)}))
                                                              numbers-with-positions)]
                                       nums-in-range))
                                   lines)
        schematic-nums (map (fn [{:keys [num near-a-symbol?]}]
                              (if near-a-symbol?
                                (Integer/parseInt num)
                                0))
                            (flatten nums-in-range))]
    (reduce + schematic-nums)))


;; expect (467, 35, 633, 617, 592, 755, 664, 598)
;; expect => 4361
(solve test-input)

(solve input)
;; returns 546563

(defn get-gear-indexes [line]
  (->> (map-indexed (fn [index char]
                      (when (re-matches #"\*" (str char))
                        index))
                    line)
       (remove nil?)
       seq))

(get-gear-indexes "467..114..")
(get-gear-indexes "...*......")
(get-gear-indexes "617*......")

(defn gear-ratio
  [star-index prev-line curr-line next-line]
  (let [star-range [(dec star-index) star-index (inc star-index)]
        line-num-positions (fn [line]
                             (when (not-empty (line->numbers line))
                               (numbers->positions (line->numbers line) line)))
        nums-in-star-range (fn [nums]
                             (map (fn [{:keys [num start end]}]
                                    (let [start-in-range? (int-in-range? star-range start)
                                          end-in-range? (int-in-range? star-range end)]
                                      (when (or start-in-range? end-in-range?)
                                        num)))
                                  nums))
        prev-line-nums (line-num-positions prev-line)
        prev-line-nums-in-range (nums-in-star-range prev-line-nums)
        curr-line-nums (line-num-positions curr-line)
        curr-line-nums-in-range (nums-in-star-range curr-line-nums)
        next-line-nums (line-num-positions next-line)
        next-line-nums-in-range (nums-in-star-range next-line-nums)
        all-nums-in-range (->> (concat prev-line-nums-in-range curr-line-nums-in-range next-line-nums-in-range)
                               (remove nil?))]
    (if (= 2 (count all-nums-in-range))
      (* (Integer/parseInt (first all-nums-in-range)) (Integer/parseInt (last all-nums-in-range)))
      0)))

(defn solve2
  "Gear ratios
   Only * with exactly two adjacent numbers count
   Multiplying those two numbers, reduce all"
  [input]
  (let [lines (seq (str/split-lines input))
        gears (->> (map-indexed (fn [index line]
                             (let [curr-line line
                                   next-line (nth lines (inc index) "...")
                                   prev-line (nth lines (dec index) "...")
                                   star-indexes (get-gear-indexes curr-line)
                                   gear-ratios (map (fn [star-index]
                                                      (gear-ratio star-index prev-line curr-line next-line))
                                                    star-indexes)]
                               gear-ratios))
                           lines)
                  (remove empty?))]
    (reduce + (flatten gears))))

;; expect (+ (* 467 35) (* 755 598))
;; expect => 467835
(solve2 test-input)

(solve2 input)
;; returns 91031374
