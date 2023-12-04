(ns aoc23.4
  (:require [aoc23.input.4 :refer [test-input
                                   input]]
            [clojure.math :as math]
            [clojure.string :as str]
            [clojure.set :as clj.set]))

;; --- Day 4: Scratchcards ---
;; The gondola takes you up. Strangely, though, the ground doesn't seem to be coming with you; you're not climbing a mountain. As the circle of Snow Island recedes below you, an entire new landmass suddenly appears above you! The gondola carries you to the surface of the new island and lurches into the station.

;; As you exit the gondola, the first thing you notice is that the air here is much warmer than it was on Snow Island. It's also quite humid. Is this where the water source is?

;; The next thing you notice is an Elf sitting on the floor across the station in what seems to be a pile of colorful square cards.

;; "Oh! Hello!" The Elf excitedly runs over to you. "How may I be of service?" You ask about water sources.

;; "I'm not sure; I just operate the gondola lift. That does sound like something we'd have, though - this is Island Island, after all! I bet the gardener would know. He's on a different island, though - er, the small kind surrounded by water, not the floating kind. We really need to come up with a better naming scheme. Tell you what: if you can help me with something quick, I'll let you borrow my boat and you can go visit the gardener. I got all these scratchcards as a gift, but I can't figure out what I've won."

;; The Elf leads you over to the pile of colorful cards. There, you discover dozens of scratchcards, all with their opaque covering already scratched off. Picking one up, it looks like each card has two lists of numbers separated by a vertical bar (|): a list of winning numbers and then a list of numbers you have. You organize the information into a table (your puzzle input).

;; As far as the Elf has been able to figure out, you have to figure out which of the numbers you have appear in the list of winning numbers. The first match makes the card worth one point and each match after the first doubles the point value of that card.

;; For example:

;; Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
;; Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
;; Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
;; Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
;; Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
;; Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
;; In the above example, card 1 has five winning numbers (41, 48, 83, 86, and 17) and eight numbers you have (83, 86, 6, 31, 17, 9, 48, and 53). Of the numbers you have, four of them (48, 83, 17, and 86) are winning numbers! That means card 1 is worth 8 points (1 for the first match, then doubled three times for each of the three matches after the first).

;; Card 2 has two winning numbers (32 and 61), so it is worth 2 points.
;; Card 3 has two winning numbers (1 and 21), so it is worth 2 points.
;; Card 4 has one winning number (84), so it is worth 1 point.
;; Card 5 has no winning numbers, so it is worth no points.
;; Card 6 has no winning numbers, so it is worth no points.
;; So, in this example, the Elf's pile of scratchcards is worth 13 points.

;; Take a seat in the large pile of colorful cards. How many points are they worth in total?

(defn string->set [first-or-last line]
  (let [line->numbers (-> line
                          (str/split #":")
                          last
                          (str/split #"\|")
                          first-or-last
                          str/trim
                          (str/split #" "))]
    (set (remove str/blank? line->numbers))))

(def test-line "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")
(string->set first test-line)
(string->set last test-line)
(def test-line2 "Card 1111: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")


(defn power-of-2 [x]
  (int (math/pow 2 x)))

(power-of-2 2)
(power-of-2 3)

(defn solve
  "Split game numbers and win numbers
   Find intersection of sets
   Calculate point totals"
  [input]
  (let [lines (seq (str/split-lines input))
        point-totals (map (fn [line]
                            (let [winning-numbers (string->set first line)
                                  scratcher-numbers (string->set last line)
                                  overlaps (clj.set/intersection winning-numbers scratcher-numbers)
                                  point-total (if (<= (count overlaps) 2)
                                                (count overlaps)
                                                (power-of-2 (- (count overlaps) 1)))]
                              point-total))
                          lines)]
    (reduce + point-totals)))

(solve test-input)
;; expect (+ 2^3 2 2 1 0 0)
;; expect => 13

(solve input)
;; returns => 24733

(defn card->number [line]
  (-> line
      (str/split #":")
      first
      (str/split #" ")
      last
      Integer/parseInt))

(card->number test-line)
(card->number test-line2)

(defn get-card-dupe-range
  [card-number line]
  (let [winning-numbers (string->set first line)
        scratcher-numbers (string->set last line)
        matches (clj.set/intersection winning-numbers scratcher-numbers)
        dupe-range (range (inc card-number) (+ (inc card-number) (count matches)))]
    dupe-range))

(defn add-lines
  ([lines]
   (let [original-cards lines
         cards-as-numbers (map #(card->number %) lines)]
     (add-lines cards-as-numbers original-cards 0)))
  ([rest-lines original-cards card-count]
   (let [current-card-number (first rest-lines)
         rest-cards (rest rest-lines)
         current-card-index-in-originals (dec current-card-number)
         current-card (nth original-cards current-card-index-in-originals)
         dupes (get-card-dupe-range current-card-number current-card)
         rest-plus-dupes (into rest-cards dupes)]
     (if (and true #_(> 1000 card-count) (not-empty rest-plus-dupes))
       (recur rest-plus-dupes original-cards (inc card-count))
       (inc card-count)))))

(defn solve2
  "Amount of matches wins you more cards"
  [input]
  (let [lines (seq (str/split-lines input))]
    (add-lines lines)))

(solve2 test-input)
;; you end up with 1 instance of card 1, 2 instances of card 2, 4 instances of card 3, 8 instances of card 4, 14 instances of card 5, and 1 instance of card 6. In total, this example pile of scratchcards causes you to ultimately have 30
;; expect => 30

(solve2 input)
;; returns => 5422730
