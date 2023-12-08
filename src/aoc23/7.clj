(ns aoc23.7
  (:require [aoc23.input.7 :refer [test-input
                                   input]]
            [clojure.string :as str]))

;; --- Day 7: Camel Cards ---
;; Your all-expenses-paid trip turns out to be a one-way, five-minute ride in an airship. (At least it's a cool airship!) It drops you off at the edge of a vast desert and descends back to Island Island.

;; "Did you bring the parts?"

;; You turn around to see an Elf completely covered in white clothing, wearing goggles, and riding a large camel.

;; "Did you bring the parts?" she asks again, louder this time. You aren't sure what parts she's looking for; you're here to figure out why the sand stopped.

;; "The parts! For the sand, yes! Come with me; I will show you." She beckons you onto the camel.

;; After riding a bit across the sands of Desert Island, you can see what look like very large rocks covering half of the horizon. The Elf explains that the rocks are all along the part of Desert Island that is directly above Island Island, making it hard to even get there. Normally, they use big machines to move the rocks and filter the sand, but the machines have broken down because Desert Island recently stopped receiving the parts they need to fix the machines.

;; You've already assumed it'll be your job to figure out why the parts stopped when she asks if you can help. You agree automatically.

;; Because the journey will take a few days, she offers to teach you the game of Camel Cards. Camel Cards is sort of similar to poker except it's designed to be easier to play while riding a camel.

;; In Camel Cards, you get a list of hands, and your goal is to order them based on the strength of each hand. A hand consists of five cards labeled one of A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2. The relative strength of each card follows this order, where A is the highest and 2 is the lowest.

;; Every hand is exactly one type. From strongest to weakest, they are:

;; Five of a kind, where all five cards have the same label: AAAAA
;; Four of a kind, where four cards have the same label and one card has a different label: AA8AA
;; Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
;; Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
;; Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
;; One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
;; High card, where all cards' labels are distinct: 23456
;; Hands are primarily ordered based on type; for example, every full house is stronger than any three of a kind.

;; If two hands have the same type, a second ordering rule takes effect. Start by comparing the first card in each hand. If these cards are different, the hand with the stronger first card is considered stronger. If the first card in each hand have the same label, however, then move on to considering the second card in each hand. If they differ, the hand with the higher second card wins; otherwise, continue with the third card in each hand, then the fourth, then the fifth.

;; So, 33332 and 2AAAA are both four of a kind hands, but 33332 is stronger because its first card is stronger. Similarly, 77888 and 77788 are both a full house, but 77888 is stronger because its third card is stronger (and both hands have the same first and second card).

;; To play Camel Cards, you are given a list of hands and their corresponding bid (your puzzle input). For example:

;; 32T3K 765
;; T55J5 684
;; KK677 28
;; KTJJT 220
;; QQQJA 483
;; This example shows five hands; each hand is followed by its bid amount. Each hand wins an amount equal to its bid multiplied by its rank, where the weakest hand gets rank 1, the second-weakest hand gets rank 2, and so on up to the strongest hand. Because there are five hands in this example, the strongest hand will have rank 5 and its bid will be multiplied by 5.

;; So, the first step is to put the hands in order of strength:

;; 32T3K is the only one pair and the other hands are all a stronger type, so it gets rank 1.
;; KK677 and KTJJT are both two pair. Their first cards both have the same label, but the second card of KK677 is stronger (K vs T), so KTJJT gets rank 2 and KK677 gets rank 3.
;; T55J5 and QQQJA are both three of a kind. QQQJA has a stronger first card, so it gets rank 5 and T55J5 gets rank 4.
;; Now, you can determine the total winnings of this set of hands by adding up the result of multiplying each hand's bid with its rank (765 * 1 + 220 * 2 + 28 * 3 + 684 * 4 + 483 * 5). So the total winnings in this example are 6440.

;; Find the rank of every hand in your set. What are the total winnings?

(defn get-hand-type [hand]
  (let [cards (str/split hand #"")
        num-of-each-card (->> (map (fn [card]
                                    (let [card-count (-> (filter #(= card %) cards)
                                                         count)]
                                      card-count)) cards)
                             (reduce +))]
    (cond
      (= 25 num-of-each-card) :five-of-a-kind
      (= 17 num-of-each-card) :four-of-a-kind
      (= 13 num-of-each-card) :full-house
      (= 11 num-of-each-card) :three-of-a-kind
      (= 9 num-of-each-card) :two-pair
      (= 7 num-of-each-card) :one-pair
      :else :high-card)))

(get-hand-type "AAAAA")
(get-hand-type "AA8AA")
(get-hand-type "23332")
(get-hand-type "TTT98")
(get-hand-type "23432")
(get-hand-type "A23A4")
(get-hand-type "23456")

(get-hand-type "3Q594")
(get-hand-type "42TJ8")
(get-hand-type "3QJ68")

(defn hand->score [hand]
  (let [place->multiplier (fn [index]
                            (cond
                              (= 0 index) 100000000000
                              (= 1 index) 1000000000
                              (= 2 index) 10000000
                              (= 3 index) 10000
                              :else 10))
        cards (str/split hand #"")
        card->score (fn [card]
                      (cond
                        (= "A" card) 14
                        (= "K" card) 13
                        (= "Q" card) 12
                        (= "J" card) 11
                        (= "T" card) 10
                        :else (Integer/parseInt card)))
        hand-score (->> (map-indexed (fn [index card]
                                       (* (place->multiplier index) (card->score card)))
                                     cards)
                        (reduce +))]
    hand-score))

(hand->score "3Q594")
(hand->score "42TJ8")
(hand->score "3QJ68")


(defn calculate-group-rankings
  [rank-offset group]
  (let [offset (inc rank-offset)
        hands->comparables (map (fn [hand]
                                  (assoc hand :score (hand->score (:hand hand))))
                                group)
        sorted-comparables (sort-by :score hands->comparables)
        attached-ranks (map-indexed (fn [index hand]
                                      (println :hand (:hand hand) :rank (+ offset index))
                                      (assoc hand :rank (+ offset index))) sorted-comparables)]
    attached-ranks))

(defn solve
  [input]
  (let [hands (-> (str/split-lines input))
        hands->type (map (fn [hand-bid]
                           (let [hand (-> (str/split hand-bid #" ")
                                          first)
                                 bid (-> (str/split hand-bid #" ")
                                         second
                                         Integer/parseInt)]
                             {:hand hand
                              :bid bid
                              :type (get-hand-type hand)}))
                         hands)
        filter-by-type (fn [group type]
                         (filter #(= type (:type %)) group))
        high-cards (filter-by-type hands->type :high-card)
        high-card-rankings (calculate-group-rankings 0 high-cards)
        one-pairs (filter-by-type hands->type :one-pair)
        one-pair-rankings (calculate-group-rankings (count high-cards) one-pairs)
        two-pairs (filter-by-type hands->type :two-pair)
        two-pair-rankings (calculate-group-rankings (+ (count high-cards)
                                                       (count one-pairs)) two-pairs)
        three-of-a-kinds (filter-by-type hands->type :three-of-a-kind)
        three-of-a-kind-rankings (calculate-group-rankings (+ (count high-cards)
                                                              (count one-pairs)
                                                              (count two-pairs)) three-of-a-kinds)
        full-houses (filter-by-type hands->type :full-house)
        full-house-rankings (calculate-group-rankings (+ (count high-cards)
                                                         (count one-pairs)
                                                         (count two-pairs)
                                                         (count three-of-a-kinds)) full-houses)
        four-of-a-kinds (filter-by-type hands->type :four-of-a-kind)
        four-of-a-kind-rankings (calculate-group-rankings (+ (count high-cards)
                                                             (count one-pairs)
                                                             (count two-pairs)
                                                             (count three-of-a-kinds)
                                                             (count full-houses)) four-of-a-kinds)
        five-of-a-kinds (filter-by-type hands->type :five-of-a-kind)
        five-of-a-kind-rankings (calculate-group-rankings (+ (count high-cards)
                                                             (count one-pairs)
                                                             (count two-pairs)
                                                             (count three-of-a-kinds)
                                                             (count full-houses)
                                                             (count four-of-a-kinds)) five-of-a-kinds)
        get-winnings (fn [group]
                       (map (fn [{:keys [bid rank]}]
                              (* bid rank))
                            group))
        all-cards-with-rankings (-> (conj (get-winnings high-card-rankings)
                                          (get-winnings one-pair-rankings)
                                          (get-winnings two-pair-rankings)
                                          (get-winnings three-of-a-kind-rankings)
                                          (get-winnings full-house-rankings)
                                          (get-winnings four-of-a-kind-rankings)
                                          (get-winnings five-of-a-kind-rankings))
                                    flatten)]
    (reduce + all-cards-with-rankings)))

(solve test-input)
;; expect => 6440

(solve input)
;; returns => 245794640

(defn get-hand-type2
  "with jokers trick"
  [hand]
  (let [cards (str/split hand #"")
        jokers (count (filter #(= "J" %) cards))
        num-of-each-card (map (fn [card]
                                (let [card-count (-> (filter #(= card %) cards)
                                                     count)]
                                  {:card card :count card-count}))
                              cards)
        get-original-type (fn [summed-num]
                            (cond
                              (= 25 summed-num) :five-of-a-kind
                              (= 17 summed-num) :four-of-a-kind
                              (= 13 summed-num) :full-house
                              (= 11 summed-num) :three-of-a-kind
                              (= 9 summed-num) :two-pair
                              (= 7 summed-num) :one-pair
                              :else :high-card))]
    (if (or (= jokers 0) (= jokers 5))
      (get-original-type (reduce + (map #(:count %) num-of-each-card)))
      (let [num-of-each-non-joker (map (fn [card]
                                         (let [card-count (-> (filter #(= card %) cards)
                                                              count)]
                                           {:card card :count card-count}))
                                       (remove #(= "J" %) cards))
            most-card (:card (apply max-key :count (set num-of-each-non-joker)))
            transformed-hand (str/replace hand "J" most-card)]
        (get-hand-type2 transformed-hand)))))

(get-hand-type2 "JT4J5")
(get-hand-type2 "AAAAA")
(get-hand-type2 "AAAAJ")
(get-hand-type2 "AA8AA")
(get-hand-type2 "AA8AJ")
(get-hand-type2 "23332")
(get-hand-type2 "233J2")
(get-hand-type2 "TTT98")
(get-hand-type2 "TTJ98")
(get-hand-type2 "23432")
(get-hand-type2 "234J2")
(get-hand-type2 "A23A4")
(get-hand-type2 "A23J4")
(get-hand-type2 "23456")
(get-hand-type2 "32T3K")
(get-hand-type2 "T55J5")
(get-hand-type2 "JJJJA")
(get-hand-type2 "JJJJJ")


(defn hand->score2 [hand]
  (let [place->multiplier (fn [index]
                            (cond
                              (= 0 index) 100000000000
                              (= 1 index) 1000000000
                              (= 2 index) 10000000
                              (= 3 index) 10000
                              :else 10))
        cards (str/split hand #"")
        card->score (fn [card]
                      (cond
                        (= "A" card) 14
                        (= "K" card) 13
                        (= "Q" card) 12
                        (= "J" card) 01
                        (= "T" card) 10
                        :else (Integer/parseInt card)))
        hand-score (->> (map-indexed (fn [index card]
                                       (* (place->multiplier index) (card->score card)))
                                     cards)
                        (reduce +))]
    hand-score))

(defn calculate-group-rankings2
  [rank-offset group]
  (let [offset (inc rank-offset)
        hands->comparables (map (fn [hand]
                                  (assoc hand :score (hand->score2 (:hand hand))))
                                group)
        sorted-comparables (sort-by :score hands->comparables)
        attached-ranks (map-indexed (fn [index hand]
                                      (println :hand (:hand hand) :rank (+ offset index))
                                      (assoc hand :rank (+ offset index))) sorted-comparables)]
    attached-ranks))

(defn solve2
 "jokers wild"
  [input]
  (let [hands (-> (str/split-lines input))
        hands->type (map (fn [hand-bid]
                           (let [hand (-> (str/split hand-bid #" ")
                                          first)
                                 bid (-> (str/split hand-bid #" ")
                                         second
                                         Integer/parseInt)]
                             {:hand hand
                              :bid bid
                              :type (get-hand-type2 hand)}))
                         hands)
        filter-by-type (fn [group type]
                         (filter #(= type (:type %)) group))
        high-cards (filter-by-type hands->type :high-card)
        high-card-rankings (calculate-group-rankings2 0 high-cards)
        one-pairs (filter-by-type hands->type :one-pair)
        one-pair-rankings (calculate-group-rankings2 (count high-cards) one-pairs)
        two-pairs (filter-by-type hands->type :two-pair)
        two-pair-rankings (calculate-group-rankings2 (+ (count high-cards)
                                                       (count one-pairs)) two-pairs)
        three-of-a-kinds (filter-by-type hands->type :three-of-a-kind)
        three-of-a-kind-rankings (calculate-group-rankings2 (+ (count high-cards)
                                                              (count one-pairs)
                                                              (count two-pairs)) three-of-a-kinds)
        full-houses (filter-by-type hands->type :full-house)
        full-house-rankings (calculate-group-rankings2 (+ (count high-cards)
                                                         (count one-pairs)
                                                         (count two-pairs)
                                                         (count three-of-a-kinds)) full-houses)
        four-of-a-kinds (filter-by-type hands->type :four-of-a-kind)
        four-of-a-kind-rankings (calculate-group-rankings2 (+ (count high-cards)
                                                             (count one-pairs)
                                                             (count two-pairs)
                                                             (count three-of-a-kinds)
                                                             (count full-houses)) four-of-a-kinds)
        five-of-a-kinds (filter-by-type hands->type :five-of-a-kind)
        five-of-a-kind-rankings (calculate-group-rankings2 (+ (count high-cards)
                                                             (count one-pairs)
                                                             (count two-pairs)
                                                             (count three-of-a-kinds)
                                                             (count full-houses)
                                                             (count four-of-a-kinds)) five-of-a-kinds)
        get-winnings (fn [group]
                       (map (fn [{:keys [bid rank]}]
                              (* bid rank))
                            group))
        all-cards-with-rankings (-> (conj (get-winnings high-card-rankings)
                                          (get-winnings one-pair-rankings)
                                          (get-winnings two-pair-rankings)
                                          (get-winnings three-of-a-kind-rankings)
                                          (get-winnings full-house-rankings)
                                          (get-winnings four-of-a-kind-rankings)
                                          (get-winnings five-of-a-kind-rankings))
                                    flatten)
        ]
    (reduce + all-cards-with-rankings)))

(solve2 test-input)
;; expect => 5905

(solve2 input)
;; returns => 247899149
