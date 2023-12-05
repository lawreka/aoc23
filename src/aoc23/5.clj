(ns aoc23.5
  (:require [aoc23.input.5 :refer [test-input
                                   input]]
            [clojure.string :as str]))

(defn map->range [input index]
  (-> input
      (str/split #"map:")
      (nth index)
      str/split-lines))

(defn trim-ranges-only [line-seq drop-last?]
  (let [lines (if drop-last? (drop-last line-seq) line-seq)]
    (->> (map (fn [line]
                (when (not (str/blank? line))
                  (-> line
                      str/trim
                      (str/split #" "))))
              lines)
         (remove nil?))))

(defn range->long [range]
  (map (fn [range-str]
         (Long/parseLong (str range-str)))
       range))

(defn populate-helpful-data-for-ranges [ranges]
  (map (fn [range]
         (let [range-as-longs (range->long range)
               shifted-seeds-start (nth range-as-longs 1)
               shifted-seeds-end (dec (+ (nth range-as-longs 1)
                                         (nth range-as-longs 2)))]
           {:range range-as-longs
            :shifted-start shifted-seeds-start
            :shifted-end shifted-seeds-end
            :start-point-replacement (nth range-as-longs 0)}))
       ranges))

(defn input->ranges [input]
  (let [seeds (-> input
                  str/split-lines
                  first
                  (str/split #":")
                  last
                  str/trim
                  (str/split #" "))
        seed-to-soil (-> (map->range input 1)
                         (trim-ranges-only true)
                         populate-helpful-data-for-ranges)
        soil-to-fertilizer (-> (map->range input 2)
                               (trim-ranges-only true)
                               populate-helpful-data-for-ranges)
        fertilizer-to-water (-> (map->range input 3)
                                (trim-ranges-only true)
                                populate-helpful-data-for-ranges)
        water-to-light (-> (map->range input 4)
                           (trim-ranges-only true)
                           populate-helpful-data-for-ranges)
        light-to-temperature (-> (map->range input 5)
                                 (trim-ranges-only true)
                                 populate-helpful-data-for-ranges)
        temperature-to-humidity (-> (map->range input 6)
                                    (trim-ranges-only true)
                                    populate-helpful-data-for-ranges)
        humidity-to-location (-> (map->range input 7)
                                 (trim-ranges-only true)
                                 populate-helpful-data-for-ranges)]
    {:seeds seeds
     :seed-to-soil {:ranges seed-to-soil}
     :soil-to-fertilizer {:ranges soil-to-fertilizer}
     :fertilizer-to-water {:ranges fertilizer-to-water}
     :water-to-light {:ranges water-to-light}
     :light-to-temperature {:ranges light-to-temperature}
     :temperature-to-humidity {:ranges temperature-to-humidity}
     :humidity-to-location {:ranges humidity-to-location}}))

(defn seed->soil [seed soil-ranges]
  (let [seed-number (Long/parseLong (str seed))
        soil-numbers (map (fn [range]
                           (if (and (>= seed-number (:shifted-start range))
                                    (<= seed-number (:shifted-end range)))
                             (let [offset (- seed-number (:shifted-start range))
                                   soil-number (+ (:start-point-replacement range) offset)]
                               soil-number)
                             seed-number))
                         soil-ranges)]
    (if (some #(not= seed-number %) soil-numbers)
      (-> (filter #(not= seed-number %) soil-numbers)
          first)
      seed-number)))

(defn solve
  "fuckin seed elves bullshit"
  [input]
  (let [map-of-ranges (input->ranges input)
        get-in-next-range (fn [numbers next-range]
                            (map (fn [num]
                                   (let [next-num (seed->soil num next-range)]
                                     next-num))
                                 numbers))
        soil-numbers (get-in-next-range (:seeds map-of-ranges) (get-in map-of-ranges [:seed-to-soil :ranges]))
        fertilizer-numbers (get-in-next-range soil-numbers (get-in map-of-ranges [:soil-to-fertilizer :ranges]))
        water-numbers (get-in-next-range fertilizer-numbers (get-in map-of-ranges [:fertilizer-to-water :ranges]))
        light-numbers (get-in-next-range water-numbers (get-in map-of-ranges [:water-to-light :ranges]))
        temperature-numbers (get-in-next-range light-numbers (get-in map-of-ranges [:light-to-temperature :ranges]))
        humidity-numbers (get-in-next-range temperature-numbers (get-in map-of-ranges [:temperature-to-humidity :ranges]))
        location-numbers (get-in-next-range humidity-numbers (get-in map-of-ranges [:humidity-to-location :ranges]))]
    (apply min location-numbers)))

(solve test-input)
;; Seed 79, soil 81, fertilizer 81, water 81, light 74, temperature 78, humidity 78, location 82.
;; Seed 14, soil 14, fertilizer 53, water 49, light 42, temperature 42, humidity 43, location 43.
;; Seed 55, soil 57, fertilizer 57, water 53, light 46, temperature 82, humidity 82, location 86.
;; Seed 13, soil 13, fertilizer 52, water 41, light 34, temperature 34, humidity 35, location 35.
;; expect => 35
(solve input)
;; returns => 340994526


(defn input->ranges2 [input]
  (let [seeds (-> input
                  str/split-lines
                  first
                  (str/split #":")
                  last
                  str/trim
                  (str/split #" "))
        more-seeds (->> (map (fn [seed-range]
                               (let [start (Long/parseLong (first seed-range))
                                     length (Long/parseLong (last seed-range))]
                                 {:start start
                                  :end (dec (+ start length))}))
                             (partition 2 seeds))
                        flatten)
        seed-to-soil (-> (map->range input 1)
                         (trim-ranges-only true)
                         populate-helpful-data-for-ranges)
        soil-to-fertilizer (-> (map->range input 2)
                               (trim-ranges-only true)
                               populate-helpful-data-for-ranges)
        fertilizer-to-water (-> (map->range input 3)
                                (trim-ranges-only true)
                                populate-helpful-data-for-ranges)
        water-to-light (-> (map->range input 4)
                           (trim-ranges-only true)
                           populate-helpful-data-for-ranges)
        light-to-temperature (-> (map->range input 5)
                                 (trim-ranges-only true)
                                 populate-helpful-data-for-ranges)
        temperature-to-humidity (-> (map->range input 6)
                                    (trim-ranges-only true)
                                    populate-helpful-data-for-ranges)
        humidity-to-location (-> (map->range input 7)
                                 (trim-ranges-only true)
                                 populate-helpful-data-for-ranges)]
    {:seeds more-seeds
     :seed-to-soil {:ranges seed-to-soil}
     :soil-to-fertilizer {:ranges soil-to-fertilizer}
     :fertilizer-to-water {:ranges fertilizer-to-water}
     :water-to-light {:ranges water-to-light}
     :light-to-temperature {:ranges light-to-temperature}
     :temperature-to-humidity {:ranges temperature-to-humidity}
     :humidity-to-location {:ranges humidity-to-location}}))

(defn get-location-number
  [seed-number map-of-ranges]
  (let [get-in-next-range (fn [seed next-range]
                            (let [next-num (seed->soil seed next-range)]
                              next-num))
        soil-number (get-in-next-range seed-number (get-in map-of-ranges [:seed-to-soil :ranges]))
        fertilizer-number (get-in-next-range soil-number (get-in map-of-ranges [:soil-to-fertilizer :ranges]))
        water-number (get-in-next-range fertilizer-number (get-in map-of-ranges [:fertilizer-to-water :ranges]))
        light-number (get-in-next-range water-number (get-in map-of-ranges [:water-to-light :ranges]))
        temperature-number (get-in-next-range light-number (get-in map-of-ranges [:light-to-temperature :ranges]))
        humidity-number (get-in-next-range temperature-number (get-in map-of-ranges [:temperature-to-humidity :ranges]))
        location-number (get-in-next-range humidity-number (get-in map-of-ranges [:humidity-to-location :ranges]))]
    location-number))

(defn process-seed-range
  ([seed-range map-of-ranges]
   (let [start (:start seed-range)
         end (:end seed-range)
         first-location-number (get-location-number start map-of-ranges)]
     (process-seed-range map-of-ranges start end first-location-number)))
  ([map-of-ranges current-seed end result]
   (let [location-number (get-location-number current-seed map-of-ranges)
         new-result (min result location-number)]
     (println :seed current-seed :location location-number)
     (if (not (> current-seed end))
       (recur map-of-ranges (inc current-seed) end new-result)
       new-result))))

(defn brute-force
  "DOES NOT WORK FOR REAL INPUT"
  [input]
  (let [map-of-ranges (input->ranges2 input)
        min-in-ranges (map (fn [seed-range]
                             (process-seed-range seed-range map-of-ranges))
                           (:seeds map-of-ranges))]
    (apply min min-in-ranges)))

(brute-force test-input)
;; expect => 46

#_(solve2 input)
