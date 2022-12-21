(ns advent.day6
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def input-filename "resources/day6-part1.txt")

(def test-inputs
  "start-of-packet marker : distinct set of 4 characters
   find the index at which the first start-of-packet marker is complete"
  [["mjqjpqmgbljsphdztnvjfqwrcgsmlb" 7]
   ["bvwbjplbgvbhsrlpgdmjqwftvncz" 5]
   ["nppdvjthqldpwncqszvftbrmjlhg"  6]
   ["nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 10]
   ["zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"  11]])

(defn run-test
  ([f]
   (run-test test-inputs))
  ([f tests]
   (or (every? (fn [[input expect-out]]
                 (= expect-out (f input)))
               tests)
       (mapv (fn [[input expect-out]]
               {:input input
                :expect expect-out
                :output (f input)})
             tests))))

(def packet-marker-length 4)
(def message-marker-length 14)

(defn marker-positions
  "Returns a lazy-seq of indices in s where markers complete."
  ([s]
   (marker-positions packet-marker-length s))
  ([marker-length s]
   (->> s
        (partition-all marker-length 1)
        (keep-indexed (fn [start-index chars]
                        (when (apply distinct? chars)
                          (+ start-index marker-length)))))))

(comment
  (run-test (comp first marker-positions)))


(comment
  ;; PART ONE
  (first (marker-positions (slurp input-filename)))
  ;; 1833
  )


(def test-inputs-part2
  "start-of-message  marker : distinct set of 14 characters
   find the index at which the first start-of-message marker is complete"
  [["mjqjpqmgbljsphdztnvjfqwrcgsmlb" 19]
   ["bvwbjplbgvbhsrlpgdmjqwftvncz" 23]
   ["nppdvjthqldpwncqszvftbrmjlhg" 23]
   ["nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 29]
   ["zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"  26]])

(comment
  (run-test (comp first (partial marker-positions message-marker-length))
            test-inputs-part2))


(comment
  ;; PART TWO
  (first (marker-positions message-marker-length
                           (slurp input-filename)))
  ;; 3425
  )


(first (marker-positions message-marker-length
                         (slurp input-filename)))
