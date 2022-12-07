(ns advent.day1
  "

  # PART ONE

  The Elves take turns writing down the number of Calories [...] that they've brought with them, one item per line.
  Each Elf separates their own inventory from the previous Elf's inventory (if any) by a blank line.

  For example, suppose the Elves finish writing their items' Calories
  and end up with the following list:

  1000
  2000
  3000

  4000

  5000
  6000

  7000
  8000
  9000

  10000

  This list represents the Calories of the food carried by five Elves:

  The first  Elf is carrying food with 1000, 2000, and 3000 Calories, a total of 6000 Calories.
  The second Elf is carrying one food item with 4000 Calories.
  The third  Elf is carrying food with 5000 and 6000 Calories, a total of 11000 Calories.
  The fourth Elf is carrying food with 7000, 8000, and 9000 Calories, a total of 24000 Calories.
  The fifth  Elf is carrying one food item with 10000 Calories.

  In case the Elves get hungry and need extra snacks, they need to know which Elf to ask:
  they'd like to know how many Calories are being carried by the Elf carrying the most Calories.
  In the example above, this is 24000 (carried by the fourth Elf).

  Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?"
  (:require [clojure.java.io :as io]))


(def input-file {::part-one "resources/day1-part1.txt"})

(defn break-line? [s] (= s ""))
(defn parse-long+
  [s]
  (if (break-line? s)
    s
    (try (parse-long s)
         (catch Exception _ nil))))


(defn read-file
  [fname]
  (with-open [r (io/reader (io/file fname))]
    (->> (line-seq r)
         #_(take 20)
         (into [] (comp (map parse-long+)
                        (partition-by break-line?)
                        (remove (comp break-line? first))
                        (map (fn [foods]
                               {:num-snacks (count foods)
                                :calories (reduce + foods)}))))
         (sort-by (juxt (comp - :calories)
                        (comp - :num-snacks)))
         vec)))


(comment
  (read-file (::part-one input-file))

  ;; =>
  [{:num-snacks 9, :calories 71506}
   {:num-snacks 1, :calories 69368} ,,,]


  (apply max-key :calories (read-file (::part-one input-file)))
  ;;=>
  {:num-snacks 9, :calories 71506}

  ;; CORRECT
  ;; Your puzzle answer was 71506.
  )


;; --- Part Two ---
;;
;; By the time you calculate the answer to the Elves' question,
;; they've already realized that the Elf carrying the most Calories of
;; food might eventually run out of snacks.

;; To avoid this unacceptable situation, the Elves would instead like
;; to know the total Calories carried by the top three Elves carrying
;; the most Calories. That way, even if one of those Elves runs out of
;; snacks, they still have two backups.

;; In the example above, the top three Elves are the fourth Elf (with
;; 24000 Calories), then the third Elf (with 11000 Calories), then the
;; fifth Elf (with 10000 Calories). The sum of the Calories carried by
;; these three elves is 45000.

;; Find the top three Elves carrying the most Calories.
;; How many Calories are those Elves carrying in total?



(comment
  (read-file (::part-one input-file))

  ;; =>
  [{:num-snacks 9, :calories 71506}
   {:num-snacks 1, :calories 69368} ,,,]


  (apply max-key :calories (read-file (::part-one input-file)))
  ;;=>
  {:num-snacks 9, :calories 71506}

  ;; CORRECT
  ;; Your puzzle answer was 71506.
  )

(defn max-calories
  [n filename]
  (->> (read-file filename)
       (transduce (comp (take n)
                        (map :calories))
                  +)))


(max-calories 3 (::part-one input-file))
;;=>
209603

;; That's the right answer! You are one gold star closer to collecting
;; enough star fruit.
