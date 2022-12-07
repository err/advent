(ns advent.day3
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For example, suppose you have the;;
;; following list of contents from  ;;
;; six rucksacks:                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vJrwpWtwJgWrhcsFMMfFFhFp         ;;
;; jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL ;;
;; PmmdzqPrVvPwwTWBwg               ;;
;; wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn   ;;
;; ttgJtRGJQctTZtZT                 ;;
;; CrZsJsPPZsGzwwsLwLmpwMDw         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  ;;
;; The first rucksack contains the items vJrwpWtwJgWrhcsFMMfFFhFp,
;; which means its first compartment contains the items vJrwpWtwJgWr,
;; while the second compartment contains the items hcsFMMfFFhFp. The
;; only item type that appears in both compartments is lowercase p.
;;
;; The second rucksack's compartments contain jqHRNqRjqzjGDLGL and
;; rsFMfFZSrLrFZsSL. The only item type that appears in both
;; compartments is uppercase L.
;;
;; The third rucksack's compartments contain PmmdzqPrV and vPwwTWBwg; the only common item type is uppercase P.
;; The fourth rucksack's compartments only share item type v.
;; The fifth rucksack's compartments only share item type t.
;; The sixth rucksack's compartments only share item type s.
;;
;; To help prioritize item rearrangement, every item type can be converted to a priority:
;;
;; Lowercase item types a through z have priorities 1 through 26.
;; Uppercase item types A through Z have priorities 27 through 52.
;;
;; In the above example, the priority of the item type that appears in
;; both compartments of each rucksack is 16 (p), 38 (L), 42 (P),
;; 22 (v), 20 (t), and 19 (s); the sum of these is 157.
;;
;; Find the item type that appears in both compartments of each
;; rucksack. What is the sum of the priorities of those item types?
(def test-input
  ["vJrwpWtwJgWrhcsFMMfFFhFp"
   "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
   "PmmdzqPrVvPwwTWBwg"
   "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
   "ttgJtRGJQctTZtZT"
   "CrZsJsPPZsGzwwsLwLmpwMDw"])

(defn common-item
  [s]
  (apply set/intersection (map set (split-at (/ (count s ) 2) s))))

(def priority
  (into {}
        (map-indexed (fn [i x] [(char x) (inc i)]))
        (concat (range (int \a) (inc (int \z)))
                (range (int \A) (inc (int \Z))))))

(defn sum-priority
  [input]
  (transduce (map (comp priority first common-item))
             +
             input))

(comment

  (let [fname "resources/day3-part1.txt"]
    (with-open [r (io/reader (io/file fname))]
      (sum-priority (line-seq r))))
  ;; => 7795
  )

;;  PART 2
;; The only way to tell which item type is the right one is
;; by finding the one item type that is common between all three
;; Elves in each group.
;;
;; Every set of three lines in your list corresponds to a single
;; group, but each group can have a different badge item type. So, in
;; the above example, the first group's rucksacks are the first three
;; lines:
;;
;; vJrwpWtwJgWrhcsFMMfFFhFp
;; jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
;; PmmdzqPrVvPwwTWBwg
;;
;; In the first group, the only item type that appears in all three
;; rucksacks is lowercase r; this must be their badges. In the second
;; group, their badge item type must be Z.



(def test-input-2
  ["vJrwpWtwJgWrhcsFMMfFFhFp"
   "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
   "PmmdzqPrVvPwwTWBwg"])

(defn common-badge
  [coll]
  (apply set/intersection (map set coll)))

(defn sum-badge-priority
  [input]
  (transduce (comp (partition-all 3)
                   (map (comp priority first common-badge)))
             +
             input))

(comment
  ;; test inputs
  (sum-badge-priority test-input-2)
  ;; => 18 (r)
  (sum-badge-priority
   ["wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
    "ttgJtRGJQctTZtZT"
    "CrZsJsPPZsGzwwsLwLmpwMDw"])
  ;; => 52 (Z)

  (let [fname "resources/day3-part1.txt"]
    (with-open [r (io/reader (io/file fname))]
      (sum-badge-priority (line-seq r))))
  ;; => 2703

  )
