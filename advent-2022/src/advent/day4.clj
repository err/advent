(ns advent.day4
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def input-filename "resources/day4-part1.txt")

(def test-input
  "In how many assignment pairs does one range fully contain the other?"
  ["2-4,6-8"
   "2-3,4-5"
   "5-7,7-9"
   "2-8,3-7" ;; 2-8 contains 3-7
   "6-6,4-6" ;; 4-6 contains 6-6
   "2-6,4-8"])

(defn parse-range
  [s]
  (mapv parse-long (str/split s #"-")))

(defn parse-line
  [s]
  (let [[a b]       (str/split s #",")
        [a-lo a-hi] (parse-range a)
        [b-lo b-hi] (parse-range b)]
    {:a (into (sorted-set) (range a-lo (inc a-hi)))
     :b (into (sorted-set) (range b-lo (inc b-hi)))}))

(defn note-superset
  [{:keys [a b] :as pair}]
  (cond-> pair
    (set/superset? a b) (assoc :superset :a)
    (set/superset? b a) (assoc :superset :b)))

(comment
  (mapv (comp note-superset parse-line) test-input)
  [{:a #{2 3 4}, :b #{6 7 8}}
   {:a #{2 3}  , :b #{4 5}}
   {:a #{5 6 7}, :b #{7 8 9}}
   {:a #{2 3 4 5 6 7 8}, :b #{3 4 5 6 7}, :superset :a} ;;
   {:a             #{6}, :b #{4 5 6},     :superset :b} ;;
   {:a #{2 3 4 5 6}, :b #{4 5 6 7 8}}])

(comment
  (let [fname input-filename]
    (with-open [r (io/reader (io/file fname))]
      (count (into []
                   (comp (map (comp note-superset parse-line))
                         (filter :superset))
                   (line-seq r)))))
  ;; =>
  595)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; --- Part Two ---
;;
;; It seems like there is still quite a bit of duplicate work
;; planned. Instead, the Elves would like to know the number of pairs
;; that overlap at all.
;;
;; In the above example, the first two pairs (2-4,6-8 and 2-3,4-5)
;; don't overlap, while the remaining four pairs (5-7,7-9, 2-8,3-7,
;; 6-6,4-6, and 2-6,4-8) do overlap:
;;
;; 5-7,7-9 overlaps in a single section, 7.
;; 2-8,3-7 overlaps all of the sections 3 through 7.
;; 6-6,4-6 overlaps in a single section, 6.
;; 2-6,4-8 overlaps in sections 4, 5, and 6.
;;
;; So, in this example, the number of overlapping assignment pairs is 4.



(def test-input-2
  "In how many assignment pairs is there **any** overlap?"
  ;; Same as earlier test input -- I just wanted a succinct statement of the part2 challenge
  ["2-4,6-8"
   "2-3,4-5"
   "5-7,7-9"
   "2-8,3-7" ;; 2-8 contains 3-7
   "6-6,4-6" ;; 4-6 contains 6-6
   "2-6,4-8"])

(defn note-intersection
  [{:keys [a b] :as pair}]
  (let [overlap (set/intersection a b)]
    (cond-> pair
      (seq overlap)
      (assoc :intersection overlap))))


(comment
  (let [fname input-filename]
    (with-open [r (io/reader (io/file fname))]
      (count (into []
                   (comp (map (comp note-intersection parse-line))
                         (filter :intersection))
                   (line-seq r)))))
  ;; => 952
  )
