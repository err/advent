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
