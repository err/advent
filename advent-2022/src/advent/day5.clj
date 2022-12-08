(ns advent.day5
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))


(def input-filename "resources/day5-part1.txt")

(def test-input
  ["    [D]    "
   "[N] [C]"
   "[Z] [M] [P]"
   " 1   2   3 "
   ""
   "move 1 from 2 to 1"
   "move 3 from 1 to 3"
   "move 2 from 2 to 1"
   "move 1 from 1 to 2"])

(def move-regex
  #"move ([0-9]+) from ([0-9]+) to ([0-9]+)")

(defn parse-move
  [s]
  (let [[_s amount from to] (re-find move-regex s)]
    {:amount (parse-long amount)
     :from   (parse-long from)
     :to     (parse-long to)}))

(defn merge+
  [old new]
  (merge-with (fnil conj []) old new))

(defn parse-board
  [board-lines]
  (let [indices (-> (last board-lines)
                    (str/trim)
                    (str/split #"( )+")
                    (->> (into [] (comp (map parse-long)))))
        ;; number of chars per board column
        col-width 4]
    {:indices indices
     :stacks (->> (butlast board-lines)
                  (mapv (fn [s]
                          (->> (partition-all col-width s)
                               (into [] (comp
                                         (map-indexed (fn [col-idx chars]
                                                        [(inc col-idx) (str/trim (apply str chars))]))
                                         (filter (fn [[col s]]
                                                   (not (str/blank? s)))))))))
                  reverse
                  (transduce (comp (map (fn [row] (into {} row))))
                             (completing merge+)
                             (into (sorted-map) (map (fn [i] [i []])) indices)))})
  )

(defn parse-input
  [lines]
  (let [[board moves]
        (split-with (fn [s]
                      (or (str/blank? (str/trim s))
                          (not (= "move" (subs s 0 4)))))
                    lines)]
    {:board (parse-board (vec (butlast board)))
     :moves (mapv parse-move moves)}))

(parse-input test-input)

;; => {:board [1 2 3],
;;     :moves
;;     [{:amount 1, :from 2, :to 1}
;;      {:amount 3, :from 1, :to 3}
;;      {:amount 2, :from 2, :to 1}
;;      {:amount 1, :from 1, :to 2}]}
