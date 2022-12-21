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
                             (into (sorted-map) (map (fn [i] [i []])) indices)))}))

(defn parse-input
  [lines]
  (let [[board moves]
        (split-with (fn [s]
                      (or (str/blank? (str/trim s))
                          (not (= "move" (subs s 0 4)))))
                    lines)]
    {:board (parse-board (vec (butlast board)))
     :moves (mapv parse-move moves)}))

(comment (parse-input test-input))

;; => {:board
;;     {:indices [1 2 3],
;;      :stacks {1 ["[Z]" "[N]"], 2 ["[M]" "[C]" "[D]"], 3 ["[P]"]}},
;;     :moves
;;     [{:amount 1, :from 2, :to 1}
;;      {:amount 3, :from 1, :to 3}
;;      {:amount 2, :from 2, :to 1}
;;      {:amount 1, :from 1, :to 2}]}

(def default-version "9000")

(defn process-move
  ([stacks {:keys [amount from to] :as move}]
   (process-move default-version stacks move))
  ([version stacks {:keys [amount from to]}]
   (let [from-stack (get stacks from)
         to-stack   (get stacks to)
         [new-from blox] (splitv-at (- (count from-stack) amount) from-stack)]
     (-> stacks
         (assoc from new-from)
         (update to into (cond-> blox (= version default-version) reverse))))))

(comment
  (let [{:keys [board moves]} (parse-input test-input)]
    {:pre (:stacks board)
     :mov (first moves)
     :pos (process-move (:stacks board) (first moves))}))

;; {:pre {1 ["[Z]" "[N]"], 2 ["[M]" "[C]" "[D]"], 3 ["[P]"]},
;;  :mov {:amount 1, :from 2, :to 1},
;;  :pos {1 ["[Z]" "[N]" "[D]"], 2 ["[M]" "[C]"], 3 ["[P]"]}}


;; After the rearrangement procedure completes, what crate ends up on top of each stack?
(comment
  (with-open [rdr (clojure.java.io/reader input-filename)]
    (let [{:keys [board moves]} (parse-input (line-seq rdr))]
      (->> (reduce process-move (:stacks board) moves)
           (mapv (comp second peek val))
           (apply str)))))
;; "FWNSHLDNZ"


;; PART TWO: The CrateMover 9001 is notable for many new and exciting
;; features: air conditioning, leather seats, an extra cup holder, and
;; THE ABILITY TO PICK UP AND MOVE MULTIPLE CRATES AT ONCE.
(with-open [rdr (clojure.java.io/reader input-filename)]
  (let [{:keys [board moves]} (parse-input (line-seq rdr))]
    (->> (reduce (partial process-move "9001") (:stacks board) moves)
         (mapv (comp second peek val))
         (apply str))))
;; "RNRGDNFQG"
