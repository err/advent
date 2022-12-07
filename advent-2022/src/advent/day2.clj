(ns advent.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))
;; Appreciative of your help yesterday, one Elf gives you an encrypted
;; strategy guide (your puzzle input) that they say will be sure to
;; help you win. "The first column is what your opponent is going to
;; play: A for Rock, B for Paper, and C for Scissors. The second
;; column--" Suddenly, the Elf is called away to help with someone's
;; tent.

;; The second column, you reason, must be what you should play in
;; response: X for Rock, Y for Paper, and Z for Scissors. Winning
;; every time would be suspicious, so the responses must have been
;; carefully chosen.

;; The winner of the whole tournament is the player with the highest
;; score. Your total score is the sum of your scores for each
;; round. The score for a single round is the score for the shape you
;; selected (1 for Rock, 2 for Paper, and 3 for Scissors) plus the
;; score for the outcome of the round (0 if you lost, 3 if the round
;; was a draw, and 6 if you won).

;; Since you can't be sure if the Elf is trying to help you or trick
;; you, you should calculate the score you would get if you were to
;; follow the strategy guide.

;; For example, suppose you were given the following strategy guide:

;; A Y
;; B X
;; C Z
;; This strategy guide predicts and recommends the following:

;; In the first round, your opponent will choose Rock (A), and you
;; should choose Paper (Y). This ends in a win for you with a score of
;; 8 (2 because you chose Paper + 6 because you won).
;;
;; In the second round, your opponent will choose Paper (B), and you
;; should choose Rock (X). This ends in a loss for you with a score of
;; 1 (1 + 0).
;;
;; The third round is a draw with both players choosing Scissors,
;; giving you a score of 3 + 3 = 6.  In this example, if you were to
;; follow the strategy guide, you would get a total score of 15 (8 + 1
;; + 6).
;;
;; What would your total score be if everything goes exactly according
;; to your strategy guide?


(def input-file "resources/day2-part1.txt")

(def shape-scores
  {:rock     1
   :paper    2
   :scissors 3})

(def outcome-scores
  {:lose 0
   :draw 3
   :win  6})

(def shape-cipher
  {:theirs {"A" :rock
            "B" :paper
            "C" :scissors}
   :yours  {"X" :rock
            "Y" :paper
            "Z" :scissors}})

(defn round-outcome
  [their-shape your-shape]
  (cond (= their-shape your-shape) :draw
        (= their-shape :rock)      (if (= your-shape :paper) :win :lose)
        (= their-shape :paper)     (if (= your-shape :scissors) :win :lose)
        (= their-shape :scissors)  (if (= your-shape :rock) :win :lose)
        :else                      (throw (ex-info "WTF" {:their-shape their-shape
                                                          :your-shape  your-shape}))))
(defn round-score
  [{:keys [shapes outcomes] :as _scores} {:move/keys [theirs yours] :as _moves}]
  (let [their-shape (get-in shape-cipher [:theirs theirs])
        your-shape  (get-in shape-cipher [:yours yours])
        outcome     (round-outcome their-shape your-shape)
        outcome-score (outcomes outcome)
        shape-score   (shapes your-shape)]
    (+ shape-score outcome-score)))

(defn parse-line
  [s]
  (when s
    (let [[opponent-move player-move] (str/split s #" ")]
      {:move/theirs opponent-move
       :move/yours  player-move})))

(defn parse-file
  [{:keys [round-score-fn parse-line-fn]} fname]
  (with-open [r (io/reader (io/file fname))]
    (->> (line-seq r)
         (into [] (comp (map parse-line-fn)
                        (map (fn [moves]
                               (assoc moves :round-score (round-score-fn {:shapes shape-scores :outcomes outcome-scores} moves)))))))))

(comment
  ;; TESTING
  (->> "resources/day2-test0.txt"
       (parse-file {:parse-line-fn parse-line
                    :round-score-fn round-score})
       (transduce (map :round-score) +))
  ;;=>
  15)

(comment
  ;; REAL THING
  (->> (parse-file input-file)
       (transduce (map :round-score) +))
  ;;=>
  13268 ;; CORRECT
  )


;; --- Part Two ---
;;
;; The Elf finishes helping with the tent and sneaks back over to
;; you. "Anyway, the second column says how the round needs to end: X
;; means you need to lose, Y means you need to end the round in a
;; draw, and Z means you need to win. Good luck!"

;; The total score is still calculated in the same way, but now you
;; need to figure out what shape to choose so the round ends as
;; indicated. The example above now goes like this:

;; In the first round, your opponent will choose Rock (A), and you need the round to end in a draw (Y), so you also choose Rock. This gives you a score of 1 + 3 = 4.
;; In the second round, your opponent will choose Paper (B), and you choose Rock so you lose (X) with a score of 1 + 0 = 1.
;; In the third round, you will defeat your opponent's Scissors with Rock for a score of 1 + 6 = 7.
;; Now that you're correctly decrypting the ultra top secret strategy guide, you would get a total score of 12.

;; Following the Elf's instructions for the second column, what would your total score be if everything goes exactly according to your strategy guide?


(def cipher-2
  {:theirs {"A" :rock
            "B" :paper
            "C" :scissors}
   :yours  {"X" :lose
            "Y" :draw
            "Z" :win}})

(defn player-shape
  [their-shape outcome]
  (cond (= outcome :draw)     their-shape
        (= their-shape :rock)      (if (= outcome :win) :paper :scissors)
        (= their-shape :paper)     (if (= outcome :win) :scissors :rock)
        (= their-shape :scissors)  (if (= outcome :win) :rock :paper)
        :else                      (throw (ex-info "WTF" {:their-shape their-shape :outcome outcome}))))

(defn round-score-2
  [{:keys [shapes outcomes] :as _scores} {:move/keys [theirs] :round/keys [outcome] :as _round-info}]
  (let [their-shape (get-in shape-cipher [:theirs theirs])
        your-outcome (get-in cipher-2 [:yours outcome])
        your-shape  (player-shape their-shape your-outcome)
        outcome-score (outcomes your-outcome)
        shape-score   (shapes your-shape)]
    (+ shape-score outcome-score)))

(defn parse-line-2
  [s]
  (when s
    (let [[opponent-move player-outcome] (str/split s #" ")]
      {:move/theirs opponent-move
       :round/outcome player-outcome})))

(comment
  (->> "resources/day2-test0.txt"
       (parse-file {:parse-line-fn parse-line-2
                    :round-score-fn round-score-2})
       (transduce (map :round-score) +))

  ;;=>
  12)

(comment
  (->> input-file
       (parse-file {:parse-line-fn parse-line-2
                    :round-score-fn round-score-2})
       (transduce (map :round-score) +))
  ;;=>
  15508)
