(ns wordle.main
  (:require [clojure.string :as str]
            [clojure.math :as math]))

(defmacro fn->>
  [& forms]
  `(fn [x#] (->> x# ~@forms)))

(defn map-kv
  "Map f over the values of a map."
  [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))

(def read-words
  "Read in a word list"
  (fn->> slurp
         str/split-lines))

(defn normalise-counts
  [m]
  (let [sum (apply + (vals m))]
    (map-kv #(/ % sum) m)))

(defn round2
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (math/pow 10 precision)]
    (/ (math/round (* d factor)) factor)))
;;------------------------
;;Word list stats

(def positional-probs
  "Return the letter probability of occurring in each position for the words in the list.
   This is independent of neighbouring letters."
  (fn->> (map #(str/split % #""))
         (apply map list) ; transpose
         (map (partial group-by identity))
         (map (partial map-kv count))
         (map normalise-counts)))

(defn normalise-counts
  [h]
  (let [sum (apply + (vals h))]
    (map-kv #(* 1. (/ % sum)) h)))

(def occurrence-probs
  (fn->> (str/join "")
         frequencies
         normalise-counts))

;;------------------------
;; Word scoring

(defn entropy
  "Calculate the entropy of a set of probabilities"
  [h]
  (->> h
       vals
       (map #(* % (Math/log %)))
       (apply +)
       (* -1.)))

(defn score-position
  "Score a word on letter positional probabilities"
  [probs word]
  (let [letters (str/split word #"")]
    (->> letters
         (map #(get %1 %2) probs)
         (map (partial * 100))
         (apply *)
         int)))

(defn score-freqs
  "Score a word on letter freqencies"
  [freqs word]
  (->> word
       (select-keys freqs)
       vals
       (apply +)))

(defn score-entropy
  "Score the entropy of a word from the probabilities of each letter"
  [probs word]
  (->> word
       (select-keys probs)
       entropy
       (round2 3)))

;;------------------------
;; Patterns

(defn contains
  [letters]
  (as-> letters <>
      (str/split <> #"")
      (map #(str "(?=.*" % ")") <>)
      (concat <> ".*")
      (str/join "" <>)
      (re-pattern <>)))

(defn !contains
  [letters]
  (if (zero? (count letters))
    #""
    (re-pattern (str "[^" letters "]{5}"))))

(def re
  "Alias for re-pattern"
  re-pattern)


(defn filter-pattern
  "Return a transducer"
  [regex]
  (filter #(re-seq regex %)))

(defn filter-words
  "Filter the words through all the patterns.
   e.g. (filter-words w (contains 'bf') (!contains 'y') #'b....')"
  [words & patterns]
  (let [xf (apply comp (map filter-pattern patterns))]
    (transduce xf conj words)))

(defn rank-words
  "Rank words by letter frequencies"
  [words & patterns]
  (let [results (apply filter-words words patterns)
        probs (occurrence-probs results)
        scores (map #(score-entropy probs %) results)]
    (sort-by val > (zipmap results scores))))

;;------------------------
(def nyt (read-words "data/nyt-words5.txt"))

(defn -main
  "Do the most common call to rank a filtered list of words created by patterns"
  ;; e.g. (-main "atn" "b" "....n")
  [letters-in letters-out pattern]
  (let [words (read-words "data/nyt-words5.txt")
        results (rank-words words (contains letters-in) (!contains letters-out) (re pattern))
        len (count results)]
    (println (take (min len 20) results))))

;; The End
