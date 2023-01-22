(ns wordle.main
  (:require [clojure.string :as str]))

(defn map-kv
  "Map f over the values of a map."
  [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))

(def read-words
  "Read in a word list"
  #(->> %
        slurp
        (str/split-lines)))

(defn normalise-counts
  [m]
  (let [sum (apply + (vals m))]
    (map-kv #(/ % sum) m)))

;;------------------------
;;Word list stats

(defn positional-probs
  "Return the letter probability of occurring in each position for the words in the list.
   This is independent of neighbouring letters."
  [words]
  (->> words
       (map #(str/split % #""))
       (apply map list) ; transpose
       (map (partial group-by identity))
       (map (partial map-kv count))
       (map normalise-counts)))

(defn occurrence-probs
  [words]
  (frequencies (str/join "" words)))

;;------------------------
;; Word scoring

(def count-letters
  #(->> %
        (group-by identity)
        (map-kv count)))

(defn normalise-counts
  [h]
  (let [sum (apply + (vals h))]
    (map-kv #(* 1. (/ % sum)) h)))

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

(defn score-word
  "Score a word based on word list stats"
  [freqs probs word]
  (+ (score-freqs freqs word)
        (score-position probs word)))

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

(def !contains
  #(re-pattern (str "[^" % "]{5}")))

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
  (let [freqs (occurrence-probs words)
        results (apply filter-words words patterns)
        scores (map #(score-freqs freqs %) results)]
    (sort-by val > (zipmap results scores))))

;;------------------------
(defn -main [letters-in letters-out pattern]
  (let [words (read-words "data/nyt-words5.txt")
        results (rank-words words (contains letters-in) (!contains letters-out) (re pattern))
        len (count results)]
    (println (take (min len 20) results))))

;; The End
