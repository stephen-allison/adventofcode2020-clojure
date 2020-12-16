(ns aoc.day1
  (:gen-class))
(require '[clojure.string :as str])

(defn day1-data []
  (str/split-lines (slurp "/Users/stephen/code/adventofcode2020/day1/input.txt")))

(defn day1-numbers []
  (map read-string (day1-data)))

(defn two-sum-to [nums target]
  (let [num-set (set nums)
         n (first num-set)
        ns (set (rest num-set))]
    (if (nil? n)
      nil
      (if (contains? ns (- target n))
        [n (- target n)] 
        (recur ns target))
      )
))

(defn two-sum-to-2 [nums target]
  (let [ns (set nums)
        comp (fn [x] (- target x))]
    (first (for [n ns :when (contains? ns (comp n))] [n (comp n)]))))

(defn three-sum-to-2 [nums target]
  (let [ns (set nums)
        comp #(- target %)]
    (first (for [n ns :when (two-sum-to-2 nums (comp n))] (concat [n] (two-sum-to-2 nums (comp n)))))))

(defn run []
  (do
    (println "Day 1")
    (println "Part 1: " (apply * (two-sum-to-2 (day1-numbers) 2020)))
    (println "Part 2: " (apply * (three-sum-to-2 (day1-numbers) 2020)))
))

