(ns aoc.day15
  (:gen-class))

; lazy evaluation using iterate
; slower than anticipated
; performance problem due to lazy sequences doing a lot of allocations
; and placing more load on GC.

(defn initial-index [input]
  (zipmap input (range 1 (count input))))

(defn game-turn
  [[last-spoken n index]]
  (let [prev (get index last-spoken)
        next (if (nil? prev) 0 (- n prev))]
    [next (inc n) (assoc! index last-spoken n)]))

(defn game-turns
  [input]
  (iterate game-turn [(last input) (count input) (transient (initial-index input))]))

(defn day15-iterative
  [input turns]
  (first (first (drop (- turns (count input)) (game-turns input)))))

; simpler and quicker recursive approach

(defn day-15
  [input turns]
  (loop [n (count input)
         index (transient (initial-index input))
         last-spoken (last input)]
    (if (< n turns)
      (let [prev (get index last-spoken)
            next (if (nil? prev) 0 (- n prev))]
        (recur (inc n) (assoc! index last-spoken n) next))
      last-spoken)))

(defn day15-recursive [n]
  (day-15 [1 12 0 20 8 16] n))


(defn run
  []
  (do (println "Day 15")
      (println (str "Part 1:" (day15-recursive 2020)))
      (println (str "Part 2:" (day15-recursive 30000000)))))

(defn -main
  [& args]
  (do
    (println "playing game")
    (run)))
