(ns aoc.day15
  (:gen-class))
(require '[clojure.string :as str])
(require '[clojure.set])

(defn initial-index [input]
  (zipmap input (range 1 (count input))))

(defn game-turn
  [[last-spoken n index]]
  (let [prev (get index last-spoken)
        next (if (nil? prev) 0 (- n prev))]
    [next (inc n) (assoc index last-spoken n)]))

(defn game-turns
  [input]
  (iterate game-turn [(last input) (count input) (initial-index input)]))

(defn play-day15
  [input turns]
  (first (first (drop (- turns (count input)) (game-turns input)))))

(defn -main
  [& args]
  (do
    (println "playing game")
    (println (play-day15 [3 2 1] 2020)) ;expect 438
    (println (play-day15 [1 12 0 20 8 16] 30000000))  ;expect 47205
))
