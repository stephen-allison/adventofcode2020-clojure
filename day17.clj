(ns aoc.day17 (:gen-class))
(require '[aoc.loader :as loader])
(require '[clojure.set :as set])

(defn scan-map 
  [state character]
  (let [[x y cells] state]
    (cond (= \newline character) [0 (inc y) cells]
          (= \# character) [(inc x) y (conj cells [x y])]
          :else [(inc x) y cells])))

(defn load-initial-active-cells []
  (last (reduce scan-map [0 0 []] (loader/load "input.txt"))))

(defn add-dimensions [xy-coords dims]
  (map #(apply conj %1 (repeat (- dims 2) 0)) xy-coords))

(defn initial-active-cells [dims]
  (-> (load-initial-active-cells)
      (add-dimensions dims)
      (set)))

(defn add-coords [a b]
  (vec (map + a b)))

(defn offsets [dims]
  (let [diffs [-1 0 1]
        zero (vec (repeat dims 0))]
    (cond (= dims 3) (for [x diffs y diffs z diffs :when (not= [x y z] zero)] [x y z])
          (= dims 4) (for [x diffs y diffs z diffs w diffs :when (not= [x y z w] zero)] [x y z w]))))

(defn neighbours [cell offsets]
  (reduce #(conj %1 (add-coords cell %2)) [] offsets))

(defn all-neighbours [cells offsets]
  (apply concat (map #(neighbours %1 offsets) cells)))

(defn inc-count [counts neighbour]
  (assoc counts neighbour (inc (get counts neighbour 0))))

(defn counter [coll]
  (reduce inc-count {} coll))

(defn run-turn [active offsets]
  (let [active-neighbours (all-neighbours active offsets)
        counts (counter (all-neighbours active offsets))
        deactivating (set (filter #(not (contains? #{2 3} (get counts %1 0))) active))
        activating (set (filter #(and (= 3 (get counts %1)) (not (contains? active %1))) active-neighbours))]
    (set/union (set/difference active deactivating) activating)
))

    
(defn run [dims turns]
  (let [offsets (offsets dims)
        initial-active (initial-active-cells dims)]
    (loop [active initial-active
           n 0]
      (if (= n turns) active
          (recur (run-turn active offsets) (inc n))))))

(defn solve []
  (do
    (println (count (run 3 6)))
    (println (count (run 4 6)))))
