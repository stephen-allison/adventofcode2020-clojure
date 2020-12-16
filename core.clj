(ns aoc.core
  (:gen-class))
(require '[clojure.string :as str])
(require '[clojure.set])
(require '[aoc.loader :as loader])
(require '[aoc.day1 :as day1])
(require '[aoc.day15 :as day15])


; Joy Of Clojure p94 -- magic is just two bodies for different arities
; callers from outside call with 2 args and hit the arity-2 body
; this then calls into neighbours again, thsi time with 3 args and hits 
; the 3-arity body that does the work
(defn neighbours 
  ([size yx] (neighbours [[-1 0] [1 0] [0 -1] [0 1]] size yx))
  ([deltas size yx] (filter 
                     (fn [new-yx] 
                       (every? #(< -1 % size) new-yx)) 
                     (map #(vec (map + yx %)) deltas ))))

; JoC p121 - building up a tree structure
; cond is a bit like switch, it executes the first expression with a true condition
(defn xconj [t v]
  (cond
    (nil? t) {:val v, :L nil, :R nil}
    (< v (:val t)) {:val (:val t), :L (xconj (:L t) v), :R (:R t)}
    (>= v (:val t)) {:val (:val t), :L (:L t), :R (xconj (:R t) v)}
))

;JoC p122 - recursivly convert tree into a seq
; when evaluates body if condition is true, body has implicit 'do' 
; so can sequence several operations
(defn xseq [t]
  (when t
    (concat (xseq (:L t)) [(:val t)] (xseq (:R t)))))

(defn -main
  [& args]
  (do
    (loader/load)
    (day1/run)
    (day15/run)))




