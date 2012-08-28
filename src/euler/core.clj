(ns euler24
  (:use [clojure.math.combinatorics :only [permutations]]))
  (defn euler24 []
    (bigint (apply str (nth (permutations [0 1 2 3 4 5 6 7 8 9]) 999999))))

(ns euler25)
  (defn fibo []
    (map first (iterate (fn [[a b]] [b(+ a b)]) [1N 1N])))
  (defn num-of-digits [n]
    (count (str n)))
  (defn euler25 [] 
    (+ 1 (count (take-while #(< (num-of-digits %1) 1000) (fibo)))))



(ns spiral)
  (def matrix1 [[1 2 3][8 9 4][7 6 5]])
  (def matrix2 [[1 2 3 4][12 13 14 5][11 16 15 6][10 9 8 7]])
  (defn spiral-print [matrix]
    (let [ [row & rows] (seq matrix)]
      (doseq [el row]  (println (str " " el)))
      (when (seq rows) (recur (reverse (apply map vector rows))))))
      ;(when (seq rows) (->> rows (apply map vector) reverse recur))))

      ;(recur (reverse (apply map vector rows)))
      ;(->> foo (a b c) (d e f)) = (d e f (a b c foo))
      ;(->> [[8 9 4] [7 6 5]] (apply map vector))
      ;([8 7] [9 6] [4 5])
  ;(spiral-print matrix1)
  ;(spiral-print matrix2)

(ns euler26)
  ()

;main 
(ns euler.core
  (:gen-class))

(defn -main [& args]
  ;(time (println (str "euler24 : " (euler24/euler24))))
  ;(time (println (str "euler25 : " (euler25/euler25))))
  )

