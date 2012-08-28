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

(ns euler.core
  (:gen-class))

(defn -main [& args]
  (time (println (str "euler24 : " (euler24/euler24))))
  (time (println (str "euler25 : " (euler25/euler25))))
  )

