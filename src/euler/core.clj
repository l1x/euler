;Copyright (C) 666, /dev/null
;
;Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
;
;The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;
;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;
;
;
(ns euler23 
  ;euler23 - Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
  (:require clojure.set))

  (defn sum-of-divisors [n] (reduce + (filter #(zero? (rem n %)) (range 1 n))))
  (defn abundant? [n] (> (sum-of-divisors n) n))
  (defn abundants [n] (filter abundant? (range 1 (+ n 1))))
  (defn sum-of-abundants [n]
    (let [abnums (abundants n)]
      (distinct
        (for [x abnums
              y abnums
              :while (and (<= y x) (<= (+ x y) n))]
                (+ x y)))))
  (defn euler23 [] (reduce + (clojure.set/difference (into #{} (range 1 28124))
                                         (into #{} (sum-of-abundants 28123)))))
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

  (defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))
  (def matrix1 [[1 2 3][8 9 4][7 6 5]])
  (def matrix2 [[1 2 3 4][12 13 14 5][11 16 15 6][10 9 8 7]])
  (defn spiral-print [matrix result]
    (let [[row & rows] (seq matrix)]
      (if (seq rows)
        (recur
          ;calls the function with the remaining rotated matrix
          (reverse (apply map vector rows))
          ;returns a new list with result + row
          (into result row)) ;for debugging replace with: (into (dbg result) (dbg row)))
        ;else
        result)))
      ;(when (seq rows) (->> rows (apply map vector) reverse recur))))
      ;(recur (reverse (apply map vector rows)))
      ;(->> foo (a b c) (d e f)) = (d e f (a b c foo))
      ;(->> [[8 9 4] [7 6 5]] (apply map vector))
      ;([8 7] [9 6] [4 5])
  ;(spiral-print matrix1 [])
  ;(spiral-print matrix2 [])

(ns quicksort)

(defn sort-parts [work] 
  (lazy-seq 
    (loop [[part & parts] work]
      (if-let [[pivot & xs] (seq part)]
        (let [smaller? #(< % pivot)]
          (recur (list*
                   (filter smaller? xs)
                   pivot
                   (remove smaller? xs)
                   parts)))
        (when-let [[x & parts] parts]
          (cons x (sort-parts parts)))))))
(defn qsort [xs]
  (sort-parts (list xs)))
(defn random-nums [n] (take n (repeatedly #(rand-int n))))

;main 
(ns euler.core
  (:gen-class))

(defn -main [& args]
  (set! *print-length* 10)
  (time (println (quicksort/qsort (random-nums 100000))))
  (time (println (str "euler23 : " (euler23/euler23))))
  ;(time (println (str "euler24 : " (euler24/euler24))))
  ;(time (println (str "euler25 : " (euler25/euler25))))
  ;(time (println (str "euler25 : " (euler25/euler25))))
  (time (println (str "spiralp : " (spiral/spiral-print spiral/matrix2 []))))
  )

