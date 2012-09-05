;Copyright (C) 666, /dev/null
;
;Permission is hereby granted, free of charge, to any person obtaining a copy of this software 
;and associated documentation files (the "Software"), to deal in the Software without restriction, 
;including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, 
;and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, 
;subject to the following conditions:
;
;The above copyright notice and this permission notice 
;shall be included in all copies or substantial portions of the Software.
;
;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
;EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
;OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
;IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, 
;DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, 
;TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION 
;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;
;And more importantly it turned out that lawyers has smaller penis than the normal sample.
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

(ns quicksort
  "From the book Joy of Clojure")

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

(ns head2tail
  "Solving the head to tail problem,
  you have to come up with a list of words which is the path from the 
  first word to the second, changing 1 letter a time.
  Original source -> https://gist.github.com/608728
  Blog post -> http://wp.me/p12FcK-3
  "
  (:use [clojure.string :only [lower-case split-lines replace-first]]))

  (def dictionary
    (into #{} 
      (map lower-case 
        (split-lines (slurp "/usr/share/dict/words")))))
  
  (def alphabet "abcdefghijklmnopqrstuvwxyz")

  (defn neighbor-words1 [^String word]
    "Return a lazy-seq with words which differ from 
    the input word by one letter. This solution heavily leverages Java"
    (filter dictionary
      (apply concat
        (map-indexed (fn [i c]
          (let [java-string (StringBuilder. word)]
            (for [abc alphabet :when (not= abc c)] 
              (str (doto java-string (.setCharAt i abc)))))) word))))

  (defn neighbor-words3 [^String word]
    ;Pure clojure implementation
    ;slower than the Java one
    (filter dictionary
      (distinct
        (for [w (for [abc alphabet i (range 0 (count word))] 
          (assoc (into [] word) i abc))] 
            (apply str w)))))

  (defn find-path [neighbors start end]
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start) preds {start nil}]
    (when-let [node (peek queue)]
      (let [nbrs (remove #(contains? preds %) (neighbors node))]
        (if (some #{end} nbrs)
          (reverse (cons end (take-while identity (iterate preds node))))
          (recur (into (pop queue) nbrs)
                 (reduce #(assoc %1 %2 node) preds nbrs)))))))
;main 
(ns euler.core
  (:gen-class))

(defn -main [& args]
  (set! *print-length* 10)
  ;(time (println (str "euler23 : " (euler23/euler23))))
  ;(time (println (str "euler24 : " (euler24/euler24))))
  ;(time (println (str "euler25 : " (euler25/euler25))))
  ;(time (println (str "euler25 : " (euler25/euler25))))
  ;(time (println (str "spiralp : " (spiral/spiral-print spiral/matrix2 []))))
  ;(time (println (quicksort/qsort (quicksort/random-nums 100000))))
  (time (println  (str "head2tail_java : " (head2tail/find-path head2tail/neighbor-words1 "head" "tail"))))
  (time (println  (str "head2tail_clj  : " (head2tail/find-path head2tail/neighbor-words3 "head" "tail"))))
  (time (println  (str "head2tail_java : " (head2tail/find-path head2tail/neighbor-words1 "hood" "pork"))))
  (time (println  (str "head2tail_clj  : " (head2tail/find-path head2tail/neighbor-words3 "hood" "pork"))))
  (time (println  (str "head2tail_java : " (head2tail/find-path head2tail/neighbor-words1 "bibs" "woom"))))
  (time (println  (str "head2tail_clj  : " (head2tail/find-path head2tail/neighbor-words3 "bibs" "woom"))))
  (time (println  (str "head2tail_java : " (head2tail/find-path head2tail/neighbor-words1 "arab" "wink"))))
  (time (println  (str "head2tail_clj  : " (head2tail/find-path head2tail/neighbor-words3 "arab" "wink"))))
  )

