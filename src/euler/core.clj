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


(ns euler)

  (defn sum-of-divisors [n] (reduce + (filter #(zero? (rem n %)) (range 1 n))))
  (defn abundant? [n] (> (sum-of-divisors n) n))

  (def certainty 10)
  (defn prime?
    "Predicate, return true if the parameter is a prime"
    [n]
    (.isProbablePrime (BigInteger/valueOf n) certainty))
  (defn char-int
    "Magic 48"
    [c]
    (- (int c) 48))
  (defn digits
    "Returns a coll of the digits of the number"
    [n]
    (map char-int (str n)))
  (defn as-int
    "Assembles a number of the elements in a coll"
    [coll]
    (read-string (apply str coll)))
  (defn numbers
    "Lazy-seq of natural numbers from 1"
    []
    (iterate inc 1))
  (defn pow
    "Power of, x^y"
    [x y]
    (reduce * (replicate y x)))
  (defn rotations
    "Returns a lazy seq of all rotations of a seq"
    [x]
    (if (seq x)
      (map (fn [n _] (lazy-cat (drop n x) (take n x))) (iterate inc 0) x)
      (list nil)))

(ns euler23 
  ;euler23 - Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
  (:require clojure.set))
  (defn abundants 
    [n] 
    (filter euler/abundant? 
      (range 1 (+ n 1))))
  (defn sum-of-abundants 
    [n]
    (let [abnums (abundants n)]
      (distinct
        (for [x abnums
              y abnums
              :while (and (<= y x) (<= (+ x y) n))]
                (+ x y)))))
  (defn euler23 
    [] 
    (reduce + 
      (clojure.set/difference (into #{} (range 1 28124))
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

(ns euler27)
  (defn quadratic-formula 
    [a b n] 
    (+ (* n n) (* a n) b))
  ;how many consecutive primes can be found with a and b
  ;=>(consecutive-primes 1 41)
  ;40
  (defn consecutive-primes
    [a b]
    (count (take-while euler/prime? (map (partial quadratic-formula a b) (iterate inc 0)))))
  (defn max-primes
    [[a b c] [d e f]]
    (if (> c f) [a b c] [d e f]))
  (defn euler27 
    []
      (reduce max-primes
        (for [a (range -999 1001)
              b (range 1001)]
              [a b (consecutive-primes a b)])))

(ns euler28)
  (defn euler28 
    [n]
    (if (= n 1)
      1
      (apply + (cons (euler28 (- n 2))
        (take 4 (iterate #(- % (- n 1)) (* n n)))))))

(ns euler29)
  (defn euler29 
    [n]
    (count (distinct (for 
                       [a (range 2 n) 
                        b (range 2 n)] 
                        (euler/pow a b)))))

(ns euler30)
  (defn pow5
    [] 
    (reduce + 
      (map first 
        (filter  #(= (nth % 0) (nth % 1)) 
          (for 
            [a (range 2 999999) 
            :let 
              [b (reduce + (map #(int (euler/pow %1 5)) (euler/digits a)))]] 
            [a b])))))

(ns euler31)
  (def coins [200 100 50 20 10 5 2 1])
  (defn change
    [c v]
    (let [f (first c)]
      (if (= f 1) 1
        (reduce + (for [n (range 0 (inc (quot v f)))]
          (change (rest c) (- v (* n f))))))))

  (defn euler31 
    [] 
    (change coins 200))

(ns euler32)
  (defn pandigital? 
    [x y z]
    (= '(1 2 3 4 5 6 7 8 9)
        (sort (apply concat (map euler/digits [x y z])))))

  (defn euler32 
    [] 
    (reduce + (distinct (for 
      [a (range 1 5000)
       b (range a (/ 9999 a))
       :let [c (* a b)]
       :when (pandigital? a b c)]
       c))))

(ns euler34)
  ;145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
  ;Find the sum of all numbers which are equal to the sum of the factorial of their digits.
  ;limit the search to 1.000.000
  (defn factorial 
    [n]
    (reduce * (range 1 (inc n))))
  (defn sum-of-factorials 
    [n] 
    (reduce +  (map factorial (euler/digits n))))
  (defn curious? 
    [n]
    (if (and (not (= n 1)) (not (= n 2)) (= (sum-of-factorials n) n)) true false))
  (defn euler34 
    [] 
    (reduce + (filter curious? (take 1000000(euler/numbers)))))

(ns euler35)
  (defn circular?
    [n]
    (every? euler/prime? (map #(Integer/parseInt (apply str %)) (euler/rotations (euler/digits n)))))
  (defn euler35 
    [] 
    (count (filter circular? (filter euler/prime? (take 1000001 (euler/numbers))))))

(ns euler36)
  (defn palindromic? 
    [n]
    (= (seq (str n)) (reverse (str n))))
  (defn bin-palindromic? 
    [n]
    (palindromic? (Integer/toBinaryString n)))
  (defn euler36 
    []  
    ;skipping even numbers -> binary form is not palindromic for sure
    ;user=>  (map #(Integer/toBinaryString %) [2 4 6 8 10 12])
    ;("10" "100" "110" "1000" "1010" "1100")
    (reduce + 
      (filter #(and (palindromic? %) (bin-palindromic? %)) 
        (filter odd? 
          (take 1000001 (euler/numbers))))))

(ns euler37)
  (defn as-int 
    [coll]
    (read-string (apply str coll)))
  
  ;todo write recursive function which takes a 
  ;number and checks if it is truncatable from left to right and back

  (defn euler37 [] (take 11 (filter euler/prime? (filter #(> % 7) (euler/numbers)))))
  
(ns euler38)

  

(ns maps-as-we-like)

  ;(defn rem3 (group-by #(rem % 3) (range 100)))
  (defn reduce-by 
    [key-fn f init coll] 
    (reduce (fn [summaries x] 
              (let [k (key-fn x)] 
                (assoc summaries k (f (summaries k init) x))))
    {} coll))
  (def orders [
                {:product "Szoporoller"      :customer "Saxus" :qty 66 :total 200}
                {:product "PHP 24 ora alatt" :customer "Saxus" :qty 10 :total 120}
                {:product "miegyebet"        :customer "Bela"  :qty 12 :total 230}
                {:product "harci maszk"      :customer "Jozsi" :qty 23 :total 290}

                ])
  (defn order-totals 
    [orders] 
    (reduce-by :customer #(+ %1 (:total %2)) 0 orders))

(ns spiral)

  (defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))
  (def matrix1 [[1 2 3][8 9 4][7 6 5]])
  (def matrix2 [[1 2 3 4][12 13 14 5][11 16 15 6][10 9 8 7]])
  (defn spiral-print 
    [matrix result]
      (let [[row & rows] (seq matrix)]
        (if (seq rows)
          (recur
            ;calls the function with the remaining rotated matrix
            (reverse (apply map vector rows))
            ;returns a new list with result + row
            (into result row)) ;for debugging replace with: (into (dbg result) (dbg row)))
          ;else
          result)))

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
  (time (println (str "euler23 : " (euler23/euler23))))
  (time (println (str "euler24 : " (euler24/euler24))))
  (time (println (str "euler25 : " (euler25/euler25))))
  (time (println (str "euler25 : " (euler25/euler25))))
  (time (println (str "spiralp : " (spiral/spiral-print spiral/matrix2 []))))
  (time (println (quicksort/qsort (quicksort/random-nums 100000))))
  (time (println  (str "head2tail_java : " (head2tail/find-path head2tail/neighbor-words1 "head" "tail"))))
  (time (println  (str "head2tail_clj  : " (head2tail/find-path head2tail/neighbor-words3 "head" "tail"))))
  (time (println  (str "head2tail_java : " (head2tail/find-path head2tail/neighbor-words1 "hood" "pork"))))
  (time (println  (str "head2tail_clj  : " (head2tail/find-path head2tail/neighbor-words3 "hood" "pork"))))
  (time (println  (str "head2tail_java : " (head2tail/find-path head2tail/neighbor-words1 "bibs" "woom"))))
  (time (println  (str "head2tail_clj  : " (head2tail/find-path head2tail/neighbor-words3 "bibs" "woom"))))
  (time (println  (str "head2tail_java : " (head2tail/find-path head2tail/neighbor-words1 "arab" "wink"))))
  (time (println  (str "head2tail_clj  : " (head2tail/find-path head2tail/neighbor-words3 "arab" "wink"))))
  )

