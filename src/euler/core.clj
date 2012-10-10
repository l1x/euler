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
    ([] (numbers 1))
    ([n] (cons n (lazy-seq (numbers (inc n))))))
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
  ;lazy-seq of Fibonacci numbers
  (defn fibo 
    []
    (map first (iterate (fn [[a b]] [b(+ a b)]) [1N 1N])))
  (defn fact 
    [n] 
    (reduce * (range 1N (inc n))))
  (defn pandigital?
    [n]
    (=  (apply sorted-set (map #(Integer. (str %)) (str n)))
        (apply sorted-set (map #(Integer. %) (range 1 (inc (count (str n))))))))
  (defn pyth?
    [a b c]
    (and (< a b c) (== (+ (* a a) (* b b)) (* c c))))
  (defn pyth 
    "returns the Pythagorean triplets to n"
    [n] 
    (for [a (range 1 n)
          b (range (+ a 1) n)
          c (range (+ b 1) n)
          :when (and (> c b a) (= (+ (* a a) (* b b)) (* c c)))]
          [a b c]))
  (defn occurrences 
    "Counting the occurrences for each value in a collection
    consider the following:
    (assoc {1 0, 2 0, 3 0} 2 (inc ({1 0, 2 0, 3 0} 2 0)))"
    [coll] 
    (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} coll))
;destructing
; (map (fn [[k v]] [k (count v)]) (group-by #(rem % 3) (range 20)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ns euler41
  (:use [clojure.math.combinatorics :only [permutations]]))
  (defn euler41 
    []
    (reduce max (filter #(and (euler/prime? %) (euler/pandigital? %))
      (map #(Integer/parseInt (apply str %))
        (permutations [1 2 3 4 5 6 7])))))
(ns euler40)
  (defn euler40 
    []
    (reduce * 
      (map #(Character/getNumericValue %) 
        (map #(nth (apply concat 
          (map str (iterate inc 0))) %) 
            [1 10 100 1000 10000 100000 1000000]))))
(ns euler39)
  (defn pyth
    "returns the perimeter of the Pythagorean triplets to n as a coll"
    [n]
    (for [a (range 1 n)
          b (range (+ a 1) n)
          c (range (+ b 1) n)
          :when (and (> c b a) (= (+ (* a a) (* b b)) (* c c)))]
          (+ a b c)))
  (defn euler39
    "We need to get the perimeters of the Pythagorean triplets
    (12 30 24...)
    we need to filter that to be less than 1000
    in the returned collection we need the occurrences of each value
    the returned map has to be sorted by the values
    "
    []
    (nth (first (sort-by val > 
      (euler/occurrences 
        (filter #(< % 1000) (pyth 400)))))0))

(ns euler9)
  (defn pyth-sum 
    [n]
    (first (for [a (range 1 n)
                 b (range (+ a 1) n)
                 c (list (- n a b))
                 :when (and (> c b a) (= (+ (* a a) (* b b)) (* c c)))]
                 (* a b c))))
  (defn euler9 
    []
    (pyth-sum 1000))

(ns euler11)

  (defn smaller-than-2m [n] (< n 2000000N)) 
  (defn euler11 
    [] 
    (reduce + (filter euler/prime? (take-while smaller-than-2m euler/numbers))))

(ns euler12)
  ;using the (n*(n+1))/2 form
  (def triangle-nums 
    (map #(/ (* % (+ % 1)) 2) euler/numbers))
  ;http://mathschallenge.net/library/number/number_of_divisors
  (defn count-of-divisors 
    [n]
    (* 2 (count
      (filter #(zero? (rem n %)) (range 1 (Math/sqrt n))))))

  (defn euler12 
    [] 
    (first (filter #(> (count-of-divisors %) 500) triangle-nums)))


(ns euler14)
  (defn collatz 
    [n]
    (let [step (fn [n]
      (if (even? n)
        (/ n 2)
        (inc (* n 3))))]
      (take-while (partial < 1) (iterate step n))))
  (defn longest-collatz
    [ceil]
    (first 
      (reduce #(if (> (count %1) (count %2)) %1 %2)
        (map collatz (range ceil)))))
  (defn euler14 
    [] 
    (longest-collatz 1000000))

(ns euler16)
  (defn euler16 
    [] 
    (reduce + (euler/digits (euler/pow 2N 1000N))))

(ns euler20)
  (defn euler20 
    [] 
    (reduce + (euler/digits (euler/fact 100N))))

(ns euler21)
  (defn sum-of-divisors 
    [n]
    (reduce + (filter #(zero? (rem n %)) (range 1 n))))
  (defn amicable [a]
    (let [b (sum-of-divisors a)]
      (if (and (not (= a b)) (= a (sum-of-divisors b)))
        a
        0)))
  (defn euler21 
    []
    (reduce + (map amicable (range 1 10001))))

(ns euler22)

  (def s (slurp "names.txt"))
  (def sorted (sort (re-seq #"[\w]+" s)))
  (defn string2anum 
    [s]
    (reduce + (map #(- (int %) 64) s)))
  (defn euler22 
    []
    (reduce + 
      (for 
        [n (range 0 (count sorted))]
        (* (inc n) (string2anum (nth sorted n)) ) )))

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
  (defn num-of-digits [n]
    (count (str n)))
  (defn euler25 [] 
    (+ 1 (count (take-while #(< (num-of-digits %1) 1000) (euler/fibo)))))

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

  ;Wait what?
  ;
  ;(def matrix1 
  ;[[1 2 3] 
  ; [8 9 4] 
  ; [7 6 5]])
  ;
  ;Simple idiom:
  ;(seq matrix1)
  ;([1 2 3] [8 9 4] [7 6 5])
  ;
  ;Rotation is exploiting the nature of the language 
  ;that vector is applied to the first element of the 
  ;first and second array, than to the second element 
  ;of the first and second array, and so on
  ;user=> (apply map vector '[[8 9 4] [7 6 5]])
  ;([8 7] [9 6] [4 5])
  ;user=> '[[1 2 3][8 9 4][7 6 5]]
  ;
  ;
  ;Reversing the list of arrays gives:
  ;user=> (reverse (apply map vector '[[8 9 4] [7 6 5]]))
  ;([4 5] [9 6] [8 7])
  ;user=> (reverse (apply map vector '[[9 6] [8 7]]))
  ;([6 7] [9 8])
  ;user=> (reverse (apply map vector '[[9 8]]))
  ;([8] [9])
  ;

  (defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))
  (def matrix1 [[1 2 3][8 9 4][7 6 5]])
  (def matrix2 [[1 2 3 4][12 13 14 5][11 16 15 6][10 9 8 7]])
  (defn spiral-print 
    [matrix result]
      (let [[row & rows] (seq matrix)]
        (if (seq rows)
          (recur
            ;calls the function with the remaining "rotated" matrix
            (reverse (apply map vector rows))
            ;returns a new list with result + row
            (into result row)) ;for debugging replace with: (into (dbg result) (dbg row)))
          ;else
          (into result row))))
          ;(spiral-print matrix1 [])

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
  (time (println (str "euler41 : " (euler41/euler41))))
  ;(time (println (str "euler40 : " (euler40/euler40))))
  ;(time (println (str "euler39 : " (euler39/euler39))))
  ;(time (println (str "euler23 : " (euler23/euler23))))
  ;(time (println (str "euler24 : " (euler24/euler24))))
  ;(time (println (str "euler25 : " (euler25/euler25))))
  ;(time (println (str "euler25 : " (euler25/euler25))))
  ;(time (println (str "spiralp : " (spiral/spiral-print spiral/matrix2 []))))
  ;(time (println (quicksort/qsort (quicksort/random-nums 100000))))
  ;(time (println  (str "head2tail_java : " (head2tail/find-path head2tail/neighbor-words1 "head" "tail"))))
  ;(time (println  (str "head2tail_clj  : " (head2tail/find-path head2tail/neighbor-words3 "head" "tail"))))
  ;(time (println  (str "head2tail_java : " (head2tail/find-path head2tail/neighbor-words1 "hood" "pork"))))
  ;(time (println  (str "head2tail_clj  : " (head2tail/find-path head2tail/neighbor-words3 "hood" "pork"))))
  ;(time (println  (str "head2tail_java : " (head2tail/find-path head2tail/neighbor-words1 "bibs" "woom"))))
  ;(time (println  (str "head2tail_clj  : " (head2tail/find-path head2tail/neighbor-words3 "bibs" "woom"))))
  ;(time (println  (str "head2tail_java : " (head2tail/find-path head2tail/neighbor-words1 "arab" "wink"))))
  ;(time (println  (str "head2tail_clj  : " (head2tail/find-path head2tail/neighbor-words3 "arab" "wink"))))
  )

