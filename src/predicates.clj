(ns predicates)

(defn sum-f [f g x]
  (+ (f x) (g x)))

(sum-f inc dec 4) ;=> 8
(sum-f inc identity 5) ;=> 11
(sum-f identity - 10) ;=> 0

(defn less-than [n]
  (fn [k] (< k n)))

(defn equal-to [n]
  (fn [k] (== k n)))

(filter (less-than 3) [1 2 3 4 5])   ;=> (1 2)
(filter (less-than 4) [-2 12 3 4 0]) ;=> (-2 3 0)
(filter (equal-to 2) [2 1 3 2.0])    ;=> (2 2.0)
(filter (equal-to 2) [3 4 5 6])      ;=> ()

(defn set->predicate [a-set]
  (fn [x] (contains? a-set x)))

(filter (set->predicate #{1 2 3})     [0 2 4 6])       ;=> (2)
(filter (set->predicate #{1 2 3 nil}) [2 nil 4 nil 6]) ;=> (2 nil nil)

(defn pred-and [pred1 pred2]
  (fn [x] (and (pred1 x) (pred2 x))))

(filter (pred-and pos? even?) [1 2 -4 0 6 7 -3]) ;=> [2 6]
(filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) ;=> [1 7]
(filter (pred-and (complement nil?) empty?) [[] '() nil {} #{}])
;=> [[] '() {} #{}]

(defn pred-or [pred1 pred2]
  (fn [x] (or (pred1 x) (pred2 x))))

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  (every? whitespace? string))

(blank? " \t\n\t ") ;=> true
(blank? "  \t a")   ;=> false
(blank? "")         ;=> true

(defn has-award? [book award]
  (let [given-awards (:awards book)]
       (contains? given-awards award)))

(defn HAS-ALL-THE-AWARDS? [book awards]
  (let[has-the-awards (filter (fn [x] (has-award? book x)) awards)]
(= (set has-the-awards) awards)))

(defn my-some [pred a-seq]
  (let [result (map pred a-seq)
       filtered-values (filter (fn [x] (not(= x false) )) result)] 
(if 
  (first filtered-values) 
  (first filtered-values) 
   false)))

(my-some even? [1 3 5 7])       ;=> falsey
(my-some even? [1 3 5 7 8])     ;=> true
(my-some neg? [1 3 5 0 7 8])    ;=> falsey
(my-some neg? [1 3 5 0 7 -1 8]) ;=> true
(my-some neg? [])               ;=> falsey
(my-some first [[false] [1]])   ;=> 1
(my-some first [[false] []])    ;=> falsey
(my-some nil? [1 2])            ;=> falsey
(my-some nil? [1 nil 2])        ;=> true

(defn my-every? [pred a-seq]
  (let [result (map pred a-seq)
       filtered-values (filter (fn [x] (= x false) ) result)] 
(= (count filtered-values) 0)))

(my-every? pos? [1 2 3 4])   ;=> true
(my-every? pos? [1 2 3 4 0]) ;=> false
(my-every? even? [2 4 6])    ;=> true
(my-every? even? [])         ;=> true

(defn prim? [n]
  (or (and (not (== (mod n 2) 0)) (not (== (mod n 3) 0)))
(or (== 2 n) (== 3 n))))

(defn prime? [n]
  (let [pred (fn [x] (and (== (mod x 1) 0) (== (mod x x) 0)))]
    (not (some pred (range 2 n)))))

(prime? 6)
(prime? 4) ;=> false
(prime? 7) ;=> true
(prime? 10)
(range 2 10) ;=> false
;^^
