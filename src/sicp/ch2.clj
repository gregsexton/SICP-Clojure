(ns sicp.ch2)

;;; Exercise 2.1

(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (rem a b))))

(defn make-rat [n d]
  (let [g (Math/abs (gcd n d))
        m (if (neg? d) (* -1 g) g)]
    [(/ n m) (/ d m)]))

;;; Exercise 2.2

(defn make-segment [start end]
  [start end])

(defn start-segment [segment]
  (first segment))

(defn end-segment [segment]
  (second segment))

(defn make-point [x y]
  [x y])

(defn x-point [point]
  (first point))

(defn y-point [point]
  (second point))

(defn midpoint-segment [segment]
  (make-point
   (/ (+ (x-point (start-segment segment))
         (x-point (end-segment segment)))
      2)
   (/ (+ (y-point (start-segment segment))
         (y-point (end-segment segment)))
      2)))

(defn print-point [p]
  (println (format "(%d, %d)"
                   (x-point p)
                   (y-point p))))

;;; Exercise 2.4

(defn cons [x y]
  (fn [m] (m x y)))

(defn car [lst]
  (lst (fn [x y] x)))

(defn cdr [lst]
  (lst (fn [x y] y)))

;;; Exercise 2.5

(defn cons [x y]
  (* (Math/pow 2 x)
     (Math/pow 3 y)))

(defn divides-count [n d]
  (loop [n (int n)
         cnt 0]
    (if (not= (mod n d) 0) cnt
        (recur (/ n d) (inc cnt)))))

(defn car [pair]
  (divides-count pair 2))

(defn cdr [pair]
  (divides-count pair 3))

;;; Exercise 2.6

(def zero
  (fn [f] (fn [x] x)))
(defn add-1 [n]
  (fn [f] (fn [x] (f ((n f) x)))))

(def one (fn [f] (fn [x] (f x))))
(def two (fn [f] (fn [x] (f (f x)))))

(defn add [m n]
  (fn [f] (fn [x] ((m f) ((n f) x)))))

;;; Exercise 2.7

(defn make-interval [a b]
  [a b])

(defn upper-bound [int] (apply max int))
(defn lower-bound [int] (apply min int))

;;; Exercise 2.8

(defn sub-interval [a b]
  (make-interval (max (- (upper-bound a)
                         (lower-bound b))
                      (- (upper-bound b)
                         (lower-bound a)))
                 (min (- (lower-bound a)
                         (upper-bound b))
                      (- (lower-bound b)
                         (upper-bound a)))))

;;; Exercise 2.12

(defn make-center-percent [c p]
  (let [ratio (/ p 100.0)]
    (make-interval (+ c (* c ratio))
                   (- c (* c ratio)))))

(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(defn percent [i]
  (let [width (- (upper-bound i) (center i))]
    (* 100 (/ width (center i)))))

;;; Exercise 2.17

(defn last-pair [coll]
  (loop [[x & xs] coll]
    (if xs (recur xs) x)))

(defn last-pair [coll]
  (if (next coll)
    (recur (next coll))
    (first coll)))

;;; Exercise 2.18

(defn my-reverse [coll]
  (loop [c coll acc '()]
    (if c (recur (next c)
                 (conj acc (first c)))
        acc)))

;;; Exercise 2.19

(def first-denomination first)
(def except-first-denomination next)
(def no-more? nil?)

(defn cc [amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values))
                     coin-values))))

;;; Exercise 2.20

(defn same-parity [x & xs]
  (cons x
        (filter (if (even? x) even? odd?) xs)))

;;; Exercise 2.21

(defn square [x] (* x x))

(defn square-list [coll]
  (if (nil? coll)
    nil
    (cons (square (first coll))
          (square-list (next coll)))))

(defn square-list [coll]
  (map square coll))

;;; Exercise 2.23

(defn for-each [f coll]
  (map f coll)                          ;won't work due to laziness
  nil)

(defn for-each [f coll]
  (doall (map f coll)) nil)

;;; Exercise 2.27

(defn deep-reverse [coll]
  (if (vector? coll)
    (map deep-reverse (reverse coll))
    coll))

;;; Exercise 2.28

;;; looking back, I may have jumped the gun here :)
(defn depth-first-tree-map [f tree]
  (if (seq? tree)
    (if (seq? (first tree))
      (concat (depth-first-tree-map f (first tree))
              (depth-first-tree-map f (next tree)))
      (cons (f (first tree))
            (depth-first-tree-map f (next tree))))
    nil))

(defn fringe [tree]
  (depth-first-tree-map identity tree))

;;; Exercise 2.29

(defn make-mobile [l r]
  (list l r))
(defn make-branch [length structure]
  (list length structure))
(defn left-branch [mobile]
  (first mobile))
(defn right-branch [mobile]
  (second mobile))
(defn branch-length [branch]
  (first branch))
(defn branch-structure [branch]
  (second branch))
(defn is-mobile? [mobile]
  (list? mobile))

(defn total-weight [mobile]
  (if (is-mobile? mobile)
    (let [l-branch-struct (branch-structure (left-branch mobile))
          r-branch-struct (branch-structure (right-branch mobile))]
      (+ (if (list? l-branch-struct)
           (total-weight l-branch-struct)
           l-branch-struct)
         (if (list? r-branch-struct)
           (total-weight r-branch-struct)
           r-branch-struct)))
    mobile))

(defn mobile-balanced? [mobile]
  (defn torque [length weight] (* length weight))
  (if (not (is-mobile? mobile)) true
      (let [l (left-branch mobile)
            r (right-branch mobile)]
        (and (= (torque (branch-length l)
                        (total-weight (branch-structure l)))
                (torque (branch-length r)
                        (total-weight (branch-structure r))))
             (mobile-balanced? (branch-structure l))
             (mobile-balanced? (branch-structure r))))))

;;; Exercise 2.30

(defn square-tree [tree]
  (cond (nil? tree) nil
        (seq? tree) (cons (square-tree (first tree))
                          (square-tree (next tree)))
        :else (* tree tree)))

(defn square-tree [tree]
  (if (seq? tree)
    (map square-tree tree)
    (* tree tree)))

;;; Exercise 2.31

(defn tree-map [f tree]
  (if (seq? tree)
    (map f tree)
    (f tree)))

(defn square-list [tree]
  (tree-map square tree))

;;; Exercise 2.32

(defn subsets [s]
  (if (nil? s) '(())
      (let [rest (subsets (next s))]
        (concat rest
                (map #(cons (first s) %) rest)))))

;;; Exercise 2.33

(defn my-map [f coll]
  (reduce #(concat %1 (list (f %2))) '() coll))

(defn my-append [coll1 coll2]
  (reduce conj coll2 (reverse coll1)))

(defn my-length [coll]
  (reduce (fn [x y] (inc x)) 0 coll))

;;; Exercise 2.34

(defn accumulate [op init coll]         ;foldr
  (if (nil? coll) init
      (op (first coll)
          (accumulate op init (next coll)))))

(defn horner-eval [x coefficient-sequence]
  (accumulate #(+ (* %2 x) %1) 0 coefficient-sequence))

;;; Exercise 2.35

(defn count-leaves [coll]
  (accumulate #(if (list? %1)
                 (+ (count-leaves %1) %2)
                 (inc %2))
              0 coll))

(defn count-leaves [coll]
  (accumulate + 0 (map #(if (list? %)
                          (count-leaves %) 1)
                       coll)))

;;; Exercise 2.36

(defn accumulate-n [op init coll]
  (if (nil? (first coll))
    nil
    (cons (accumulate op init (map first coll))
          (accumulate-n op init (map next coll)))))

;;; Exercise 2.37

(defn dot-product [v w]
  (accumulate + 0 (map * v w)))

(defn matrix-*-vector [m v]
  (map (partial dot-product v) m))

(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map (fn [row] (map #(dot-product row %) cols)) m)))

(defn transpose [m]
  (accumulate-n cons nil m))

;;; Exercise 2.39

(defn my-reverse [coll]
  (accumulate #(concat %2 (list %1)) nil coll))

(defn my-reverse [coll]
  (accumulate #(conj %2 %1) [] coll))

(defn my-reverse [coll]
  (reduce #(cons %2 %1) nil coll))

;;; Exercise 2.40

(defn unique-pairs [n]
  (mapcat (fn [x]
         (map (partial list x)
              (range (inc x) (inc n))))
       (range 1 n)))

(defn unique-pairs [n]
  (for [x (range 1 n) y (range (inc x) (inc n))]
    [x y]))

;;; Exercise 2.41

(defn unique-triples [n]
  (for [x (range 1 (dec n))
        y (range (inc x) n)
        z (range (inc y) (inc n))]
    [x y z]))

(defn sum [coll]
  (reduce + 0 coll))

(defn ex-2-41 [n s]
  (let [trips (unique-triples n)]
    (filter #(= (sum %) s) trips)))

;;; Exercise 2.42

(defn queens [board-size]
  (defn queen-cols [k]
    (defn adjoin-position [row col queens]
      (cons row queens))
    (def empty-board nil)
    (defn safe? [k [trial & queens]]
      (and (not (some #{trial} queens))
           (last (reduce #(let [[k1 k2 acc] %1]
                            [(dec k1) (inc k2)
                             (and acc (not= %2 k1) (not= %2 k2))])
                         [(dec trial) (inc trial) true]
                         queens))))
    (if (= k 0)
      (list empty-board)
      (filter #(safe? k %)
              (mapcat (fn [rest-of-queens]
                        (map (fn [row]
                               (adjoin-position row k rest-of-queens))
                             (range 1 (inc board-size))))
                      (queen-cols (dec k))))))
  (queen-cols board-size))

;;; Exercise 2.44

(defn up-split [painter n]
  (if (= n 0) painter
      (let [smaller (up-split painter (dec n))]
        (below painter
               (beside smaller smaller)))))

;;; Exercise 2.45

(defn split [a b]
  (fn ret [painter n]
    (if (= n 0)
      (let [smaller (ret painter (dec n))]
        (a painter (b smaller smaller))))))

;;; Exercise 2.46

(defn make-vect [x y] [x y])

(def xcor-vect first)
(def ycor-vect second)

(defn add-vect [v1 v2] (map + v1 v2))
(defn sub-vect [v1 v2] (map - v1 v2))
(defn scale-vect [v s] (map (partial * s) v))

;;; Exercise 2.47

(defn make-frame [origin e1 e2] [origin e1 e2])
(def get-origin first)
(def get-edge-1 second)
(def get-edge-2 #(nth % 2))

(defn make-frame [origin e1 e2] [origin [e1 e2]])
(def get-origin first)
(def get-edge-1 (comp first second))
(def get-edge-2 (comp second second))

;;; Exercise 2.48

(defn make-segment [start end] [start end])
(def start-segment first)
(def end-segment second)

;;; Exercise 2.49

(segments->painter
 (list
  (make-segment (make-vect 0 0)
                (make-vect 0 1))
  (make-segment (make-vect 0 1)
                (make-vect 1 1))
  (make-segment (make-vect 1 1)
                (make-vect 1 0))
  (make-segment (make-vect 1 0)
                (make-vect 0 0))))

(segments->painter
 (list
  (make-segment (make-vect 0 0)
                (make-vect 1 1))
  (make-segment (make-vect 0 1)
                (make-vect 1 0))))

(segments->painter
 (list
  (make-segment (make-vect 0.5 0)
                (make-vect 0 0.5))
  (make-segment (make-vect 0 0.5)
                (make-vect 0.5 1))
  (make-segment (make-vect 0.5 1)
                (make-vect 1 0.5))
  (make-segment (make-vect 1 0.5)
                (make-vect 0.5 0))))

;;; Exercise 2.50

(defn flip-horiz [painter]
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(defn rotate-180 [painter]
  (flip-vert (flip-horiz painter)))

;;; Exercise 2.51

(defn below [painter1 painter2]
  (let [split-point (make-vect 0.0 0.5)]
    (let [paint-below
          (transform-painter painter1
                             (make-vect 0.0 0.0)
                             (make-vect 1.0 0.0)
                             split-point)
          paint-above
          (transform-painter painter2
                             split-point
                             (make-vect 1.0 0.5)
                             (make-vect 0.0 1.0))]
      (fn [frame]
        (paint-below frame)
        (paint-above frame)))))

(defn below [p1 p2]
  (rotate-90
   (beside
    (rotate-270 p1)
    (rotate-270 p2))))

;;; Exercise 2.56

(def variable? symbol?)
(def same-variable? #(and (variable? %1)
                          (variable? %2)
                          (= %1 %2)))

(defn =number? [exp num]
  (and (number? exp)
       (= exp num)))

(defmacro def-binary-op [symbol constructor
                         test left right constructor-exprs]
  `(do
     (defn ~constructor [~left ~right]
       (cond ~@constructor-exprs
             :else (list '~symbol ~left ~right)))
     (def ~test #(and (list? %) (= (first %) '~symbol)))
     (def ~left second)
     (def ~right #(nth % 2))))

(def-binary-op + make-sum sum? addend augend
  [(=number? addend 0) augend
   (=number? augend 0) addend
   (and (number? addend) (number? augend)) (+ addend augend)])

(def-binary-op * make-product product? multiplier multiplicand
  [(or (=number? multiplier 0)
       (=number? multiplicand 0)) 0
   (=number? multiplier 1) multiplicand
   (=number? multiplicand 1) multiplier
   (and (number? multiplicand) (number? multiplier)) (* multiplicand multiplier)])

(def-binary-op ** make-exponentiation exponentiation? base exponent
  [(=number? base 1) 1
   (=number? exponent 1) exponent
   (=number? exponent 0) 1])

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) var)
                             (deriv (augend exp) var))
        (product? exp) (make-sum
                        (make-product (multiplier exp)
                                      (deriv (multiplicand exp) var))
                        (make-product (deriv (multiplier exp) var)
                                      (multiplicand exp)))
        (exponentiation? exp) (make-product
                               (make-product
                                (exponent exp)
                                (make-exponentiation (base exp)
                                                     (dec (exponent exp))))
                               (deriv (base exp) var))
        :else nil))

;;; Exercise 2.57

(defmacro def-left-assoc-binary-op [symbol constructor
                                    test left right constructor-exprs]
  `(do
     (defn ~constructor [~left & right#]
       (if (nil? right#) ~left
           (let [~right (apply ~constructor right#)]
             (cond ~@constructor-exprs
                   :else (if (~test ~right)
                           (concat (list '~symbol ~left) (rest ~right))
                           (list '~symbol ~left ~right))))))
     (def ~test #(and (list? %) (= (first %) '~symbol)))
     (def ~left second)
     (defn ~right [[_# _# e# & es#]]
       (if (nil? es#) e#
         (apply ~constructor e# es#)))))

(def-left-assoc-binary-op + make-sum sum? addend augend
  [(=number? addend 0) augend
   (=number? augend 0) addend
   (and (number? addend) (number? augend)) (+ addend augend)])

(def-left-assoc-binary-op * make-product product? multiplier multiplicand
  [(or (=number? multiplier 0)
       (=number? multiplicand 0)) 0
   (=number? multiplier 1) multiplicand
   (=number? multiplicand 1) multiplier
   (and (number? multiplicand) (number? multiplier)) (* multiplicand multiplier)])

;;; Exercise 2.59
