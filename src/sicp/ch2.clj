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
