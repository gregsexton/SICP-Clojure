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
