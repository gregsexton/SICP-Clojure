(ns sicp.ch1)

;;; Exercise 1.1

;;; Exercise 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))               ; -37/150

;;; Exercise 1.3

(defn sum-square-larger [a b c]
  (defn square [n] (* n n))
  (- (apply + (map square (vector a b c)))
     (square (min a b c))))
