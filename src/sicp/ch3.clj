(ns sicp.ch3)

(def balance (atom 100))

(defn withdraw [amount]
  (if (>= @balance amount)
    (do
      (swap! balance #(- % amount))
      @balance)
    (throw (RuntimeException. "Insufficient funds."))))

(defn withdraw [amount]
  (if (>= @balance amount)
    (do
      (reset! balance (- @balance amount))
      @balance)
    (throw (RuntimeException. "Insufficient funds."))))

(defn withdraw []
  (let [balance (atom 100)]
    (fn [amount]
      (if (>= @balance amount)
        (do
          (swap! balance #(- % amount))
          @balance)
        (throw (RuntimeException. "Insufficient funds."))))))

;;; Exercise 3.1

(defn make-accumulator [initial]
  (let [acc (atom initial)]
    (fn [value]
      (swap! acc #(+ % value)))))

;;; Exercise 3.2

(defn make-monitored [f]                ;works for any number of args
  (let [count (atom 0)]
    (defn call [& args]
      (swap! count inc)
      (apply f args))
    (fn
      ([] (call))
      ([x] (cond (= x :how-many-calls?) count
                 (= x :reset) (reset! count 0)
                 :else (call x)))
      ([x & xs] (apply call x xs)))))

;;; Exercise 3.3

(defn make-account [balance password]
  (let [balance (atom balance)
        attempts (atom 0)]
    (defn call-the-cops []
      (throw (RuntimeException. "Now you're in trouble...")))
    (defn withdraw [amount]
      (if (< @balance amount)
        (throw (RuntimeException. "Insufficient funds"))
        (swap! balance #(- % amount))))
    (defn deposit [amount]
      (swap! balance #(+ % amount)))
    (defn dispatch [pass m]
      (if (= pass password)
        (do
          (reset! attempts 0)
          (cond (= m :withdraw) withdraw
                (= m :deposit) deposit
                :else (throw (RuntimeException. "Unknown request"))))
        (do
          (swap! attempts inc)
          (if (> @attempts 7) (call-the-cops)
              (throw (RuntimeException. "Password incorrect."))))))
    dispatch))

;;; Exercise 3.4
