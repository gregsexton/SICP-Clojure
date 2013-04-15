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

;;; Exercise 3.3/4

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

;;; Exercise 3.5

(defn monte-carlo [trials experiment]
  (loop [remaining trials
         passed 0]
    (cond (= remaining 0) (/ passed trials)
          (experiment) (recur (dec remaining) (inc passed))
          :else (recur (dec remaining) passed))))

(defn rand-in-range [low high]
  (+ low (* (Math/random) (- high low))))

(defn estimate-integral [P x1 x2 y1 y2 trials]
  (let [area (* (- x2 x1) (- y2 y1))]
    (* area
       (monte-carlo trials #(P (rand-in-range x1 x2)
                               (rand-in-range y1 y2))))))

;;; Exercise 3.7

(defn make-joint [acct pass new-pass]
  (fn [p m]
    (if (= p new-pass)
      (acct pass m)
      (throw (RuntimeException. "Password incorrect.")))))

;;; Exercise 3.24

(defn make-table []
  (let [local-table (atom {})]
    (defn lookup [k1 k2]
      (let [subtable (get @local-table k1)]
        (when subtable
          (get subtable k2))))
    (defn insert! [k1 k2 val]
      (let [subtable (get @local-table k1 {})]
        (swap! local-table
               assoc k1 (assoc subtable k2 val))))
    (def dispatch-table {:lookup lookup
                         :insert insert!})
    #(% dispatch-table)))

(deftype Alist [eq? contents]
  clojure.lang.IPersistentMap
  (assoc [_ k v]
    (Alist. eq? (cons [k v] contents)))
  (assocEx [self k v]
    (.assoc self k v))
  (without [self k]
    (Alist. eq? (filter (not #(= (first %) k))
                    contents)))

  java.lang.Iterable
  (iterator [_]
    (let [contents (atom contents)]
      (proxy [java.util.Iterator] []
        (hasNext [] (not (nil? @contents)))
        (next []
          (let [entry (first @contents)]
            (swap! contents next)
            (clojure.lang.MapEntry. (first entry)
                                    (second entry)))))))

  clojure.lang.Associative
  (containsKey [self k]
    (not (nil? (.entryAt self k))))
  (entryAt [_ k]
    (let [entry (some #(when (eq? (first %) k) %) contents)]
      (when entry
        (clojure.lang.MapEntry. (first entry)
                                (second entry)))))

  clojure.lang.IPersistentCollection
  (count [_] (count contents))
  (cons [_ o] (cons o contents))
  (empty [_] (Alist. eq? '()))
  (equiv [_ o]
    (and (isa? (class o) Alist) (= o contents)))

  clojure.lang.Seqable
  (seq [self] (iterator-seq (.iterator self)))

  clojure.lang.ILookup
  (valAt [self k] (.valAt self k nil))
  (valAt [self k not-found]
    (let [entry (.entryAt self k)]
      (if entry
        (.val entry)
        not-found))))

(defn alist-map
  ([] (Alist. = '()))
  ([k v & keyvals]
     (assoc (apply alist-map keyvals)
       k v)))

;;; Exercise 3.47

(defn make-mutex [name]
  (fn [op]
    (println name " mutex: " op)))

;;; I _think_ this would work
(defn make-semaphore [n]
  (let [sem-mutex (make-mutex "sem")
        serial-mutex (make-mutex "serial")
        n (atom n)]
    (defn semaphore [op]
      (serial-mutex :aquire)
      (let [current @n]
        (condp = op
          :aquire (if (<= current 0)
                    (do
                      (serial-mutex :release)
                      (sem-mutex :aquire) ;should block
                      (sem-mutex :release)
                      (semaphore :aquire))
                    (do
                      (reset! n (dec current))
                      (when (= (dec current) 0)
                        (sem-mutex :aquire)) ;should not block, should be available
                      (serial-mutex :release)))
          :release (do
                     (reset! n (inc current))
                     (sem-mutex :release) ;should be a noop if not aquired
                     (serial-mutex :release)))))
    semaphore))

;;; Exercise 3.50

(defn stream-map [proc & argstreams]
  (lazy-seq
   (if (empty? (first argstreams)) '()
       (cons
        (apply proc (map first argstreams))
        (apply stream-map (cons proc (map rest argstreams)))))))

;;; Exercise 3.54

(defn sieve [stream]
  (lazy-seq
   (cons
    (first stream)
    (sieve (filter #(not= 0 (mod % (first stream)))
                   (rest stream))))))

(def primes (sieve (iterate inc 2)))

(def factorials
  (lazy-seq
   (cons 1
         (map * factorials (iterate inc 2)))))

;;; Exercise 3.55

(defn partial-sums [coll]
  (lazy-seq
   (cons (first coll)
         (map + (partial-sums coll) (rest coll)))))
