(ns sicp.ch4)

;;; syntax

(defn self-evaluating? [exp]
  (or (number? exp) (string? exp)))

(def variable? symbol?)

(defn tagged-list? [exp tag]
  (when (seq exp)
    (= (first exp) tag)))

(def quoted? #(tagged-list? % 'quote))
(def text-of-quotation second)

(def assignment? #(tagged-list? % 'set!))
(def assignment-variable second)
(def assignment-value #(nth % 2))

(def lambda? #(tagged-list? % 'lambda))
(def lambda-parameters second)
(def lambda-body (partial drop 2))
(defn make-lambda [parameters body]
  (list 'lambda parameters body))

;;; (define <var> <value>)
;;; (define (<var> <param_1> ... <param_n>) <body>)
(def definition? #(tagged-list? % 'define))
(defn definition-variable [exp]
  (if (symbol? (second exp))
    (second exp)
    ((comp first second) exp)))
(defn definition-value [exp]
  (if (symbol? (second exp))
    (nth exp 2)
    (make-lambda ((comp rest second) exp)
                 (drop 2 exp))))

(def if? #(tagged-list? % 'if))
(def if-predicate second)
(def if-consequent #(nth % 2))
(defn if-alternative [exp]
  (when-let [more (seq (drop 3 exp))]
    (first more)))
(defn make-if [pred consequent alternative]
  (list 'if pred consequent alternative))

(def begin? #(tagged-list? % 'begin))
(def begin-actions rest)
(def last-exp? (complement next))
(def first-exp first)
(def rest-exps rest)

(def make-begin (partial cons 'begin))
(defn sequence->exp [coll]
  (cond (seq coll) coll
        (last-exp? coll) (first-exp coll)
        :else (make-begin coll)))

(def application? list?)
(def operator first)
(def operands rest)
(def no-operands? seq)
(def first-operand first)
(def rest-operands rest)

(def cond? #(tagged-list? % 'cond))
(def cond-clauses rest)
(def cond-predicate first)
(def cond-actions rest)
(def cond-else-clause? #(= (cond-predicate %) 'else))
(defn expand-clauses [clauses]
  (if-not (seq clauses)
    false                               ;no else clause
    (let [f (first clauses)
          r (next clauses)]
      (if (cond-else-clause? f)
        (if-not r
          (sequence->exp (cond-actions f))
          (throw (RuntimeException.
                  (format "else clause is not last: %s" clauses))))
        (make-if (cond-predicate f)
                 (sequence->exp (cond-actions f))
                 (expand-clauses r))))))
(def cond->if (comp expand-clauses cond-clauses))

(def primitive-procedure? #(tagged-list? % 'primitive))
(def primitve-implementation second)

(defn apply-primitive-procedure [proc args]
  (apply (primitve-implementation proc) args)) ;this is the underlying apply not the one defined here

(defn make-procedure [params body env]
  (list 'procedure params body env))

(def compound-procedure? #(tagged-list? % 'procedure))
(def procedure-parameters second)
(def procedure-body #(nth % 2))
(def procedure-environment #(nth % 3))

(def enclosing-environment rest)
(def first-frame first)
(def the-empty-environment '())

(defn make-frame [variables values]
  (atom (zipmap variables values)))

(def frame-variables keys)
(def frame-values vals)

(defn add-binding-to-frame! [var val frame]
  (swap! frame assoc var val))

(defn extend-environment [vars vals base]
  (when (= (count vars) (count vals))
    (cons (make-frame vars vals)
          base)))

(defn lookup-variable-value [var env]
  (some (comp #(get % var) deref) env))

(defn find-first-frame-containing [var env]
  (some #(when (contains? (deref %) var) %) env))

(defn set-variable-value! [var val env]
  (when-let [frame (find-first-frame-containing var)]
    (add-binding-to-frame! var val frame)))

(defn define-variable! [var val env]
  (add-binding-to-frame! var val
                         (or (find-first-frame-containing var env)
                             (first-frame env))))

;;; eval/apply

(declare eval)

(defn eval-sequence [exps env]
  (if (last-exp? exps)
    (eval (first-exp exps) env)
    (do
      (eval (first-exp exps) env)
      (recur (rest-exps exps) env))))

(defn apply [procedure arguments]
  (cond (primitive-procedure? procedure)
        (apply-primitive-procedure procedure arguments)
        (compound-procedure? procedure)
        (eval-sequence
         (procedure-body procedure)
         (extend-environment
          (procedure-parameters procedure)
          arguments
          (procedure-environment procedure)))
        :else (throw (RuntimeException.
                      (format "Unknown procedure type: %s" procedure)))))

(defn list-of-values [exps env]
  (lazy-seq
   (if (no-operands? exps)
     '()
     (cons (eval (first-operand exps) env)
           (list-of-values (rest-operands exps) env)))))

(defn eval-assignment [exp env]
  (set-variable-value! (assignment-variable exp)
                       (eval (definition-value exp) env)
                       env))

(defn eval-definition [exp env]
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env))

(defn eval-if [exp env]
  (if (eval (if-predicate exp) env)
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

;;; original -- replaced with multimethods below
;; (defn eval [exp env]
;;   (cond (self-evaluating? exp) exp
;;         (variable? exp) (lookup-variable-value exp env)
;;         (quoted? exp) (text-of-quotation exp)
;;         (assignment? exp) (eval-assignment exp env)
;;         (definition? exp) (eval-definition exp env)
;;         (if? exp) (eval-if exp env)
;;         (lambda? exp)
;;         (make-procedure (lambda-parameters exp)
;;                         (lambda-body exp)
;;                         env)
;;         (begin? exp)
;;         (eval-sequence (begin-actions exp) env)
;;         (cond? exp) (eval (cond->if exp) env)
;;         (application? exp)
;;         (apply (eval (operator exp) env)
;;                (list-of-values (operands exp) env))
;;         :else (throw (RuntimeException.
;;                       (format "Unknown expression: %s" exp)))))

(def primitive-procedures
  `((car ~first)
    (cdr ~rest)
    (cons ~cons)
    (null? ~nil?)
    (+ ~+)))
(def primitive-procedure-names (map first primitive-procedures))
(def primitive-procedure-objects (map second primitive-procedures))

(defn setup-environment []
  (let [initial-env (extend-environment primitive-procedure-names
                                        primitive-procedure-objects
                                        the-empty-environment)]
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(def the-global-environment (setup-environment))

(def input-prompt "> ")
(def output-prompt ">> ")

(def prompt-for-input #(print (newline) (newline) % (newline)))
(def announce-output #(print (newline) % (newline)))

(defn user-print [object]
  (if (compound-procedure? object)
    (print (list 'compound-procedure
                 (procedure-parameters object)
                 (procedure-body object)
                 '<procedure-env>))
    (print object)))

(defn driver-loop []
  (prompt-for-input input-prompt)
  (let [input (read)
        output (eval input the-global-environment)]
    (announce-output output-prompt)
    (user-print output)))

;;; Exercise 4.1

;;; evaluate left-to-right
(defn list-of-values [exps env]
  (if (no-operands? exps) '()
      (let [value (eval (first-operand exps) env)]
        (cons value (list-of-values (rest-operands exps) env)))))

;;; evaluate right-to-left
(defn list-of-values [exps env]
  (if (no-operands? exps) '()
      (let [tail (list-of-values (rest-operands exps) env)]
        (cons (eval (first-operand exps) env)
              tail))))

;;; Exercise 4.3

;;; a nicer way of doing this would be to have a reader that parses
;;; into a structure that is tagged. The eval multimethod could then
;;; inspect this tag for dispatch. This approach will work for the
;;; sake of the exercise, however.
(defn form [exp _]
  (cond (or (number? exp)
            (string? exp)) :self-evaluating
            (symbol? exp) :variable
            :else (first exp)))

(defmulti eval form)

(defmethod eval :self-evaluating [exp env]
  exp)

(defmethod eval :variable [exp env]
  (lookup-variable-value exp env))

(defmethod eval 'quote [exp env]
  (text-of-quotation exp))

(defmethod eval 'set! [exp env]
  (eval-assignment exp env))

(defmethod eval 'define [exp env]
  (eval-definition exp env))

(defmethod eval 'if [exp env]
  (eval-if exp env))

(defmethod eval 'lambda [exp env]
  (make-procedure (lambda-parameters exp)
                  (lambda-body exp)
                  env))

(defmethod eval 'begin [exp env]
  (eval-sequence (begin-actions exp) env))

(defmethod eval 'cond [exp env]
  (eval (cond->if exp) env))

(defmethod eval :default [exp env]
  (apply (eval (operator exp) env)
         (list-of-values (operands exp) env)))

;;; Exercise 4.4

(defn and->if [exps env]
  (let [rexps (reverse exps)
        init (eval (first rexps) env)]  ;evaluate this only once
    (reduce (fn [acc exp]
              (make-if exp acc false))
            init (cons init (rest rexps)))))

(defmethod eval 'and [exp env]
  (eval (and->if (rest exp) env) env))

(defn or->if [[exp & more] env]
  (if-not (seq more)
    exp
    (let [e (eval exp env)]         ;bad: captures e
       (make-if e e (or->if more env)))))

(defmethod eval 'or [exp env]
  (eval (or->if (rest exp) env) env))

;;; Exercise 4.6

(defn let->combination [bindings body]
  (list
   (make-lambda (map first bindings)
                body)
   (map second bindings)))

(defmethod eval 'let [exp env]
  (eval (let->combination (second exp)
                          (nth exp 2))
        env))

;;; Exercise 4.41

(defn satisfies? [baker cooper fletcher miller smith]
  (and
   (distinct? baker cooper fletcher miller smith)
   (not= baker 5)
   (not= cooper 1)
   (not= fletcher 5)
   (not= fletcher 1)
   (> miller cooper)
   (not= (Math/abs (- smith fletcher)) 1)
   (not= (Math/abs (- fletcher cooper)) 1)))

(defn search [pred choices]
  (letfn [(help [args [possibles & more]]
            (when (seq possibles)
              (if more
                (some #(help % more)
                      (map #(conj args %) possibles))
                (let [sat (first (filter
                                  (apply partial pred args)
                                  possibles))]
                  (when sat
                    (conj args sat))))))]
    (help [] choices)))

(search satisfies? [[1 2 3 4 5]
                    [1 2 3 4 5]
                    [1 2 3 4 5]
                    [1 2 3 4 5]
                    [1 2 3 4 5]]) ; => [3 2 4 5 1]

;;; Exercise 4.42

(defn xor [a b]
  (or (and a (not b))
      (and (not a) b)))

(defn satisfies? [b e j k m]
  (and
   (distinct? b e j k m)
   (xor (= k 2) (= b 3))
   (xor (= e 1) (= j 2))
   (xor (= j 3) (= e 5))
   (xor (= k 2) (= m 4))
   (xor (= m 4) (= b 1))))

(search satisfies? [[1 2 3 4 5]
                    [1 2 3 4 5]
                    [1 2 3 4 5]
                    [1 2 3 4 5]
                    [1 2 3 4 5]]) ; => [3 5 2 1 4]

;;; Continuation-passing style search implementation in the same
;;; manner as section 4.3.3

(defn check [pred args fail]
  (if (apply pred args) args (fail)))

(defn dfs [[choices & more] success fail]
  (if (seq choices)
    (success [(first choices) more]
             #(dfs (cons (rest choices) more) success fail))
    (fail)))

(defn search [pred choices]
  (letfn [(help [args choices fail]
            (dfs choices
                 (fn [[choice more] fail2]
                   (if (seq more)
                     (help (conj args choice) more fail2)
                     (check pred (conj args choice) fail2)))
                 fail))]
    (help [] choices (fn [] :not-found))))
