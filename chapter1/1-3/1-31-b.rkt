#lang sicp

(define (prod term a next b)
  (define (prod-iter a result)
    (if (> a b)
        result
	(prod-iter (next a) (* (term a) result))))
  (prod-iter a 1))

(define (factorial n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (prod identity 1 inc n))

(factorial 10)

(define (Wallis-pi n)
  (define (square n) (* n n))
  (define (term n) 
    (/ (square (* 2.0 n)) (* (- (* 2.0 n) 1.0) (+ (* 2.0 n) 1.0))))
  (define (inc n) (+ n 1.0))
  (* 2.0 (prod term 1.0 inc n)))

(Wallis-pi 1000)
