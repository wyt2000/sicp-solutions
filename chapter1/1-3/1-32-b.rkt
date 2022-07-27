#lang sicp 

(define (accumulate combiner null-value term a next b)
  (define (accumulate-iter a result)
    (if (> a b)
        result
	(accumulate-iter (next a) (combiner (term a) result))))
  (accumulate-iter a null-value))
  
(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (prod term a next b)
  (accumulate * 1 term a next b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))

(define (factorial n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (prod identity 1 inc n))

(factorial 10)

