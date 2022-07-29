#lang sicp

(define (square x)
  (* x x))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeat f n)
  (define (double f)
    (compose f f))
  (cond ((= n 0) (lambda (x) x)) 
	((= (remainder n 2) 0) (double (repeat f (/ n 2)))) 
	(else (compose f (repeat f (- n 1))))))

(define (fixed-point f guess)
  (define (close-enough? x y)
    (< (abs (- x y)) 1e-6))
  (define (fixed-point-iter x) 
    (define next (f x))
    (if (close-enough? x next)
        x
	(fixed-point-iter next)))
  (fixed-point-iter guess))

(define (average-damp f)
  (lambda (x)
    (/ (+ x (f x)) 2)))

(define (fast-power x n)
  ((repeat (lambda (y) (* x y)) n) 1))

(define (n-root x n)
  (fixed-point ((repeat average-damp 2) (lambda (y) (/ x (fast-power y (- n 1))))) 1.0))

(n-root 625 4)
