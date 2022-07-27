#lang sicp

(define (fixed-point f first-try)
  (define (close-enough? x y)
    (< (abs (- x y)) 1e-6))
  (define (try x)
    (let ((next (f x)))
    (if (close-enough? x next)
        x
	(try next))))
  (try first-try))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
