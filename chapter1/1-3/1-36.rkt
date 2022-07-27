#lang sicp

(define (fixed-point f first-try)
  (define (close-enough? x y)
    (< (abs (- x y)) 1e-6))
  (define (try x)
    (let ((next (f x)))
      (display next)
      (newline)
      (if (close-enough? x next)
          x
	  (try next))))
  (try first-try))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2) ;41 times
(fixed-point
  (lambda (x)
    (/ (+ x (/ (log 1000) (log x))) 2)) 2) ;12 times
