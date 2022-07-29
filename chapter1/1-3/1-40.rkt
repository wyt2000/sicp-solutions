#lang sicp

(define (fixed-point f guess)
  (define (close-enough? x y)
    (< (abs (- x y)) 1e-6))
  (define (fixed-point-iter x) 
    (define next (f x))
    (if (close-enough? x next)
        x
	(fixed-point-iter next)))
  (fixed-point-iter guess))

(define (deriv g)
  (define dx 1e-6)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

;x^3 + 2x^2 + x = 0
(newtons-method (cubic 2 1 0) -3)  ;-1
(newtons-method (cubic 2 1 0) 1)   ;0

