#lang sicp

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeat f n)
  (define (double f)
    (compose f f))
  (cond ((= n 0) (lambda (x) x)) 
	((= (remainder n 2) 0) (double (repeat f (/ n 2)))) 
	(else (compose f (repeat f (- n 1))))))

(define (smooth f)
  (define dx 0.1)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (smooth-ntimes f n)
  ((repeat smooth n) f))

((smooth-ntimes sin 10) 1)
