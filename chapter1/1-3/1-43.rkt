#lang sicp

(define (compose f g)
  (lambda (x)
    (f (g x))))

;Just like fast power
(define (repeat f n)
  (define (double f)
    (compose f f))
  (cond ((= n 0) (lambda (x) x)) 
	((= (remainder n 2) 0) (double (repeat f (/ n 2)))) 
	(else (compose f (repeat f (- n 1))))))

(define (square x) (* x x))

((repeat square 2) 5)
