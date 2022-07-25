#lang sicp

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (* a b)
  (cond ((= b 1) a)
	((even? b) (* (double a) (halve b)))
	(else (+ a (* a (- b 1))))))

(* 4 10)
