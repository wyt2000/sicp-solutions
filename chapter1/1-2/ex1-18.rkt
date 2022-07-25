#lang sicp

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (* b n)
  (define (mul-iter b n a)
    (cond ((= n 1) (+ a b))
	  ((even? n) (mul-iter (double b) (halve n) a))              ;b^{2n} = (b^2)^n
	  (else (mul-iter (double b) (halve (- n 1)) (+ a b))))) 	 ;b^{2n+1} = (b^2)^n * b
  (mul-iter b n 0))
 
(* 325 517)
