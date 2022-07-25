#lang sicp

(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (define (fast-expt-iter b n a)
    (cond ((= n 1) (* a b))
	  ((even? n) (fast-expt-iter (square b) (/ n 2) a))              ;b^{2n} = (b^2)^n
	  (else (fast-expt-iter (square b) (/ (- n 1) 2) (* a b))))) 	 ;b^{2n+1} = (b^2)^n * b
  (fast-expt-iter b n 1))
 
(fast-expt 2 10)
;2^10 = 4^5 = 16^2 * 4 = 256^1 * 4
