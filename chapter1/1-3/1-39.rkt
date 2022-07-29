#lang sicp

(define (cont-frac n d k)
  (define (cont-frac-iter i result)
    (if (= i 0)
        result 
  	(cont-frac-iter (- i 1) (/ (n i) (+ (d i) result)))))
  (cont-frac-iter k 0))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1) x (- (* x x))))
  (define (d i)
    (- (* 2 i) 1))
  (cont-frac n d k))


(tan-cf (/ 3.1415926 4) 100)
