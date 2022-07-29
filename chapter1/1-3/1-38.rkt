#lang sicp

(define (cont-frac n d k)
  (define (cont-frac-iter i result)
    (if (= i 0)
        result 
  	(cont-frac-iter (- i 1) (/ (n i) (+ (d i) result)))))
  (cont-frac-iter k 0))

(define (get-e k)
  (define (d i)
    (if (= (remainder i 3) 2) (* 2 (/ (+ i 1) 3)) 1))
  (+ 2 (cont-frac (lambda (i) 1.0) d k)))

(get-e 100)
