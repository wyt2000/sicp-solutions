#lang sicp

(define (power x n)
  (cond ((= n 0) 1)
        ((= (remainder n 2) 0) ((lambda (x) (* x x)) (power x (/ n 2))))
        (else (* x (power x (- n 1))))))

(define (cons a b)
  (* (power 2 a) (power 3 b)))

(define (get-factor-number x n)
  (define (get-num-iter x result)
    (if (not (= (remainder x n) 0))
         result
         (get-num-iter (/ x n) (+ result 1))))
  (get-num-iter x 0))

(define (car c)
  (get-factor-number c 2))

(define (cdr c)
  (get-factor-number c 3))

(car (cons 7 13))
(cdr (cons 7 13))