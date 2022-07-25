#lang sicp

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))
(f 10)

(define (f2 n)
  (define (f-iter a b c counter)
    (cond ((= counter n) a)
          (else (f-iter
                 b
                 c
                 (+ c (* 2 b) (* 3 a))
                 (+ counter 1)
                 ))))
  (f-iter 0 1 2 0))

(f2 10)