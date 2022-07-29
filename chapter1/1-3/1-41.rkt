#lang sicp

(define (double p)
  (lambda (x) (p (p x))))

(((double (double double)) inc) 5)
