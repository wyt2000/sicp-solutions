#lang sicp

(define (reverse items)
  (define (reverse-iter items result)
    (if (null? items)
        result
	(reverse-iter (cdr items) (cons (car items) result))))
  (reverse-iter items (list)))

(reverse (list 1 2 3 4 5 6))
