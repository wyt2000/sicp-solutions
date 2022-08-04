#lang sicp

(define (reverse items)
  (define (reverse-iter items result)
    (if (null? items)
        result
	(reverse-iter (cdr items) (cons (car items) result))))
  (reverse-iter items (list)))

(define (deep-reverse items)
  (define (iter items result)
    (if (null? items)
        result
	(iter (cdr items)
              (cons (if (pair? (car items)) (deep-reverse (car items)) (car items)) result)))) 
  (iter items nil))


(define x (list (list 1 2) (list 3 4)))
(define y (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(reverse x)
(deep-reverse x)

(reverse y)
(deep-reverse y)
