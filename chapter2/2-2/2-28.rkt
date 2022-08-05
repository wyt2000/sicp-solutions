#lang sicp

(define (fringe x)
  (define (iter x result)
    (cond ((null? x) result)
          ((not (pair? x)) (cons x result))
          (else (iter (car x) (iter (cdr x) result)))))
  (iter x nil))

(define x (list (list 1 2) (list 3 4)))
(define list1 (list 1 3 (list 5 7) 9))
(define list2 (list (list 7)))
(define list3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(fringe x)
(fringe (list x x))
(fringe list1)
(fringe list2)
(fringe list3)
