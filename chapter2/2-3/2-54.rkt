#lang sicp

(define (equal? a b)
  (cond ((and (null? a) (null? b)) #t)
    	((and (list? a) (list? b)) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
	((and (not (list? a)) (not (list? b))) (eq? a b))
	(else #f)))

(equal? '(this is a list)
        '(this is a list))

(equal? '(this is a list)
        '(this (is a) list))
