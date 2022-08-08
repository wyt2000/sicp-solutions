#lang sicp

(define (accumulate p zero seq)
  (if (null? seq)
      zero
      (p (car seq) (accumulate p zero (cdr seq)))))

;map can be used to do recurision for list.
(define (count-leaves t)
  (accumulate + 0 (map (lambda (t)
			 (cond ((null? t) 0)
			       ((pair? t) (count-leaves t))
			       (else 1)))
		       t)))

(define tree (list 1 2 3 (list 4 5 (list 6 7)))) 

(count-leaves tree)
