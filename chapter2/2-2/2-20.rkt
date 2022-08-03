#lang sicp

(define (same-parity first . items)
  (define parity (remainder first 2))
  (define (sp items)
    (cond ((null? items) items)
	  ((= (remainder (car items) 2) parity) (cons (car items) (sp (cdr items))))
	  (else (sp (cdr items)))))
  (cons first (sp items)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
