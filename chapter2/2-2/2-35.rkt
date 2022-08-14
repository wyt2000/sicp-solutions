#lang sicp

(define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 

(define (fringe x)
  (define (iter x result)
    (cond ((null? x) result)
          ((not (pair? x)) (cons x result))
          (else (iter (car x) (iter (cdr x) result)))))
  (iter x nil))

(define (count-leaves t)
  (accumulate (lambda (x y) (+ y 1)) 0 (map fringe t)))
