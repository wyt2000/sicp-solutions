#lang sicp

(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(square-list (list 1 2 3 4))
;Because list should be (cons first next-list)
;This procedure will return ((((() . 1) . 4) . 9) . 16)

;There is a real iter solution in http://community.schemewiki.org/?sicp-ex-2.22
 (define (square-list-iter items) 
     (define (iter l pick) 
         (define r (square (car l))) 
         (if (null? (cdr l)) 
             (pick (list r)) 
             (iter (cdr l) (lambda (x) (pick (cons r x)))))) 
     (iter items (lambda (x) x))) 

(square-list-iter (list 1 2 3 4))

