#lang sicp

<<<<<<< HEAD
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
=======
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
>>>>>>> f0e4e524b0fc6ed916b805a995e5420759fe4469
