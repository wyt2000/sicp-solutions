#lang sicp

(define (accumulate p zero seq)
  (if (null? seq)
      zero
      (p (car seq) (accumulate p zero (cdr seq)))))

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))             ;(/ 1 (/ 2 (/ 3 1))) = (/ 1 2/3) = 3/2
(fold-left / 1 (list 1 2 3))	          ;(/ (/ (/ 1 1) 2) 3) = 1/6
(fold-right list nil (list 1 2 3))	  ;(list 1 (list 2 (list 3 nil))) = (1 (2 (3 ())))
(fold-left list nil (list 1 2 3))         ;(list (list (list () 1) 2) 3) = (((() 1) 2) 3)

;fold-right: (op (an-1 (an init)) ... a1)
;fold-left:  (op init a1 a2 ... an)
;So op need commutativity and associativity.
