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

(define (reverse-right seq)
  (fold-right (lambda (x y) (append y (list x))) nil seq))

(define (reverse-left seq)
  (fold-left (lambda (x y) (cons y x)) nil seq))

(reverse-right (list 1 2 3 4 5))
(reverse-left (list 1 2 3 4 5))
