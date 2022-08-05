#lang sicp

(define (accumulate p zero seq)
  (define (iter seq result)
          (if (null? seq)
	      result
	      (iter (cdr seq) (p (car seq) result))))
  (iter seq zero))

(define (append seq1 seq2)
  (accumulate cons seq2 (reverse seq1)))

(append (list 1 2 3) (list 4 5 6))

(define (map p sequence)
  (accumulate (lambda (x y) (append y (list (p x)))) nil sequence))

(map (lambda (x) (+ x 1)) (list 1 2 3 4 5))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(length (list 1 2 3 4 5))
