#lang sicp

(define (accumulate p zero seq)
  (define (iter seq result)
          (if (null? seq)
	      result
	      (iter (cdr seq) (p result (car seq)))))
  (iter seq zero))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(map (lambda (x) (+ x 1)) (list 1 2 3 4 5))

;(define (append seq1 seq2)
;  (accumulate cons <??> <??>))
;
;(define (length sequence)
;  (accumulate <??> 0 sequence))
