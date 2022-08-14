#lang sicp

(define (accumulate p zero seq)
  (if (null? seq)
      zero
      (p (car seq) (accumulate p zero (cdr seq)))))

(define (filter sat? items)
  (cond ((null? items) nil)
	((sat? (car items)) (cons (car items) (filter sat? (cdr items))))
	(else (filter sat? (cdr items)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enum-interval i j)
  (if (> i j)
      nil
      (cons i (enum-interval (+ i 1) j))))

(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enum-interval 1 (- i 1))))
	   (enum-interval 1 n)))


