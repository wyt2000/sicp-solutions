#lang sicp

(define (accumulate p zero seq)
  (if (null? seq)
      zero
      (p (car seq) (accumulate p zero (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (u) (dot-product u v)) m))

(define m (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))

(define v (list 2 4 6 8))

(matrix-*-vector m v)

(define (transpose mat)
  (accumulate-n (lambda (x y) (cons x y)) nil mat))

(transpose m)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda(v) (matrix-*-vector cols v)) m)))

(define n (list (list 2 3 4) (list 5 6 7) (list 8 9 10) (list 11 12 13)))

(matrix-*-matrix m n)
