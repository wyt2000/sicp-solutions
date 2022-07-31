#lang sicp

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment p q) (cons p q))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (mid-point s)
  (let ((start-point (start-segment s)) (end-point (end-segment s)))
    (make-point (/ (+ (x-point start-point) (x-point end-point)) 2)
                (/ (+ (y-point start-point) (y-point end-point)) 2))))

(define s
  (make-segment (make-point -12 7) (make-point 4 -3)))

(print-point (mid-point s))
                      