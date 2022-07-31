#lang sicp

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-rectangle p q) (cons p q))
(define (up-left-rectangle r) (car r))
(define (down-right-rectangle r) (cdr r))

(define (get-side-length-rectangle r)
  (define up-down-length (abs (- (y-point (down-right-rectangle r)) (y-point (up-left-rectangle r)))))
  (define left-right-length (abs (- (x-point (down-right-rectangle r)) (x-point (up-left-rectangle r)))))
  (cons up-down-length left-right-length))

(define (get-perimeter-rectangle r)
  (define side-length (get-side-length-rectangle r))
  (* 2 (+ (car side-length) (cdr side-length))))

(define (get-area-rectangle r)
  (define side-length (get-side-length-rectangle r))
  (* (car side-length) (cdr side-length)))

(define r (make-rectangle (make-point -1 2) (make-point 5 7)))

(get-perimeter-rectangle r)
(get-area-rectangle r)