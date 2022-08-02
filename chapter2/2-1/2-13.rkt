#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound c) (car c))

(define (upper-bound c) (cdr c))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (* c p)))

(define (percent i)
  (/ (width i) (center i)))

(define (print-interval x)
  (display "[")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display "]")
  (newline))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; [c1-b1, c1+b1] * [c2-b2, c2+b2] = [c1c2-c1b2-b1c2+b1b2, c1c2+c1b2+b1c2+b1b2]
; 				  = [c1c2-(c1b2+b1c2), c1c2+(c1b2+b1c2)] (when b1b2 = o(b1,b2))
; c1b2 + b1c2 = c1 * (c2 * p2) + c2 * (c1 * p1) = c1 * c2 * (p1 + p2)
(define (mul-interval-tolerance x y)
  (make-center-percent
    (* (center x) (center y)) (+ (percent x) (percent y))))

(print-interval (mul-interval (make-center-percent 1 0.01) (make-center-percent 2 0.02)))
(print-interval (mul-interval-tolerance (make-center-percent 1 0.01) (make-center-percent 2 0.02)))
