#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound c) (car c))

(define (upper-bound c) (cdr c))

(define (print-interval x)
  (display "[")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display "]")
  (newline))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (get-width x)
  (/ (- upper-bound lower-bound) 2))

; [a,b] + [c,d] = [a+c,b+d]
; w([a+c, b+d]) = ((b + d) - (a + c)) / 2 = (b - a) / 2 + (d - c) / 2 = w([a,b]) + w([c,d])
(define (get-add-width x y)
  (+ (get-width x) (get-width y)))

; [a,b] - [c,d] = [a-d,b-c]
; w([a-d, b-c]) = ((b - c) - (a - d)) / 2 = (b - a) / 2 + (d - c) / 2 = w([a,b]) + w([c,d])
(define (get-sub-width x y)
  (- (get-width x) (get-width y)))

; for [a,b] * [c,d], we assume it equals to [ac, bd]
; Then w([ac, bd]) = (bd - ac) / 2, couldn't be expressed by (b - a) and (d - c).
