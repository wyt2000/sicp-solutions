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
  (if (and (<= (lower-bound y) 0) (>= (upper-bound y) 0))
      (error "Interval across 0: " y)
      (mul-interval x
		    (make-interval (/ 1.0 (lower-bound y))
				   (/ 1.0 (upper-bound y))))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define r1 (make-center-percent 1 0.1))
(define r2 (make-center-percent 2 0.2))

(print-interval (par1 r1 r2)) ;[0.4114285714285715,1.056]
(print-interval (par2 r1 r2)) ;[0.576,0.7542857142857143]

(define a (make-center-percent 1 0.5))
(define b (make-center-percent 1 0.5))

(print-interval (div-interval a a)) (print-interval (div-interval a b)) 

; [0.5, 1.5] / [0.5, 1.5] = [1/3, 3.0]
; a / a should be 1.0 because a is a definite number, but a / b should be an interval because a and b have no connection.
; par2 is better than par1 because there is only one (r1, r2) in par2, so it avoid extra tolerance.

