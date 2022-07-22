#lang sicp

(define (cqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3)) 

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cube x)
  (* x x x))

(define (cqrt x)
  (cqrt-iter 1.0 x))

(cqrt 27)

