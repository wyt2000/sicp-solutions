#lang sicp
; For old good-enough:
; (sqrt 0.0001) Should be 0.01, but return 0.03230844833048122.
; (sqrt 1e+100) Will stuck. 

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess improved_guess)
  (< (/ (abs (- improved_guess guess)) guess) 0.001))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 0.0001) ;0.010000714038711746
(sqrt 1e+100) ;1.000000633105179e+50
