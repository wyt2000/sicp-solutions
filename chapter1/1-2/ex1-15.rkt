#lang sicp
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

(sine 12.15)
;12.15 / 3^n <= 0.1
;n >= log_3^121.5
;n = 5
;space complex: O(log_3(10a)) = O(loga)
;time complex:  O(loga)