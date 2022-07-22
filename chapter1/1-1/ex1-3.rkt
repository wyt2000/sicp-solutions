#lang sicp
(define (sum_of_max_two a b c)
  (cond ((and (<= a b) (<= a c)) (+ b c))
	((and (<= b a) (<= b c)) (+ a c))
	((and (<= c a) (<= c b)) (+ a b))))

(sum_of_max_two 1 2 3) 
(sum_of_max_two 1 3 2) 
(sum_of_max_two 2 1 3) 
(sum_of_max_two 2 3 1) 
(sum_of_max_two 3 1 2) 
(sum_of_max_two 3 2 1) 

(sum_of_max_two 1 1 2) 
