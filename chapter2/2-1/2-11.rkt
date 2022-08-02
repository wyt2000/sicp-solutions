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

;[a,b] * [c,d]
;case 1: 0 <= a <= b, 0 <= c <= d | [a * c, b * d]
;case 2: 0 <= a <= b, c <  0 <= d | [b * c, b * d]
;case 3: 0 <= a <= b, c <= d <  0 | [b * c, a * d]
;case 4: a <  0 <= b, 0 <= c <= d | [a * d, b * d]
;case 5: a <  0 <= b, c <  0 <= d | [min (a * d, b * c), max(a * c, b * d)]
;case 6: a <  0 <= b, c <= d <  0 | [b * c, a * c]
;case 7: a <= b <  0, 0 <= c <= d | [a * d, b * c]
;case 8: a <= b <  0, c <  0 <= d | [a * d, a * c]
;case 9: a <= b <  0, c <= d <  0 | [b * d, a * c]
(define (mul-interval x y)
  (let ((a (lower-bound x))
	(b (upper-bound x))
	(c (lower-bound y))
	(d (upper-bound y)))
    (cond ((and (<= 0 a) (<= a b) (<= 0 c) (<= c d)) (make-interval (* a c) (* b d)))		                  	;case 1 
	  ((and (<= 0 a) (<= a b) (<  c 0) (<= 0 d)) (make-interval (* b c) (* b d)))					;case 2 
	  ((and (<= 0 a) (<= a b) (<= c d) (<  d 0)) (make-interval (* b c) (* a d)))					;case 3
	  ((and (<  a 0) (<= 0 b) (<= 0 c) (<= c d)) (make-interval (* a d) (* b d)))					;case 4
	  ((and (<  a 0) (<= 0 b) (<  c 0) (<= 0 d)) (make-interval (min (* a d) (* b c)) (max (* a c) (* b d))))	;case 5
	  ((and (<  a 0) (<= 0 b) (<= c d) (<  d 0)) (make-interval (* b c) (* a c)))					;case 6
	  ((and (<= a b) (<  b 0) (<= 0 c) (<= c d)) (make-interval (* a d) (* b c)))					;case 7
	  ((and (<= a b) (<  b 0) (<  c 0) (<= 0 d)) (make-interval (* a d) (* a c)))					;case 8
	  ((and (<= a b) (<  b 0) (<= c d) (<  d 0)) (make-interval (* b d) (* a c)))	 				;case 9
	  (else (error "Interval input is vaild!" x y)))))

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (>= (upper-bound y) 0))
      (error "Interval across 0: " y)
      (mul-interval x
		    (make-interval (/ 1.0 lower-bound y)
				   (/ 1.0 upper-bound y)))))

(define x (make-interval -3 4))
(define y (make-interval -5 2))

(print-interval (mul-interval x y))
