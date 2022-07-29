#lang sicp

(define (cont-frac n d k)
  (define (cont-frac-recursive i)
    (if (= i k)
        (/ (n i) (d i))
	(/ (n i) (+ (d i) (cont-frac-recursive (+ i 1))))))
  (cont-frac-recursive 1))

(define (get-phi k)
  (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k))

(define (get-phi-iter a b)
  (define (show-phi k)
    (display (get-phi k))
    (display "***")
    (display k)
    (newline)
    (get-phi-iter (+ k 1) b))
  (if (< a b)
      (show-phi a)))

(get-phi-iter 1 10) ;When k = 9, we get phi = 0.618
