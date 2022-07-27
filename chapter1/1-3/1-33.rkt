#lang sicp

(define (filtered-accumulate satisfy? combiner null-value term a next b)
  (define (filtered-accumulate-iter a result)
    (if (> a b)
        result
	(filtered-accumulate-iter (next a) (combiner (if (satisfy? a) (term a) null-value) result))))
  (filtered-accumulate-iter a null-value))

(define (prime? n)
  (define (test-divided-iter x)
    (cond ((> (* x x) n) #t)
	  ((= (remainder n x) 0) #f)
	  (else (test-divided-iter (+ x 1)))))
  (test-divided-iter 2))

(define (GCD a b)
  (if (= b 0)
      a
      (GCD b (remainder a b))))

(define (sum-primes a b)
  (define (id n) n)
  (define (inc n) (+ n 1))
  (filtered-accumulate prime? + 0 id a inc b)) 

(define (prod-less-coprime n)
  (define (id n) n)
  (define (inc n) (+ n 1))
  (define (coprime? x) (= (GCD x n) 1))
  (filtered-accumulate coprime? * 1 id 1 inc n)) 

(sum-primes 2 10)        ;17
(prod-less-coprime 10)   ;189
