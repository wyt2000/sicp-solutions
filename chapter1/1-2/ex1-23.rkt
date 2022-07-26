#lang sicp

(define (next n) 
  (if (= n 2)
      3
      (+ n 2)))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  )

(define (search-for-primes from to)

  (define (search-for-primes-iter)
    (if (= (remainder from 2) 1) (timed-prime-test from))
    (search-for-primes (+ from 1) to))

  (if (< from to)
    (search-for-primes-iter)))

(search-for-primes 1000 10000)
(search-for-primes 10000 100000)
(search-for-primes 100000 1000000)
;When n is big enough, the speed rate is close to 1:2.

